module RudderDataTableTest exposing (..)

import Expect
import Filters exposing (SearchFilterState, byValues, getTextValue)
import Fuzz exposing (..)
import Html exposing (div)
import List.Nonempty as NonEmptyList exposing (Nonempty)
import Ordering
import RudderDataTable exposing (..)
import Test exposing (..)


type alias Model =
    RudderDataTable.Model String ()


filterFuzz : Fuzzer SearchFilterState
filterFuzz =
    oneOf [ constant Filters.empty, substringFilterFuzz ]


substringFilterFuzz : Fuzzer SearchFilterState
substringFilterFuzz =
    -- Filter without space, because filter is trimmed
    map Filters.substring (map String.fromList (list (intRange 33 126 |> map Char.fromCode)))


columnNameFuzz : Fuzzer ColumnName
columnNameFuzz =
    map ColumnName string


columnFuzz : Fuzzer (Column String ())
columnFuzz =
    constant Column
        |> andMap columnNameFuzz
        |> andMap (constant (\_ -> Html.text ""))
        |> andMap (constant Ordering.natural)


exportCsvColumnFuzz : Fuzzer (Column String ())
exportCsvColumnFuzz =
    constant Column
        |> andMap columnNameFuzz
        |> andMap (constant (\_ -> Html.text ""))
        |> andMap (constant (\_ _ -> LT))


nonEmptyListFuzzer : Fuzzer a -> Fuzzer (NonEmptyList.Nonempty a)
nonEmptyListFuzzer aFuzz =
    map2 NonEmptyList.Nonempty aFuzz (list aFuzz)


sortOrderFuzz : Fuzzer SortOrder
sortOrderFuzz =
    oneOfValues [ Asc, Desc ]


optionsFuzz : SearchFilterState -> Fuzzer (Options String ())
optionsFuzz filter =
    constant
        (buildOptions.newOptions
            |> buildOptions.withStorage { key = "", saveToLocalStoragePort = \_ -> Cmd.none }
            |> buildOptions.withFilter (SearchInputFilter { predicate = byValues (\d -> [ d ]), state = filter })
        )


csvOptionsFuzz : Fuzzer (Options String ())
csvOptionsFuzz =
    constant
        (buildOptions.newOptions
            |> buildOptions.withCsvExport { fileName = "myFile.txt", entryToStringList = \_ -> [] }
        )


filterModelFuzz : SearchFilterState -> Fuzzer Model
filterModelFuzz filter =
    let
        config =
            constant Config
                |> andMap (nonEmptyListFuzzer columnFuzz)
                |> andMap columnNameFuzz
                |> andMap sortOrderFuzz
                |> andMap (optionsFuzz filter)
    in
    map2 RudderDataTable.init config dataFuzzer


sortModelFuzz : ColumnName -> SortOrder -> Fuzzer Model
sortModelFuzz sortBy sortOrder =
    let
        config =
            constant Config
                |> andMap (constant (NonEmptyList.singleton { name = sortBy, renderHtml = \_ -> Html.text "", ordering = Ordering.natural }))
                |> andMap (constant sortBy)
                |> andMap (constant sortOrder)
                |> andMap (optionsFuzz Filters.empty)
    in
    map2 RudderDataTable.init config dataFuzzer


exportCsvModelFuzz : Fuzzer Model
exportCsvModelFuzz =
    let
        config =
            constant Config
                |> andMap (nonEmptyListFuzzer exportCsvColumnFuzz)
                |> andMap columnNameFuzz
                |> andMap sortOrderFuzz
                |> andMap csvOptionsFuzz
    in
    map2 RudderDataTable.init config dataFuzzer


applyFilterFuzz : Fuzzer ((String -> Bool) -> String -> Bool)
applyFilterFuzz =
    constant identity


dataFuzzer : Fuzzer (List String)
dataFuzzer =
    list rowFuzzer


rowFuzzer : Fuzzer String
rowFuzzer =
    string


suite : Test
suite =
    let
        model ( a, _, _ ) =
            a

        effect ( _, a, _ ) =
            a

        stubFilter =
            Filters.substring "test"

        stubColumnName =
            ColumnName "column"
    in
    describe "RudderDataTable"
        [ -- filters
          fuzz (filterModelFuzz Filters.empty) "save new filter in local storage" <|
            \m ->
                let
                    options =
                        storageOptions m

                    expectation =
                        options
                            |> Maybe.map (\{ key, saveToLocalStoragePort } -> Expect.equal [ SaveFilterInLocalStorage key stubFilter saveToLocalStoragePort ])
                            |> Maybe.withDefault (\_ -> Expect.fail "initial model was not configured to save to local storage, please fix the stub fuzz")
                in
                updateWithEffect (updateFilter stubFilter) m |> effect |> expectation
        , fuzz (filterModelFuzz Filters.empty) "save new filter in model" <|
            \m -> updateWithEffect (updateFilter stubFilter) m |> model |> getFilterOptionValue |> Expect.equal "test"
        , fuzz3 (filterModelFuzz Filters.empty) dataFuzzer substringFilterFuzz "apply substring filter to data" <|
            \m d f ->
                let
                    dataLen =
                        List.length d

                    filterText =
                        getTextValue f

                    -- each data contains the filter string so filtered data is the same
                    data =
                        List.map (String.append filterText) d
                in
                m
                    |> updateData data
                    |> updateWithEffect (updateFilter f)
                    |> model
                    |> getRows
                    |> List.length
                    |> Expect.equal dataLen

        -- sort
        , fuzz (sortModelFuzz stubColumnName Asc) "toggle already sorted column with Asc" <|
            \m -> updateWithEffect (sortColumn stubColumnName) m |> model |> getSort |> Expect.equal ( stubColumnName, Desc )
        , fuzz (sortModelFuzz stubColumnName Desc) "toggle already sorted column with Desc" <|
            \m -> updateWithEffect (sortColumn stubColumnName) m |> model |> getSort |> Expect.equal ( stubColumnName, Asc )
        , fuzz2 (sortModelFuzz stubColumnName Asc) columnNameFuzz "apply column with Asc order" <|
            \m c -> updateWithEffect (sortColumn c) m |> model |> getSort |> Expect.equal ( c, Asc )
        , fuzz2 (sortModelFuzz stubColumnName Asc) dataFuzzer "apply sort to data" <|
            \m data ->
                m
                    |> updateData data
                    |> updateWithEffect (sortColumn stubColumnName)
                    |> model
                    |> getRows
                    |> Expect.equalLists (List.reverse (List.sort data))

        -- csv export
        , test "test csv export on empty table that does not define a csv export configuration" <|
            \_ ->
                init (buildConfig.newConfig (NonEmptyList.singleton (Column (ColumnName "name") (\_ -> div [] []) (\_ _ -> LT)))) []
                    |> updateWithEffect (exportCsv "myFile")
                    |> effect
                    |> Expect.equal [ DownloadTableAsCsv "myFile" CsvExportConfigUndefined ]
        , test "test csv export on empty table that defines a csv export configuration" <|
            \_ ->
                let
                    options =
                        buildOptions.newOptions
                            |> buildOptions.withCsvExport { fileName = "otherFile", entryToStringList = \row -> row }
                in
                init
                    (buildConfig.newConfig (NonEmptyList.singleton (Column (ColumnName "name") (\_ -> div [] []) (\_ _ -> LT)))
                        |> buildConfig.withOptions options
                    )
                    []
                    |> updateWithEffect (exportCsv "otherFile")
                    |> effect
                    |> Expect.equal [ DownloadTableAsCsv "otherFile" (CsvExportSuccess "") ]
        , fuzz (nonEmptyListFuzzer columnFuzz) "test csv export on empty tables that do not define a csv export configuration" <|
            \c ->
                init (buildConfig.newConfig c) []
                    |> updateWithEffect (exportCsv "yetAnotherFile")
                    |> effect
                    |> Expect.equal [ DownloadTableAsCsv "yetAnotherFile" CsvExportConfigUndefined ]
        , test "test csv export on non-empty table that defines a csv export configuration" <|
            \_ ->
                let
                    options =
                        buildOptions.newOptions
                            |> buildOptions.withCsvExport { fileName = "otherFile", entryToStringList = \{ a, b } -> [ a, String.fromInt b ] }

                    config =
                        buildConfig.newConfig
                            (NonEmptyList.Nonempty
                                (Column (ColumnName "name") (\_ -> div [] []) (\_ _ -> LT))
                                [ Column (ColumnName "age") (\_ -> div [] []) (\_ _ -> LT) ]
                            )
                            |> buildConfig.withOptions options

                    data =
                        [ { a = "Alice", b = 45 }
                        , { a = "Bob", b = 37 }
                        ]

                    tab =
                        RudderDataTable.init config data
                in
                tab
                    |> updateWithEffect (exportCsv "fileName")
                    |> effect
                    |> Expect.equal [ DownloadTableAsCsv "fileName" (CsvExportSuccess "name,age\u{000D}\nAlice,45\u{000D}\nBob,37") ]
        , test "test csv export on non-empty table that defines a csv export configuration and that contains fields with special characters" <|
            \_ ->
                let
                    options =
                        buildOptions.newOptions
                            |> buildOptions.withCsvExport { fileName = "otherFile", entryToStringList = \{ a, b } -> [ a, String.fromInt b ] }

                    config =
                        buildConfig.newConfig
                            (NonEmptyList.Nonempty
                                (Column (ColumnName "name") (\_ -> div [] []) (\_ _ -> LT))
                                [ Column (ColumnName "age") (\_ -> div [] []) (\_ _ -> LT) ]
                            )
                            |> buildConfig.withOptions options

                    data =
                        [ { a = "\"Al\"ice\"", b = 45 }
                        , { a = "Bo,b", b = 37 }
                        , { a = "\nEve", b = 28 }
                        ]

                    tab =
                        RudderDataTable.init config data
                in
                tab
                    |> updateWithEffect (exportCsv "fileName")
                    |> effect
                    |> Expect.equal [ DownloadTableAsCsv "fileName" (CsvExportSuccess "name,age\u{000D}\n\"\"\"Al\"\"ice\"\"\",45\u{000D}\n\"Bo,b\",37\u{000D}\n\"\nEve\",28") ]
        ]
