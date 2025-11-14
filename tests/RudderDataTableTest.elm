module RudderDataTableTest exposing (suite)

import Expect
import Filters exposing (SearchFilterState(..), byValues, getTextValue)
import Fuzz exposing (..)
import Html
import List.Nonempty as NonEmptyList
import Ordering
import RudderDataTable exposing (..)
import Test exposing (..)


type alias Model =
    RudderDataTable.Model String ()


filterFuzz : Fuzzer SearchFilterState
filterFuzz =
    oneOf [ constant EmptyFilter, substringFilterFuzz ]


substringFilterFuzz : Fuzzer SearchFilterState
substringFilterFuzz =
    -- Filter without space, because filter is trimmed
    map Substring (nonEmptyListFuzzer (intRange 33 126 |> map Char.fromCode))


columnNameFuzz : Fuzzer ColumnName
columnNameFuzz =
    map ColumnName string


columnFuzz : Fuzzer (Column String ())
columnFuzz =
    constant Column
        |> andMap columnNameFuzz
        |> andMap (constant (\_ -> Html.text ""))
        |> andMap (constant Nothing)
        |> andMap (constant Ordering.natural)


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
                |> andMap (constant (NonEmptyList.singleton { name = sortBy, renderHtml = \_ -> Html.text "", renderCsv = Nothing, ordering = Ordering.natural }))
                |> andMap (constant sortBy)
                |> andMap (constant sortOrder)
                |> andMap (optionsFuzz EmptyFilter)
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


suite =
    let
        model ( a, _, _ ) =
            a

        effect ( _, a, _ ) =
            a

        stubFilter =
            Substring (NonEmptyList.Nonempty 't' (String.toList "est"))

        stubColumnName =
            ColumnName "column"
    in
    describe "RudderDataTable"
        [ -- filters
          fuzz (filterModelFuzz EmptyFilter) "save new filter in local storage" <|
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
        , fuzz (filterModelFuzz EmptyFilter) "save new filter in model" <|
            \m -> updateWithEffect (updateFilter stubFilter) m |> (model >> getFilterOptionValue) |> Expect.equal "test"
        , fuzz3 (filterModelFuzz EmptyFilter) dataFuzzer substringFilterFuzz "apply substring filter to data" <|
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
                    |> (model >> size)
                    |> Expect.equal dataLen

        -- sort
        , fuzz (sortModelFuzz stubColumnName Asc) "toggle already sorted column with Asc" <|
            \m -> updateWithEffect (sortColumn stubColumnName) m |> (model >> getSort) |> Expect.equal ( stubColumnName, Desc )
        , fuzz (sortModelFuzz stubColumnName Desc) "toggle already sorted column with Desc" <|
            \m -> updateWithEffect (sortColumn stubColumnName) m |> (model >> getSort) |> Expect.equal ( stubColumnName, Asc )
        , fuzz2 (sortModelFuzz stubColumnName Asc) columnNameFuzz "apply column with Asc order" <|
            \m c -> updateWithEffect (sortColumn c) m |> (model >> getSort) |> Expect.equal ( c, Asc )
        , fuzz2 (sortModelFuzz stubColumnName Asc) dataFuzzer "apply sort to data" <|
            \m data ->
                m
                    |> updateData data
                    |> updateWithEffect (sortColumn stubColumnName)
                    |> (model >> getRows)
                    |> Expect.equalLists (List.reverse (List.sort data))
        ]
