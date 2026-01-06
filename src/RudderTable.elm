module RudderTable exposing
    ( Column, ColumnName(..), SortOrder(..)
    , Config, ConfigBuilder, buildConfig
    , Options, OptionsBuilder, buildOptions
    , FilterOptions, FilterOptionsType(..)
    , RefreshButtonOptions(..)
    , StorageOptions, StorageOptionsConfig, StorageKey
    , Customizations
    , CsvExportData
    , CsvExportConfig, CsvExportOptions
    , updateData, updateFilter, updateDataWithFilter
    , Model, Msg
    , view, update, init
    , OutMsg(..)
    , Effect(..), updateWithEffect
    , sortColumn
    , storageOptions, getFilterOptionValue, getRows, getSort
    , exportCsv
    -- the internal message type, we never want to expose it with Msg(..)
    -- for testing the updates
    )

{-| A table component, inspired from [elm-sortable-table], with the
functionalities taken from [DataTable].

It has a TEA approach, so it should be used with the [Nested TEA][nested-tea] architecture.

[elm-sortable-table]: https://github.com/billstclair/elm-sortable-table/
[DataTable]: https://datatables.net/
[nested-tea]: https://sporto.github.io/elm-patterns/architecture/nested-tea.html


# Constructors and configuration data types

@docs Column, ColumnName, SortOrder
@docs Config, ConfigBuilder, buildConfig
@docs Options, OptionsBuilder, buildOptions
@docs FilterOptions, FilterOptionsType
@docs RefreshButtonOptions
@docs StorageOptions, StorageOptionsConfig, StorageKey
@docs Customizations
@docs CsvExportData
@docs CsvExportConfig, CsvExportOptions


# State-changing functions

@docs updateData, updateFilter, updateDataWithFilter


# Common TEA

@docs Model, Msg
@docs view, update, init
@docs OutMsg


# Testing only

@docs Effect, updateWithEffect
@docs sortColumn
@docs storageOptions, getFilterOptionValue, getRows, getSort
@docs exportCsv

-}

import Csv.Encode
import File.Download
import Html exposing (Attribute, Html, button, div, i, input, span, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class, colspan, placeholder, rowspan, style, tabindex, type_, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode exposing (Value)
import Json.Encode as Encode
import List.Extra
import List.Nonempty as NonEmptyList
import Ordering exposing (Ordering)
import TableFilters exposing (FilterStringPredicate, SearchFilterState, applyString, getTextValue, substring)


{-| ATTENTION !

You will need to copy-paste this port in the application, because it cannot be defined in packages

port saveToLocalStorage : Value -> Cmd msg

-}
type alias StorageKey =
    String


type StorageValueType
    = StorageFilter


{-| Sorting in ascending or descending order
-}
type SortOrder
    = Asc
    | Desc


{-| Type marker for the column name that is displayed
-}
type ColumnName
    = ColumnName String


{-| A table column which can be rendered in multiple ways, by default in arbitrary HTML (with the same concrete message as in the encapsulating parent).
It can also be sorted, an enforces ordering on all columns. FIXME: it could be made optional.
-}
type alias Column row msg =
    { name : ColumnName
    , renderHtml : row -> Html msg
    , ordering : Ordering row
    }


{-| The options to configure the table at initialization, namely the table columns, and also including several initial defaults.
-}
type alias Config row msg =
    { columns : NonEmptyList.Nonempty (Column row msg)
    , sortBy : ColumnName
    , sortOrder : SortOrder
    , options : Options row msg
    }


{-| The options to have a refresh button in the table or not. Refresh should be handled in parent from the Nested TEA pattern,
see `OutMsg`
-}
type RefreshButtonOptions msg
    = NoRefreshButton
    | RefreshButtonOptions (List (Attribute (Msg msg)))


{-| The options to include a known filter in the table header, e.g. a search input filter, or to display no filter at all.
It should still be possible to filter the table data with an exposed filtering function.
-}
type FilterOptions row msg
    = NoFilter
    | FilterOptions (FilterOptionsType row msg)


{-| The filter types supported for display and/or filtering

  - `SearchInputFilter` provides functionality to save to localStorage, so use it along the storage options if needed.
  - `HtmlFilter` is arbitrary HTML in the table header

-}
type FilterOptionsType row msg
    = HtmlFilter { predicate : row -> Bool, html : Html msg, toMsg : msg -> Msg msg }
    | SearchInputFilter { predicate : FilterStringPredicate row, state : SearchFilterState }


{-| The options to support saving to localStorage
-}
type StorageOptions msg
    = NoStorage
    | StorageOptions (StorageOptionsConfig msg)


{-| The configuration of saving to local storage, under specific key, and with the port function,
which needs to be bound to the parent Elm app
-}
type alias StorageOptionsConfig msg =
    { key : String
    , saveToLocalStoragePort : Value -> Cmd msg
    }


{-| CSV export configuration
-}
type alias CsvExportConfig row parentMsg =
    { fileName : String, entryToStringList : row -> List String, btnAttributes : List (Attribute (Msg parentMsg)) }


{-| Options for CSV export support of a given table
-}
type CsvExportOptions row msg
    = CsvExportButton (CsvExportConfig row msg)
    | NoCsvExportButton


{-| Datatype that represents the result of exporting a table to CSV
-}
type alias CsvExportData =
    { fileName : String, csv : String }


{-| Table display customizations for adding custom HTML attributes, e.g. `class` to parts of the table.
The defaults should be fine, but customizations can be added and defined depending on each case.
-}
type alias Customizations row msg =
    { tableContainerAttrs : List (Attribute (Msg msg))
    , tableAttrs : List (Attribute (Msg msg))
    , optionsHeaderAttrs : List (Attribute (Msg msg))

    -- , caption : Maybe (HtmlDetails msg)
    , theadAttrs : List (Attribute (Msg msg))

    -- , tfoot : Maybe (HtmlDetails msg)
    , tbodyAttrs : List (Attribute (Msg msg))
    , rowAttrs : row -> List (Attribute (Msg msg))
    }


{-| The table features and customization that you may like to tweak.

Use the `buildOptions` builder to create one.

-}
type alias Options row msg =
    { customizations : Customizations row msg
    , refresh : RefreshButtonOptions msg
    , storage : StorageOptions msg
    , filter : FilterOptions row msg
    , csvExport : CsvExportOptions row msg
    }


{-| Init function from the config and initial data
-}
init : Config row msg -> List row -> Model row msg
init config data =
    Model
        { columns = config.columns
        , sortBy = config.sortBy
        , sortOrder = config.sortOrder
        , options = config.options
        , initialData = data
        , data = data
        }


{-| The internal model of the table, which should be kept as reference in parents, to subsequent updates
-}
type Model row msg
    = Model
        { columns : NonEmptyList.Nonempty (Column row msg)
        , sortBy : ColumnName
        , sortOrder : SortOrder
        , options : Options row msg
        , data : List row

        -- To keep references even when filtering
        , initialData : List row
        }


{-| The internal message which is handled internally and which can be used to execute messages from parent that were
passed in the configuration
-}
type Msg parentMsg
    = SortColumn ColumnName
    | RefreshMsg
    | FilterInputChanged String
    | UpdateFilterMsg SearchFilterState
    | ExportCsvMsg
    | ParentMsg parentMsg


{-| The public message to be exposed to parent components
-}
type OutMsg parentMsg
    = Refresh
    | OnHtml parentMsg


{-| A representation of concrete effects in the type system to allow testing
-}
type Effect parentMsg
    = SaveFilterInLocalStorage StorageKey SearchFilterState (Value -> Cmd parentMsg)
    | DownloadTableAsCsv CsvExportData
    | IgnoreExportCsvMsgNoConfig



{- BUILDERS -}


{-| The builder DSL for the config. It allows creating the custom configuration you want for you table :

  - with mandatory columns, and predicate to filter rows (since the logic of filtering must exist in our table)
  - all the options you want and initial values

All defaults are defined in `defaultConfig`.

To use it, the builder DSL heavily uses the flow syntax, take this example :

    cols =
        NonEmptyList.Nonempty
            (Column (ColumnName "id") (\{ id } -> text id) Nothing (Ordering.byField .id))
            [ Column (ColumnName "name") (\{ name } -> span [ class "name" ] [ text name ]) Nothing (Ordering.byField .name) ]

    filterByFields =
        filterByValues (\{ id, name } -> [ id, name ])

    config =
        buildConfig.newConfig cols filterByFields
            |> buildConfig.withSortBy (ColumnName "id")
            |> buildConfig.withSortOrder Desc
            |> buildConfig.withOptions
                (buildOptions.newOptions
                    |> buildOptions.withRefresh [ class "btn btn-primary" ]
                    |> buildOptions.withFilter (SearchInputFilter { predicate = Filters.byValues (\{ id, name } -> [ id, name ]), state = Filters.empty }) [ class "input-group" ]
                )

-}
type alias ConfigBuilder row msg =
    { newConfig : NonEmptyList.Nonempty (Column row msg) -> Config row msg
    , withSortBy : ColumnName -> Config row msg -> Config row msg
    , withSortOrder : SortOrder -> Config row msg -> Config row msg
    , withOptions : Options row msg -> Config row msg -> Config row msg
    }


{-| The builder DSL for table options
-}
type alias OptionsBuilder row msg =
    { newOptions : Options row msg
    , withCustomizations : Customizations row msg -> Options row msg -> Options row msg
    , withRefresh : List (Attribute (Msg msg)) -> Options row msg -> Options row msg
    , withStorage : StorageOptionsConfig msg -> Options row msg -> Options row msg
    , withFilter : FilterOptionsType row msg -> Options row msg -> Options row msg
    , withCsvExport : CsvExportConfig row msg -> Options row msg -> Options row msg
    }


{-| Builder to obtain a table configuration
-}
buildConfig : ConfigBuilder row msg
buildConfig =
    { newConfig = defaultConfig
    , withSortBy = \value -> \state -> { state | sortBy = value }
    , withSortOrder = \value -> \state -> { state | sortOrder = value }
    , withOptions = \value -> \state -> { state | options = value }
    }


{-| Builder of the options within the `Config`
-}
buildOptions : OptionsBuilder row msg
buildOptions =
    { newOptions = defaultOptions
    , withCustomizations = \opt -> \state -> { state | customizations = opt }
    , withRefresh = \opt -> \state -> { state | refresh = RefreshButtonOptions opt }
    , withStorage = \opt -> \state -> { state | storage = StorageOptions opt }
    , withFilter = \opt -> \state -> { state | filter = FilterOptions opt }
    , withCsvExport = \opt -> \state -> { state | csvExport = CsvExportButton opt }
    }



{- DEFAULTS -}


defaultConfig : NonEmptyList.Nonempty (Column row msg) -> Config row msg
defaultConfig columns =
    { columns = columns
    , sortBy = NonEmptyList.head columns |> .name
    , sortOrder = Asc
    , options = defaultOptions
    }


{-| Disable all features and apply default customizations
-}
defaultOptions : Options row msg
defaultOptions =
    { customizations = defaultCustomizations
    , refresh = NoRefreshButton
    , storage = NoStorage
    , filter = NoFilter
    , csvExport = NoCsvExportButton
    }


defaultCustomizations : Customizations row msg
defaultCustomizations =
    { tableContainerAttrs = []
    , tableAttrs = []
    , optionsHeaderAttrs = []

    -- , caption = Nothing
    , theadAttrs = []

    -- , tfoot = Nothing
    , tbodyAttrs = []
    , rowAttrs = \_ -> []
    }



{- GETTERS -}


{-| Getter for the storage options on the model
-}
storageOptions : Model row msg -> Maybe (StorageOptionsConfig msg)
storageOptions (Model model) =
    case model.options.storage of
        StorageOptions cfg ->
            Just cfg

        NoStorage ->
            Nothing


{-| Getter for the table sorting state
-}
getSort : Model row msg -> ( ColumnName, SortOrder )
getSort (Model { sortBy, sortOrder }) =
    ( sortBy, sortOrder )


{-| Getter for the table data
-}
getRows : Model row msg -> List row
getRows (Model { data }) =
    data



{- UPDATE -}


{-| Initialize from a set of data, useful in "refresh" scenario or for initialization
-}
updateData : List row -> Model row msg -> Model row msg
updateData rows (Model model) =
    Model { model | initialData = rows, data = rows }
        |> applySort


{-| The update function to call in the parent :

    RudderTableMsg tableMsg ->
        let
            ( updatedModel, cmd, outMsg ) =
                update tableMsg model.rudderTableModel

            ...do something with public message, e.g. refresh...
        in
        ( { model | rudderTableModel = updatedModel }, cmd )

-}
update : Msg msg -> Model row msg -> ( Model row msg, Cmd msg, Maybe (OutMsg msg) )
update msg model =
    let
        ( updatedModel, effect, outMsg ) =
            updateWithEffect msg model
    in
    ( updatedModel, interpret effect, outMsg )


interpret : List (Effect msg) -> Cmd msg
interpret =
    List.map
        (\e ->
            case e of
                SaveFilterInLocalStorage key filter cb ->
                    cb (encodeStorageEffect key filter)

                DownloadTableAsCsv { fileName, csv } ->
                    File.Download.string fileName "text/csv" csv

                IgnoreExportCsvMsgNoConfig ->
                    Cmd.none
        )
        >> Cmd.batch


{-| Concrete implementation of the update loop, using the [effect] pattern.
Would be useful for testing, not for public API.

[effect]: https://sporto.github.io/elm-patterns/architecture/effects.html

-}
updateWithEffect : Msg parentMsg -> Model row parentMsg -> ( Model row parentMsg, List (Effect parentMsg), Maybe (OutMsg parentMsg) )
updateWithEffect msg (Model model) =
    case msg of
        SortColumn columnName ->
            -- FIXME: Sorting effect : save in local storage
            ( Model model |> sortByColumnName columnName, [], Nothing )

        RefreshMsg ->
            -- FIXME: disable button
            ( Model model, [], Just Refresh )

        FilterInputChanged s ->
            updateWithEffect (UpdateFilterMsg (substring s)) (Model model)

        UpdateFilterMsg s ->
            let
                ( newModel, effects ) =
                    updateOnFilterInput s (Model model)
            in
            ( newModel, effects, Nothing )

        ExportCsvMsg ->
            case model.options.csvExport of
                CsvExportButton csvExportConfig ->
                    ( Model model, [ DownloadTableAsCsv (tableToCsv (Model model) csvExportConfig) ], Nothing )

                NoCsvExportButton ->
                    ( Model model, [ IgnoreExportCsvMsgNoConfig ], Nothing )

        ParentMsg m ->
            -- FIXME: do we know the effects ?
            ( Model model, [], Just (OnHtml m) )


updateOnFilterInput : SearchFilterState -> Model row msg -> ( Model row msg, List (Effect msg) )
updateOnFilterInput newFilterState (Model model) =
    let
        (Model modelWithFilter) =
            Model model
                |> updateFilterOptions (setSearchInputFilterState newFilterState)

        predicate =
            getFilterOptionPredicate modelWithFilter.options.filter

        newModel =
            updateDataWithFilter predicate (Model modelWithFilter)
    in
    case model.options.storage of
        StorageOptions options ->
            ( newModel, [ SaveFilterInLocalStorage options.key newFilterState options.saveToLocalStoragePort ] )

        NoStorage ->
            ( newModel, [] )


{-| Get the text value in case the table has an input filter
-}
getFilterOptionValue : Model row msg -> String
getFilterOptionValue (Model { options }) =
    case options.filter of
        FilterOptions (SearchInputFilter { state }) ->
            getTextValue state

        _ ->
            ""


getFilterOptionPredicate : FilterOptions row msg -> (row -> Bool)
getFilterOptionPredicate options =
    case options of
        NoFilter ->
            \_ -> True

        FilterOptions (HtmlFilter { predicate }) ->
            predicate

        FilterOptions (SearchInputFilter { state, predicate }) ->
            applyString state predicate


{-| Filter the data within the table from the outside, useful when the table filters are not configured because the
filters are "outside" the table
-}
updateDataWithFilter : (row -> Bool) -> Model row msg -> Model row msg
updateDataWithFilter pred (Model model) =
    Model { model | data = List.filter pred model.initialData }


updateFilterOptions : (FilterOptions row msg -> FilterOptions row msg) -> Model row msg -> Model row msg
updateFilterOptions f (Model ({ options } as model)) =
    let
        filter =
            options.filter
    in
    Model { model | options = { options | filter = f filter } }


setSearchInputFilterState : SearchFilterState -> FilterOptions row msg -> FilterOptions row msg
setSearchInputFilterState state option =
    case option of
        FilterOptions (SearchInputFilter o) ->
            FilterOptions (SearchInputFilter { o | state = state })

        _ ->
            option



{- MESSAGE CONSTRUCTORS -}


{-| Internal Msg to produce message to sort column, for testing
-}
sortColumn : ColumnName -> Msg parentMsg
sortColumn =
    SortColumn


{-| Internal Msg to produce message to update the search input filter, for testing
-}
updateFilter : SearchFilterState -> Msg msg
updateFilter =
    UpdateFilterMsg


{-| Internal Msg that produces the CSV export event; used in tests
-}
exportCsv : Msg msg
exportCsv =
    ExportCsvMsg



{- INTERNAL HELPERS -}


sortByColumnName : ColumnName -> Model row msg -> Model row msg
sortByColumnName columnName (Model model) =
    model
        |> setSortOrToggle columnName
        |> Model
        |> applySort


applySort : Model row msg -> Model row msg
applySort (Model model) =
    let
        maybeColumnAccessor =
            model.columns
                |> NonEmptyList.toList
                |> List.Extra.find (\{ name } -> name == model.sortBy)

        maybeSort =
            maybeColumnAccessor
                |> Maybe.map
                    (\{ ordering } ->
                        if model.sortOrder == Asc then
                            ordering

                        else
                            Ordering.reverse ordering
                    )
    in
    case maybeSort of
        Nothing ->
            Model model

        Just sort ->
            Model { model | data = List.sortWith sort model.data }


type alias Sort a =
    { a
        | sortBy : ColumnName
        , sortOrder : SortOrder
    }


setSortOrToggle : ColumnName -> Sort a -> Sort a
setSortOrToggle columnName sortState =
    if columnName == sortState.sortBy then
        let
            toggle =
                case sortState.sortOrder of
                    Asc ->
                        Desc

                    Desc ->
                        Asc
        in
        { sortState | sortOrder = toggle }

    else
        { sortState | sortBy = columnName, sortOrder = Asc }



{- STORAGE AND ENCODING -}


storageValueTypeText : StorageValueType -> String
storageValueTypeText valueType =
    case valueType of
        StorageFilter ->
            "storageFilter"



{- CSV EXPORT -}


{-| Table to CSV export function
-}
tableToCsv : Model row msg -> CsvExportConfig row msg -> CsvExportData
tableToCsv (Model model) { fileName, entryToStringList } =
    let
        -- first row contains column names
        columns : List String
        columns =
            model.columns
                |> NonEmptyList.toList
                |> List.map (\c -> c.name)
                |> List.map (\(ColumnName c) -> c)

        data : List (List String)
        data =
            model.data |> List.map entryToStringList
    in
    data
        |> Csv.Encode.encode
            { encoder =
                Csv.Encode.withFieldNames
                    (\entry -> List.map2 Tuple.pair columns entry)
            , fieldSeparator = ','
            }
        |> CsvExportData fileName



--FIXME: we need to enforce in the Options, that if Search && Storage are configured, how to store any kind of "filter model"


encodeFilter : SearchFilterState -> Value
encodeFilter filter =
    Encode.string (getTextValue filter)


encodeStorageValueType : StorageValueType -> Value
encodeStorageValueType valueType =
    Encode.string (storageValueTypeText valueType)


encodeStorageEffect : StorageKey -> SearchFilterState -> Value
encodeStorageEffect key filter =
    Encode.object
        [ ( "key", Encode.string key )
        , ( "type", encodeStorageValueType StorageFilter )
        , ( "value", encodeFilter filter )
        ]



{- VIEW -}


{-| The main view function for the table
-}
view : Model row msg -> Html (Msg msg)
view (Model ({ columns, data, options } as model)) =
    let
        theadAttrs =
            options.customizations.theadAttrs

        tbodyAttrs =
            options.customizations.tbodyAttrs

        tableContainerAttrs =
            options.customizations.tableContainerAttrs

        tableView =
            table options.customizations.tableAttrs
                [ thead theadAttrs [ tableHeader columns model ]
                , tbody tbodyAttrs (tableBody columns data)
                ]

        content =
            case viewHeaderOptions model.options of
                [] ->
                    [ tableView ]

                elems ->
                    [ div options.customizations.optionsHeaderAttrs elems
                    , tableView
                    ]
    in
    div tableContainerAttrs content


viewHeaderOptions : Options row msg -> List (Html (Msg msg))
viewHeaderOptions options =
    let
        buttons =
            viewHeaderButtons options
    in
    case options.filter of
        NoFilter ->
            buttons

        FilterOptions (SearchInputFilter { state }) ->
            div []
                [ input [ class "form-control", type_ "text", placeholder "Filter...", onInput FilterInputChanged, value (getTextValue state) ] []
                ]
                :: buttons

        FilterOptions (HtmlFilter { html, toMsg }) ->
            Html.map toMsg html :: buttons


viewHeaderButtons : Options row msg -> List (Html (Msg msg))
viewHeaderButtons { refresh, csvExport } =
    case ( refresh, csvExport ) of
        ( NoRefreshButton, NoCsvExportButton ) ->
            []

        ( NoRefreshButton, csvOptions ) ->
            [ div [ style "margin-left" "auto", style "margin-right" "0" ]
                [ div [] [ viewCsvExportButton csvOptions ]
                ]
            ]

        ( refreshOptions, NoCsvExportButton ) ->
            [ div [ style "margin-left" "auto", style "margin-right" "0" ] [ viewRefreshButton refreshOptions ] ]

        ( refreshOptions, csvOptions ) ->
            [ div [ style "margin-left" "auto", style "display" "flex" ]
                [ div [ style "margin-right" ".5rem" ]
                    [ div [] [ viewCsvExportButton csvOptions ] ]
                , viewRefreshButton refreshOptions
                ]
            ]


viewRefreshButton : RefreshButtonOptions msg -> Html (Msg msg)
viewRefreshButton option =
    case option of
        NoRefreshButton ->
            text ""

        RefreshButtonOptions attrs ->
            button (onClick RefreshMsg :: attrs) [ i [ class "fa fa-refresh" ] [] ]


viewCsvExportButton : CsvExportOptions row msg -> Html (Msg msg)
viewCsvExportButton option =
    case option of
        NoCsvExportButton ->
            text ""

        CsvExportButton { btnAttributes } ->
            button
                ([ class "btn btn-primary"
                 , tabindex 0
                 , type_ "button"
                 , onClick ExportCsvMsg
                 ]
                    ++ btnAttributes
                )
                [ span []
                    [ text "Export"
                    , text " "
                    , i [ class "fa fa-table" ] []
                    ]
                ]


tableHeader : NonEmptyList.Nonempty (Column row msg) -> Sort a -> Html (Msg msg)
tableHeader columns sort =
    tr [ class "head" ]
        (columns
            |> NonEmptyList.toList
            |> List.map (\column -> ( column.name, thClass sort column.name ))
            |> List.map
                (\( ColumnName name, thClassValue ) ->
                    th
                        [ class thClassValue
                        , rowspan 1
                        , colspan 1
                        , onClick (SortColumn (ColumnName name))
                        ]
                        [ text name ]
                )
        )


thClass : Sort a -> ColumnName -> String
thClass { sortBy, sortOrder } columnName =
    if sortBy == columnName then
        case sortOrder of
            Asc ->
                "sorting_asc"

            Desc ->
                "sorting_desc"

    else
        "sorting"


tableBody : NonEmptyList.Nonempty (Column row msg) -> List row -> List (Html (Msg msg))
tableBody columns data =
    let
        rowTable n =
            tr []
                (columns
                    |> NonEmptyList.toList
                    |> List.map (\{ renderHtml } -> Html.map ParentMsg (n |> renderHtml))
                    |> List.map (\s -> td [] [ s ])
                )
    in
    if List.length data > 0 then
        List.map rowTable data

    else
        [ tr [] [ td [ class "empty", colspan 5 ] [ i [ class "fa fa-exclamation-triangle" ] [], text "Nothing matches your filters." ] ] ]
