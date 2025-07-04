module RudderDataTable exposing
    ( ApplyFilterPredicate
    , Column
    , ColumnName(..)
    , Config
    , ConfigBuilder
    , Customizations
    , Effect(..)
    , Filter(..)
    , FilterOptions
    , Model
      -- the internal message type, we never want to expose it with Msg(..)
    , Msg
    , Options
    , OptionsBuilder
    , OutMsg(..)
    , RefreshButtonOptions
    , SortOrder(..)
    , StorageKey
    , StorageOptions
    , StorageOptionsConfig
    , buildConfig
    , buildOptions
    , defaultCustomizations
    , defaultOptions
      -- for testing the updates
    , filterByValues
    , getFilterTextValue
    , getFilterValue
    , getRows
    , getSort
    , init
    , size
    , sortColumn
    , storageOptions
    , update
    , updateData
    , updateFilter
    , updateSubstringFilter
    , updateWithEffect
    , view
    )

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Json.Decode exposing (Value)
import Json.Encode as Encode
import List.Extra
import List.Nonempty as NonEmptyList
import Ordering exposing (Ordering)


{-| ATTENTION !

You will need to copy-paste this port in the application, because it cannot be defined in packages

port saveToLocalStorage : Value -> Cmd msg

-}
type alias StorageKey =
    String


type StorageValueType
    = StorageFilter


type SortOrder
    = Asc
    | Desc


type ColumnName
    = ColumnName String


{-| A table column which can be rendered in multiple ways:
by default in arbitrary HTML (with the same concrete message as in the encapsulating parent)
-}
type alias Column row msg =
    { name : ColumnName, renderHtml : row -> Html msg, renderCsv : Maybe (row -> List String), ordering : Ordering row }


type alias Config row msg =
    { columns : NonEmptyList.Nonempty (Column row msg)
    , sortBy : ColumnName
    , sortOrder : SortOrder
    , filter : Filter
    , applyFilter : ApplyFilterPredicate row
    , options : Options row msg
    }


{-| The filter types. It has an option to display it in the table header, but is always available in the table model.
See <https://datatables.net/reference/type/DataTables.SearchOptions> for other possible filters to implement.

The filter can be empty (e.g. when the input is empty, which can be the default), so invariants should enforce the
EmptyFilter case

-}
type Filter
    = Substring (NonEmptyList.Nonempty Char)
    | EmptyFilter


type RefreshButtonOptions msg
    = NoRefreshButton
    | RefreshButtonOptions (List (Attribute (Msg msg)))


type FilterOptions msg
    = NoFilter
    | FilterHeaderOptions (List (Attribute (Msg msg)))


type alias StorageOptionsConfig msg =
    { key : String, saveToLocalStoragePort : Value -> Cmd msg }


type StorageOptions msg
    = StorageOptions (StorageOptionsConfig msg)
    | NoStorage


type alias Customizations row msg =
    { tableContainerAttrs : List (Attribute (Msg msg))

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
    , filter : FilterOptions msg
    }


init : Config row msg -> List row -> Model row msg
init config data =
    Model
        { columns = config.columns
        , sortBy = config.sortBy
        , sortOrder = config.sortOrder
        , filter = config.filter
        , applyFilter = config.applyFilter
        , options = config.options
        , initialData = data
        , data = data
        }


type alias ApplyFilterPredicate row =
    (String -> Bool) -> row -> Bool


type Model row msg
    = Model
        { columns : NonEmptyList.Nonempty (Column row msg)
        , sortBy : ColumnName
        , sortOrder : SortOrder
        , filter : Filter
        , applyFilter : ApplyFilterPredicate row
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
    | FilterInputChanged String
    | RefreshMsg
    | UpdateFilterMsg Filter
    | ParentMsg parentMsg


{-| The public message to be exposed to parent components
-}
type OutMsg parentMsg
    = Refresh
    | OnHtml parentMsg


{-| A representation of concrete effects in the type system to allow testing
-}
type Effect parentMsg
    = SaveFilterInLocalStorage StorageKey Filter (Value -> Cmd parentMsg)



-- Builders


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
            |> buildConfig.withFilter (Substring (NonEmptyList.Nonempty 'i' [ 'd', '0' ]))
            |> buildConfig.withOptions
                (buildOptions.newOptions
                    |> buildOptions.withRefresh [ class "btn btn-primary" ]
                    |> buildOptions.withFilter [ class "input-group" ]
                )

-}
type alias ConfigBuilder row msg =
    { newConfig : NonEmptyList.Nonempty (Column row msg) -> ApplyFilterPredicate row -> Config row msg
    , withSortBy : ColumnName -> Config row msg -> Config row msg
    , withSortOrder : SortOrder -> Config row msg -> Config row msg
    , withFilter : Filter -> Config row msg -> Config row msg
    , withOptions : Options row msg -> Config row msg -> Config row msg
    }


type alias OptionsBuilder row msg =
    { newOptions : Options row msg
    , withRefresh : List (Attribute (Msg msg)) -> Options row msg -> Options row msg
    , withStorage : StorageOptionsConfig msg -> Options row msg -> Options row msg
    , withFilter : List (Attribute (Msg msg)) -> Options row msg -> Options row msg
    }


buildConfig : ConfigBuilder row msg
buildConfig =
    { newConfig = defaultConfig
    , withSortBy = \value -> \state -> { state | sortBy = value }
    , withSortOrder = \value -> \state -> { state | sortOrder = value }
    , withFilter = \value -> \state -> { state | filter = value }
    , withOptions = \value -> \state -> { state | options = value }
    }


buildOptions : OptionsBuilder row msg
buildOptions =
    { newOptions = defaultOptions
    , withRefresh = \opt -> \state -> { state | refresh = RefreshButtonOptions opt }
    , withStorage = \opt -> \state -> { state | storage = StorageOptions opt }
    , withFilter = \opt -> \state -> { state | filter = FilterHeaderOptions opt }
    }



-- defaults


defaultConfig : NonEmptyList.Nonempty (Column row msg) -> ApplyFilterPredicate row -> Config row msg
defaultConfig columns applyFilter =
    { columns = columns
    , applyFilter = applyFilter
    , sortBy = NonEmptyList.head columns |> .name
    , sortOrder = Asc
    , filter = EmptyFilter
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
    }


defaultCustomizations : Customizations row msg
defaultCustomizations =
    { tableContainerAttrs = []

    -- , caption = Nothing
    , theadAttrs = []

    -- , tfoot = Nothing
    , tbodyAttrs = []
    , rowAttrs = \_ -> []
    }



-- getters


storageOptions : Model row msg -> Maybe (StorageOptionsConfig msg)
storageOptions (Model model) =
    case model.options.storage of
        StorageOptions cfg ->
            Just cfg

        NoStorage ->
            Nothing


getFilterValue : Model row msg -> String
getFilterValue (Model { filter }) =
    getFilterTextValue filter


getSort : Model row msg -> ( ColumnName, SortOrder )
getSort (Model { sortBy, sortOrder }) =
    ( sortBy, sortOrder )


getRows : Model row msg -> List row
getRows (Model { data }) =
    data


size : Model row msg -> Int
size (Model { data }) =
    List.length data


getFilterTextValue : Filter -> String
getFilterTextValue filter =
    case filter of
        Substring filterString ->
            String.fromList (NonEmptyList.toList filterString)

        EmptyFilter ->
            ""



-- update


updateData : List row -> Model row msg -> Model row msg
updateData rows (Model model) =
    Model { model | initialData = rows, data = rows }
        |> applySort


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
                SaveFilterInLocalStorage _ _ cb ->
                    cb (encodeStorageEffect e)
        )
        >> Cmd.batch


updateWithEffect : Msg parentMsg -> Model row parentMsg -> ( Model row parentMsg, List (Effect parentMsg), Maybe (OutMsg parentMsg) )
updateWithEffect msg (Model model) =
    case msg of
        SortColumn columnName ->
            -- TODO: Sorting effect : save in local storage
            ( Model model |> sortByColumnName columnName, [], Nothing )

        FilterInputChanged filter ->
            --TODO: this does not work, data is reset when filter is applied
            updateWithEffect (updateSubstringFilter filter) (Model model)

        RefreshMsg ->
            -- FIXME: disable button
            ( Model model, [], Just Refresh )

        UpdateFilterMsg filter ->
            -- TODO: proper setter with invariants
            let
                effects =
                    case model.options.storage of
                        StorageOptions options ->
                            [ SaveFilterInLocalStorage options.key filter options.saveToLocalStoragePort ]

                        NoStorage ->
                            []
            in
            ( Model
                { model
                    | filter = filter
                    , data = List.filter (model.applyFilter (createPredicate filter)) model.initialData
                }
            , effects
            , Nothing
            )

        ParentMsg m ->
            -- FIXME: do we know the effects ?
            ( Model model, [], Just (OnHtml m) )


{-| The internal predicate : it always trims and use the lowercase filter
-}
createPredicate : Filter -> (String -> Bool)
createPredicate filter =
    case filter of
        Substring _ ->
            String.toLower >> String.contains (getFilterTextValue filter |> String.toLower |> String.trim)

        EmptyFilter ->
            \_ -> True



-- public utils


{-| A filter where a|b allows to match a row with two consecutive columns with the values `a` and `b`.
-}
filterByValues : (row -> List String) -> ApplyFilterPredicate row
filterByValues rowToValues =
    \p r -> r |> rowToValues |> String.join "|" |> String.toLower |> p



-- message constructors


updateFilter : Filter -> Msg parentMsg
updateFilter =
    UpdateFilterMsg


updateSubstringFilter : String -> Msg parentMsg
updateSubstringFilter str =
    str
        |> String.toList
        |> NonEmptyList.fromList
        |> Maybe.map (\s -> UpdateFilterMsg (Substring s))
        |> Maybe.withDefault (UpdateFilterMsg EmptyFilter)


sortColumn : ColumnName -> Msg parentMsg
sortColumn =
    SortColumn



-- internal helpers


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
    { a | sortBy : ColumnName, sortOrder : SortOrder }


setSortOrToggle : ColumnName -> Sort a -> Sort a
setSortOrToggle columnName sortState =
    let
        toggle =
            case sortState.sortOrder of
                Asc ->
                    Desc

                Desc ->
                    Asc
    in
    if columnName == sortState.sortBy then
        { sortState | sortOrder = toggle }

    else
        { sortState | sortBy = columnName, sortOrder = Asc }



-- storage and encoding


storageValueTypeText : StorageValueType -> String
storageValueTypeText valueType =
    case valueType of
        StorageFilter ->
            "storageFilter"


encodeFilter : Filter -> Value
encodeFilter filter =
    Encode.string (getFilterTextValue filter)


encodeStorageValueType : StorageValueType -> Value
encodeStorageValueType valueType =
    Encode.string (storageValueTypeText valueType)


encodeStorageEffect : Effect msg -> Value
encodeStorageEffect effect =
    case effect of
        SaveFilterInLocalStorage key filter _ ->
            Encode.object
                [ ( "key", Encode.string key )
                , ( "type", encodeStorageValueType StorageFilter )
                , ( "value", encodeFilter filter )
                ]



-- Views


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
            table [ class "no-footer dataTable" ]
                [ thead theadAttrs [ tableHeader options.refresh columns model ]
                , tbody tbodyAttrs (tableBody columns data)
                ]

        content =
            case viewHeaderOptions model.filter model.options of
                [] ->
                    [ tableView ]

                elems ->
                    [ div [ class "dataTables_wrapper_top d-flex justify-content-between" ] elems
                    , tableView
                    ]
    in
    div tableContainerAttrs content


viewHeaderOptions : Filter -> Options row msg -> List (Html (Msg msg))
viewHeaderOptions currentFilter { filter, refresh } =
    case ( filter, refresh ) of
        ( NoFilter, NoRefreshButton ) ->
            []

        ( FilterHeaderOptions filterAttrs, NoRefreshButton ) ->
            [ input ([ class "form-control", type_ "text", placeholder "Filter...", onInput FilterInputChanged, value (getFilterTextValue currentFilter) ] ++ filterAttrs) []
            ]

        ( FilterHeaderOptions filterAttrs, RefreshButtonOptions refreshAttrs ) ->
            [ input ([ class "form-control", type_ "text", placeholder "Filter...", onInput FilterInputChanged, value (getFilterTextValue currentFilter) ] ++ filterAttrs) []

            -- TODO : when there are export options, it should be a group of buttons at the end
            , button refreshAttrs [ i [ class "fa fa-refresh" ] [] ]
            ]

        ( NoFilter, RefreshButtonOptions refreshAttrs ) ->
            --TODO: should the buttons still be aligned at the end ?
            [ div [ class "ms-auto me-0" ]
                [ button refreshAttrs [ i [ class "fa fa-refresh" ] [] ]
                ]
            ]


tableHeader : RefreshButtonOptions msg -> NonEmptyList.Nonempty (Column row msg) -> Sort a -> Html (Msg msg)
tableHeader refreshOptions columns sort =
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
        [ tr [] [ td [ class "empty", colspan 5 ] [ i [ class "fa fa-exclamation-triangle" ] [], text "No nodes match your filters." ] ] ]
