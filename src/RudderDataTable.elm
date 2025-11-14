module RudderDataTable exposing
    ( Column
    , ColumnName(..)
    , Config
    , ConfigBuilder
    , Customizations
    , Effect(..)
    , FilterOptions
    , FilterOptionsType(..)
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
    , getFilterOptionValue
    , getRows
    , getSort
    , init
    , size
    , sortColumn
    , storageOptions
    , update
    , updateData
    , updateFilter
    , updateWithEffect
    , view
    )

import Filters exposing (FilterStringPredicate, Predicate, SearchFilterState, apply, getTextValue, stringPredicate, substring)
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
    , options : Options row msg
    }


type RefreshButtonOptions msg
    = NoRefreshButton
    | RefreshButtonOptions (List (Attribute (Msg msg)))


{-| The option to include an search input filter in the table header, or filter with arbitrary HTML in the table header,
or to use filters that are external to the table.

SearchInputFilter provides functionality to save to localStorage, so use it along the storage options if needed.

-}
type FilterOptions row msg
    = NoFilter
    | FilterOptions (FilterOptionsType row msg)


type FilterOptionsType row msg
    = HtmlFilter { predicate : Predicate row, html : Html msg, toMsg : msg -> Msg msg }
    | SearchInputFilter { predicate : FilterStringPredicate row, state : SearchFilterState }


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
    , filter : FilterOptions row msg
    }


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
    | FilterMsg
    | FilterInputChanged String
    | UpdateFilterMsg SearchFilterState
    | ParentMsg parentMsg


{-| The public message to be exposed to parent components
-}
type OutMsg parentMsg
    = Refresh
    | Filter
    | OnHtml parentMsg


{-| A representation of concrete effects in the type system to allow testing
-}
type Effect parentMsg
    = SaveFilterInLocalStorage StorageKey SearchFilterState (Value -> Cmd parentMsg)



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
            |> buildConfig.withOptions
                (buildOptions.newOptions
                    |> buildOptions.withRefresh [ class "btn btn-primary" ]
                    |> buildOptions.withFilter filterByFields (Substring (NonEmptyList.Nonempty 'i' [ 'd', '0' ])) [ class "input-group" ]
                )

@deprecated newConfig : use newColumnsConfig, and add filters using the withSearchFilter options

-}
type alias ConfigBuilder row msg =
    { newConfig : NonEmptyList.Nonempty (Column row msg) -> Config row msg
    , withSortBy : ColumnName -> Config row msg -> Config row msg
    , withSortOrder : SortOrder -> Config row msg -> Config row msg
    , withOptions : Options row msg -> Config row msg -> Config row msg
    }


type alias OptionsBuilder row msg =
    { newOptions : Options row msg
    , withRefresh : List (Attribute (Msg msg)) -> Options row msg -> Options row msg
    , withStorage : StorageOptionsConfig msg -> Options row msg -> Options row msg
    , withFilter : FilterOptionsType row msg -> Options row msg -> Options row msg
    }


buildConfig : ConfigBuilder row msg
buildConfig =
    { newConfig = defaultConfig
    , withSortBy = \value -> \state -> { state | sortBy = value }
    , withSortOrder = \value -> \state -> { state | sortOrder = value }
    , withOptions = \value -> \state -> { state | options = value }
    }


buildOptions : OptionsBuilder row msg
buildOptions =
    { newOptions = defaultOptions
    , withRefresh = \opt -> \state -> { state | refresh = RefreshButtonOptions opt }
    , withStorage = \opt -> \state -> { state | storage = StorageOptions opt }
    , withFilter = \opt -> \state -> { state | filter = FilterOptions opt }
    }



-- defaults


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


getSort : Model row msg -> ( ColumnName, SortOrder )
getSort (Model { sortBy, sortOrder }) =
    ( sortBy, sortOrder )


getRows : Model row msg -> List row
getRows (Model { data }) =
    data


size : Model row msg -> Int
size (Model { data }) =
    List.length data



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

        RefreshMsg ->
            -- TODO: disable button
            ( Model model, [], Just Refresh )

        -- FilterMsg ->
        --     -- FIXME: filter model, update invariants
        --     let
        --         effects =
        --             case model.options.storage of
        --                 StorageOptions options ->
        --                     [ SaveFilterInLocalStorage options.key options.saveToLocalStoragePort ]
        --                 NoStorage ->
        --                     []
        --     in
        --     ( Model
        --         { model
        --             -- FIXME: call filter "model update"
        --             , data = List.filter (model.applyFilter (createPredicate filter)) model.initialData
        --         }
        --     , effects
        --     , Nothing
        --     )
        FilterMsg ->
            ( Model model, [], Just Filter )

        FilterInputChanged s ->
            updateWithEffect (UpdateFilterMsg (substring s)) (Model model)

        UpdateFilterMsg s ->
            let
                ( newModel, effects ) =
                    updateOnFilterInput s (Model model)
            in
            ( newModel, effects, Nothing )

        ParentMsg m ->
            -- FIXME: do we know the effects ?
            ( Model model, [], Just (OnHtml m) )


updateFilter : SearchFilterState -> Msg msg
updateFilter =
    UpdateFilterMsg


updateOnFilterInput : SearchFilterState -> Model row msg -> ( Model row msg, List (Effect msg) )
updateOnFilterInput newFilterState (Model model) =
    let
        (Model modelWithFilter) =
            Model model
                |> updateFilterOptions (setSearchInputFilterState newFilterState)

        predicate =
            getFilterOptionPredicate modelWithFilter.options.filter

        newModel =
            filterData predicate (Model modelWithFilter)
    in
    case model.options.storage of
        StorageOptions options ->
            ( newModel, [ SaveFilterInLocalStorage options.key newFilterState options.saveToLocalStoragePort ] )

        NoStorage ->
            ( newModel, [] )


getFilterOptionValue : Model row msg -> String
getFilterOptionValue (Model { options }) =
    case options.filter of
        FilterOptions (SearchInputFilter { state }) ->
            getTextValue state

        _ ->
            ""


getFilterOptionPredicate : FilterOptions row msg -> Predicate row
getFilterOptionPredicate options =
    case options of
        NoFilter ->
            \_ -> True

        FilterOptions (HtmlFilter { predicate }) ->
            predicate

        FilterOptions (SearchInputFilter { predicate, state }) ->
            predicate <| stringPredicate state


filterData : (row -> Bool) -> Model row msg -> Model row msg
filterData pred (Model model) =
    Model { model | data = apply pred model.data }


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



-- public utils
-- message constructors


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



--FIXME: we need to enforce in the Options, that if Search && Storage are configured, how to store any kind of "filter model"


encodeFilter : SearchFilterState -> Value
encodeFilter filter =
    Encode.string (getTextValue filter)


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
                [ thead theadAttrs [ tableHeader columns model ]
                , tbody tbodyAttrs (tableBody columns data)
                ]

        content =
            case viewHeaderOptions model.options of
                [] ->
                    [ tableView ]

                elems ->
                    [ div [ class "dataTables_wrapper_top d-flex justify-content-between" ] elems
                    , tableView
                    ]
    in
    div tableContainerAttrs content


viewHeaderOptions : Options row msg -> List (Html (Msg msg))
viewHeaderOptions { filter, refresh } =
    case ( filter, refresh ) of
        ( NoFilter, NoRefreshButton ) ->
            []

        ( NoFilter, (RefreshButtonOptions _) as option ) ->
            --TODO: should the buttons still be aligned at the end ?
            [ div [ class "ms-auto me-0" ] (viewRefreshButton option)
            ]

        -- FIXME: when there are export options, it should be a group of buttons at the end
        ( FilterOptions (SearchInputFilter { state }), refreshOptions ) ->
            [ input [ class "form-control", type_ "text", placeholder "Filter...", onInput FilterInputChanged, value (getTextValue state) ] []
            ]
                ++ viewRefreshButton refreshOptions

        ( FilterOptions (HtmlFilter { html, toMsg }), refreshOptions ) ->
            [ Html.map toMsg html
            ]
                ++ viewRefreshButton refreshOptions


viewRefreshButton : RefreshButtonOptions msg -> List (Html (Msg msg))
viewRefreshButton option =
    case option of
        NoRefreshButton ->
            []

        RefreshButtonOptions attrs ->
            [ button attrs [ i [ class "fa fa-refresh" ] [] ] ]


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
