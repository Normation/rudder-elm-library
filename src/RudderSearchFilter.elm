module RudderSearchFilter exposing
    ( Model, Msg
    , view, update, init
    , updateFilter, OutMsg(..)
    )

{-| A simple search input, with the [Nested TEA][nested-tea] pattern.

[nested-tea]: https://sporto.github.io/elm-patterns/architecture/nested-tea.html


# TEA

@docs Model, Msg
@docs view, update, init


# Public API

@docs updateFilter, OutMsg

It may be useful to set the filter value from the outside (e.g. for testing).
It will be useful to handle `OutMsg` public message upon change of the filter.

-}

import Filters exposing (FilterStringPredicate, SearchFilterState, applyString, getTextValue, substring)
import Html exposing (Html, input)
import Html.Attributes exposing (class, placeholder, type_, value)
import Html.Events exposing (onInput)


{-| The model of a search input filter. Here the will be always the same, and the state represents the input search.
-}
type Model data
    = Model (FilterStringPredicate data) SearchFilterState


{-| The internal message type
-}
type Msg
    = FilterInputChanged String
    | UpdateFilterState SearchFilterState


{-| The public message type which is emitted when filter has changed
-}
type OutMsg data
    = FilterData (data -> Bool)



{- INIT -}


{-| The init function, which requires the predicate strategy, see `Filters`
-}
init : FilterStringPredicate data -> Model data
init predicate =
    Model predicate Filters.empty



{- UPDATE -}


{-| The update function to call in the parent :

    RudderSearchFilterMsg filterMsg ->
        let
            ( updatedModel, cmd, outMsg ) =
                update filterMsg model.filterModel

            ...do something with public message, e.g. apply filter to some model data...
        in
        ( { model | filterModel = updatedModel }, cmd )

-}
update : Msg -> Model data -> ( Model data, Cmd Msg, Maybe (OutMsg data) )
update msg ((Model pred _) as model) =
    case msg of
        FilterInputChanged s ->
            update (updateFilter s) model

        UpdateFilterState newState ->
            ( Model pred newState, Cmd.none, Just <| FilterData (applyString newState pred) )


{-| Create a message from a given filter string, with the default `substring` filter type.
It may be useful to reset the search input, or set it to a specific value
-}
updateFilter : String -> Msg
updateFilter value =
    UpdateFilterState <| substring value



{- VIEW -}


{-| The main view function for the input, without much customization
-}
view : Model data -> Html Msg
view (Model _ state) =
    input [ class "form-control", type_ "text", placeholder "Filter", onInput FilterInputChanged, value (getTextValue state) ] []
