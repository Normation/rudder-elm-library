module RudderSearchFilter exposing
    ( Model
    , Msg
    , OutMsg(..)
    )

import Filters exposing (FilterStringPredicate, SearchFilterState, applyString, getTextValue, substring)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import List.Nonempty as NonEmptyList


{-| The model of a search input filter. Here the will be always the same, and the state represents the input search.
-}
type Model data
    = Model (FilterStringPredicate data) SearchFilterState


type Msg
    = Ignore
    | FilterInputChanged String
    | UpdateFilterState SearchFilterState


type OutMsg data
    = FilterData (List data -> List data)


type alias Config data =
    { predicate : FilterStringPredicate data
    , state : SearchFilterState
    }



{- UPDATE -}


update : Msg -> Model data -> ( Model data, Cmd Msg, Maybe (OutMsg data) )
update msg ((Model pred _) as model) =
    case msg of
        Ignore ->
            ( model, Cmd.none, Nothing )

        FilterInputChanged s ->
            update (updateFilter s) model

        UpdateFilterState newState ->
            ( Model pred newState, Cmd.none, Just <| FilterData (applyString pred newState) )


updateFilter : String -> Msg
updateFilter value =
    UpdateFilterState <| substring value



{- VIEW -}


view : Model data -> Html Msg
view (Model _ state) =
    input [ class "form-control", type_ "text", placeholder "Filter", onInput FilterInputChanged, value (getTextValue state) ] []
