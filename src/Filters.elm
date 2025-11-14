module Filters exposing
    ( FilterStringPredicate
    , Predicate
    , SearchFilterState(..)
    , and
    , apply
    , applyString
    , byValues
    , getTextValue
    , minLength
    , stringPredicate
    , substring
    )

import List.Nonempty as NonEmptyList


{-| A function that, when used with the filters utils exposed by this module,
can be used to filter data from an input String.

See `apply` for concrete usage.

-}
type alias FilterStringPredicate data =
    (String -> Bool) -> Predicate data


type alias Predicate data =
    data -> Bool


{-| The different possible states of the search component.

The component can only known filtering logic to apply,
and is initially configured with a filter value and the predicate to apply.

The filter can be empty (e.g. when the input is empty, which is usually the default).

Other possible filters include RegExp, match option values, case sensitive variants

-}
type SearchFilterState
    = Substring (NonEmptyList.Nonempty Char)
    | EmptyFilter



{- MAIN PUBLIC UTILS -}


substring : String -> SearchFilterState
substring value =
    value
        |> String.toList
        |> NonEmptyList.fromList
        |> Maybe.map Substring
        |> Maybe.withDefault EmptyFilter


{-| Obtain a filter that is applicable to data by using stringified values from the data.
For instance with `{ id: String, name: String, description: String }`, `filterByValues ({ id, name } -> [ id, name ])`
will skip the description field and by both `id` and `name`.

The syntax `a|b` allows to match a row with two consecutive columns with the values `a` and `b`.
So, by construct, ordering of `|` matters.

-}
byValues : (data -> List String) -> FilterStringPredicate data
byValues dataToValues =
    \p r -> r |> dataToValues |> String.join "|" |> String.toLower |> p


{-| Start applying a filter only when the input has a given length (non-strict bound).
-}
minLength : Int -> FilterStringPredicate data -> FilterStringPredicate data
minLength length filter =
    filter << (\p -> \s -> length <= String.length s || p s)


{-| Assume that we filter a list of elements, and return a filtered list in the same order,
without identifying each element.

Filtering on list may hinder performance since Html.Keyed could be used for optimization,
so consider using dictionary-based structure, or include a `String` identifier in the data.

-}
applyString : FilterStringPredicate data -> SearchFilterState -> List data -> List data
applyString pred state =
    apply (pred <| stringPredicate state)


and : Predicate data -> Predicate data -> Predicate data
and p1 p2 =
    \d -> p1 d && p2 d


apply : Predicate data -> List data -> List data
apply pred data =
    List.filter pred data



{- UTILS -}


getTextValue : SearchFilterState -> String
getTextValue state =
    case state of
        Substring filterString ->
            String.fromList (NonEmptyList.toList filterString)

        EmptyFilter ->
            ""


{-| Predicate on search input filter : it always trims and use the lowercase filter
-}
stringPredicate : SearchFilterState -> Predicate String
stringPredicate state =
    case state of
        Substring _ ->
            String.toLower >> String.contains (getTextValue state |> String.toLower |> String.trim)

        EmptyFilter ->
            \_ -> True
