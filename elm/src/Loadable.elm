module Loadable exposing
    ( Loadable(..)
    , map
    , toMaybe
    , viewLoadable
    , viewLoadableList
    )

import Html exposing (Html, text)
import Http
import String.Conversions as Conversions


type Loadable a
    = Loading
    | Loaded (Result Http.Error a)


map : (a -> b) -> Loadable a -> Loadable b
map f loadable =
    case loadable of
        Loaded (Ok data) ->
            Loaded (Ok (f data))

        Loaded (Err oops) ->
            Loaded (Err oops)

        Loading ->
            Loading


toMaybe : Loadable a -> Maybe a
toMaybe loadable =
    case loadable of
        Loaded (Ok data) ->
            Just data

        _ ->
            Nothing


viewLoadable : Loadable a -> (a -> Html msg) -> Html msg
viewLoadable loadable viewNormal =
    case loadable of
        -- todo spinner
        Loading ->
            text "Loading"

        -- todo display nicely
        Loaded (Err oops) ->
            text <| Conversions.fromHttpError oops

        Loaded (Ok a) ->
            viewNormal a


viewLoadableList : Loadable a -> (a -> List (Html msg)) -> List (Html msg)
viewLoadableList loadable viewNormal =
    case loadable of
        -- todo spinner
        Loading ->
            [ text "Loading" ]

        -- todo display nicely
        Loaded (Err oops) ->
            [ text <| Conversions.fromHttpError oops ]

        Loaded (Ok a) ->
            viewNormal a
