module Loadable exposing (Loadable(..), viewLoadable, viewLoadableList)

import Html exposing (Html, text)
import Http
import String.Conversions exposing (fromHttpError)



--------------------------------------------------------------------------------


type Loadable a
    = Loading
    | Loaded (Result Http.Error a)


viewLoadable : Loadable a -> (a -> Html msg) -> Html msg
viewLoadable loadable viewNormal =
    case loadable of
        Loading ->
            text "Loading"

        -- todo spinner
        Loaded (Err oops) ->
            text <| fromHttpError oops

        -- todo display nicely
        Loaded (Ok a) ->
            viewNormal a


viewLoadableList : Loadable a -> (a -> List (Html msg)) -> List (Html msg)
viewLoadableList loadable viewNormal =
    case loadable of
        Loading ->
            [ text "Loading" ]

        -- todo spinner
        Loaded (Err oops) ->
            [ text <| fromHttpError oops ]

        -- todo display nicely
        Loaded (Ok a) ->
            viewNormal a
