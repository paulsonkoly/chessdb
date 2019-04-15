module Loadable exposing
    ( Loadable(..)
    , map
    , toMaybe
    , viewLoadable, viewLoadableList
    )

{-| Server loaded data

#Types

@docs Loadable

#Data manipulation

@docs map

#Conversions

@docs toMaybe

#Views

@docs viewLoadable, viewLoadableList

-}

import Html exposing (Html, text)
import Http
import String.Conversions as Conversions



------------------------------------------------------------------------
--                               Types                                --
------------------------------------------------------------------------


{-| Loadable data

  - `NoRequest` when we don't have data but also no request was sent
    to the server.
  - `Loading` when we have stale data or no data, but a request is sent.
  - `Loaded` when a request is completed, either with Err or Ok.

-}
type Loadable a
    = NoRequest
    | Loading
    | Loaded (Result Http.Error a)



------------------------------------------------------------------------
--                         Data manipulation                          --
------------------------------------------------------------------------


{-| Loadable is "functor" over the Ok loaded data.
-}
map : (a -> b) -> Loadable a -> Loadable b
map f loadable =
    case loadable of
        Loaded (Ok data) ->
            Loaded (Ok (f data))

        NoRequest ->
            NoRequest

        Loaded (Err oops) ->
            Loaded (Err oops)

        Loading ->
            Loading



------------------------------------------------------------------------
--                            Conversions                             --
------------------------------------------------------------------------


{-| Convert to Maybe.

Loaded (Ok a) to Just a, everything else to nothing

-}
toMaybe : Loadable a -> Maybe a
toMaybe loadable =
    case loadable of
        Loaded (Ok data) ->
            Just data

        _ ->
            Nothing



------------------------------------------------------------------------
--                               Views                                --
------------------------------------------------------------------------


{-| Renders a loadable, the user spcifying how to render loaded data or no
request.
-}
viewLoadable : Loadable a -> (a -> Html msg) -> Html msg -> Html msg
viewLoadable loadable viewNormal viewNoRequest =
    case loadable of
        NoRequest ->
            viewNoRequest

        -- todo spinner
        Loading ->
            text "Loading"

        -- todo display nicely
        Loaded (Err oops) ->
            text <| Conversions.fromHttpError oops

        Loaded (Ok a) ->
            viewNormal a


{-| Same as `viewLoadable` except returns a List so one can ++ it to existing
list of Html.
-}
viewLoadableList :
    Loadable a
    -> (a -> List (Html msg))
    -> List (Html msg)
    -> List (Html msg)
viewLoadableList loadable viewNormal viewNoRequest =
    case loadable of
        NoRequest ->
            viewNoRequest

        -- todo spinner
        Loading ->
            [ text "Loading" ]

        -- todo display nicely
        Loaded (Err oops) ->
            [ text <| Conversions.fromHttpError oops ]

        Loaded (Ok a) ->
            viewNormal a
