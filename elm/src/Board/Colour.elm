module Board.Colour exposing (Colour(..), flip, toUrlQueryParameter)

import Url.Builder as Url


type Colour
    = White
    | Black



------------------------------------------------------------------------
--                            Conversions                             --
------------------------------------------------------------------------


toUrlQueryParameter : String -> Colour -> Url.QueryParameter
toUrlQueryParameter str colour =
    case colour of
        White ->
            Url.int str 0

        Black ->
            Url.int str 1



------------------------------------------------------------------------
--                         Data manipulations                         --
------------------------------------------------------------------------


flip : Colour -> Colour
flip colour =
    case colour of
        White ->
            Black

        Black ->
            White
