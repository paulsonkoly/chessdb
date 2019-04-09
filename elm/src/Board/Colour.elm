module Board.Colour exposing
    ( Colour(..)
    , flip
    , fromEncoded
    , toUrlQueryParameter
    )

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


fromEncoded : Int -> Result String Colour
fromEncoded i =
    case i of
        0 ->
            Ok White

        1 ->
            Ok Black

        _ ->
            Err ("colour encoding is out of range " ++ String.fromInt i)



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
