module Board.Colour exposing
    ( Colour(..)
    , flip
    , fromEncoded
    , fromHtmlToggle
    , jsonEncode
    , toHtmlAttribute
    , toUrlQueryParameter
    )

import Html
import Html.Attributes as Attributes
import Json.Encode
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


toHtmlAttribute : Colour -> Html.Attribute msg
toHtmlAttribute colour =
    case colour of
        White ->
            Attributes.checked True

        Black ->
            Attributes.checked False


fromHtmlToggle : Bool -> Colour
fromHtmlToggle bool =
    if bool then
        White

    else
        Black


jsonEncode : Colour -> Json.Encode.Value
jsonEncode colour =
    case colour of
        White ->
            Json.Encode.int 0

        Black ->
            Json.Encode.int 1



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
