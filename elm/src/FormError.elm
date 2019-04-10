module FormError exposing (Error(..), anyError, errorAttribute, viewError)

import Html exposing (Attribute, Html, span, text)
import Html.Attributes exposing (class)


type Error
    = NoError
    | Error String


isError : Error -> Bool
isError err =
    case err of
        NoError ->
            False

        Error _ ->
            True



-- TODO : figure out how to do this without a list. atttributes like checked
-- can do it..


errorAttribute : Error -> List (Attribute msg)
errorAttribute err =
    case err of
        Error string ->
            [ class "is-invalid-input" ]

        NoError ->
            []


viewError : Error -> Html msg
viewError err =
    case err of
        Error oops ->
            span [ class "form-error", class "is-visible" ] [ text oops ]

        NoError ->
            span [ class "form-error" ] []


anyError : List Error -> Bool
anyError =
    List.any isError
