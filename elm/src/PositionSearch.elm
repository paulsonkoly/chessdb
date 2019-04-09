port module PositionSearch exposing (main)

import Browser
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, id, style)


port signalDomRendered3 : () -> Cmd msg


port signalFenChanged3 : (String -> msg) -> Sub msg


type alias Model =
    { fenPosition : Maybe String }


type Msg
    = BoardFenChanged String


main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        model =
            { fenPosition = Nothing }
    in
    ( model, signalDomRendered3 () )


subscriptions : Model -> Sub Msg
subscriptions _ =
    signalFenChanged3 BoardFenChanged


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ------------------------------------------------------------------------
        BoardFenChanged fen ->
            ( { model | fenPosition = Just fen }, Cmd.none )


view : Model -> Html msg
view model =
    div [ class "grid-x", class "grid-padding-x" ]
        [ div [ class "cell", class "medium-6" ]
            [ div [ id "chessboard", style "width" "400px" ] [] ]
        , div [ class "cell", class "medium-6" ]
            [ text <| Debug.toString model.fenPosition ]
        ]
