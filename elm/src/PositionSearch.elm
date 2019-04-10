port module PositionSearch exposing (main)

import Browser
import PositionSearch.Model as Model exposing (Model)
import PositionSearch.View as View


port signalDomRendered3 : () -> Cmd msg


port signalFenChanged3 : (String -> msg) -> Sub msg


type Msg
    = BoardFenChanged String


main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = View.view
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
