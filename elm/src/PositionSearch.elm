port module PositionSearch exposing (main)

import Board exposing (Castle(..))
import Board.Colour exposing (Colour(..))
import Browser
import Http
import Parser
import Position
import PositionSearch.Model exposing (Model)
import PositionSearch.Msg exposing (Msg(..))
import PositionSearch.View as View


port signalDomRendered3 : () -> Cmd msg


port signalFenChanged3 : (String -> msg) -> Sub msg


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
            { position = Position.empty }
    in
    ( model, signalDomRendered3 () )


subscriptions : Model -> Sub Msg
subscriptions _ =
    signalFenChanged3 BoardFenChanged


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        BoardFenChanged fen ->
            let
                eBoard =
                    Parser.run Board.boardParser fen

                position =
                    model.position

                newPosition board =
                    { position | board = board }
            in
            case eBoard of
                Ok board ->
                    ( { model | position = newPosition board }, Cmd.none )

                Err oops ->
                    -- TODO
                    ( model, Cmd.none )

        CastleChecked colour castle bool ->
            let
                position =
                    model.position
            in
            ( { model
                | position = Position.setCastle position colour castle bool
              }
            , Cmd.none
            )
