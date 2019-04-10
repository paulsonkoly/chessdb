port module PositionSearch exposing (main)

import Board exposing (Castle(..))
import Board.Colour exposing (Colour(..))
import Board.Square as Square
import Browser
import FormError exposing (Error(..))
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
            { position = Position.empty
            , enPassantStringError = NoError
            }
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

        ActiveColourChecked colour ->
            let
                position =
                    model.position

                newPosition =
                    { position | activeColour = colour }
            in
            ( { model | position = newPosition }, Cmd.none )

        EnPassantInputted string ->
            let
                eSquare =
                    Parser.run Square.parser string

                position =
                    model.position

                newPosition mSquare =
                    { position | enPassant = mSquare }
            in
            case ( string, eSquare ) of
                ( "", _ ) ->
                    ( { model
                        | position = newPosition Nothing
                        , enPassantStringError = NoError
                      }
                    , Cmd.none
                    )

                ( _, Ok square ) ->
                    ( { model
                        | position = newPosition (Just square)
                        , enPassantStringError = NoError
                      }
                    , Cmd.none
                    )

                ( _, Err _ ) ->
                    ( { model
                        | position = newPosition Nothing
                        , enPassantStringError =
                            Error "square like \"e3\" expected"
                      }
                    , Cmd.none
                    )
