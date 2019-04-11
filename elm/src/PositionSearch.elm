port module PositionSearch exposing (main)

import Board exposing (Castle(..))
import Board.Colour exposing (Colour(..))
import Board.Square as Square
import Browser
import FormError exposing (Error(..))
import Http
import Loadable exposing (Loadable(..))
import Pagination
import Parser
import Position
import PositionSearch.Model as Model exposing (Model)
import PositionSearch.Msg exposing (Msg(..))
import PositionSearch.ServerResponse as ServerResponse
import PositionSearch.View as View
import Url.Builder as Url


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
    ( Model.init, signalDomRendered3 () )


subscriptions : Model -> Sub Msg
subscriptions _ =
    signalFenChanged3 BoardFenChanged


postRequest : Model -> Cmd Msg
postRequest model =
    Http.post
        { url = Url.absolute [ "positions", "search" ] []
        , body = Http.jsonBody (Model.jsonEncode model)
        , expect =
            Http.expectJson GamesReceived ServerResponse.jsonDecoder
        }


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

        SearchClicked ->
            ( { model
                | games = Loading
                , pagination = Pagination.init
              }
            , postRequest model
            )

        GamesReceived response ->
            ( { model
                | pagination =
                    response
                        |> Result.map .pagination
                        |> Result.toMaybe
                        |> Maybe.withDefault model.pagination
                , games = Loadable.map .games (Loaded response)
              }
            , Cmd.none
            )

        PaginationRequested (Pagination.Request offset) ->
            let
                newPagination =
                    model.pagination
                        |> Pagination.setOffset offset
                        |> Pagination.setBusy True

                newModel =
                    { model | pagination = newPagination }
            in
            ( newModel, postRequest newModel )
