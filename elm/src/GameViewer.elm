port module GameViewer exposing (main)

import Array
import Board
import Browser
import Browser.Dom as Dom
import Browser.Events as Events
import Browser.Navigation as Browser
import Game exposing (..)
import Game.Decoder exposing (..)
import GameViewer.Model exposing (..)
import GameViewer.Msg exposing (Msg(..), Scrolling(..))
import GameViewer.View exposing (..)
import Http
import Json.Decode as Decode
import Loadable exposing (..)
import Maybe.Extra as Maybe
import Parser
import Platform.Cmd
import Platform.Sub
import Popularities exposing (Popularities)
import Position
import Task
import Url.Builder as Url


port signalDomRendered : String -> Cmd msg


port signalFenChanged : String -> Cmd msg


main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


getMoveProperty : Int -> Model -> (Move -> a) -> a -> Maybe a
getMoveProperty ix model f default =
    if ix == -1 then
        Just default

    else
        Loadable.toMaybe model.game
            |> Maybe.map .moves
            |> Maybe.andThen (Array.get ix)
            |> Maybe.map f


cmdFetchPopularitiesFor : Model -> Cmd Msg
cmdFetchPopularitiesFor model =
    let
        mInitial =
            Just Position.initial

        mSpecific move =
            Position.specific
                move.fenPosition
                move.castlingAvailability
                move.activeColour
                move.enPassant
                |> Result.toMaybe

        mQuery =
            getMoveProperty model.move model mSpecific mInitial
                |> Maybe.join
                |> Maybe.map
                    (\pos ->
                        Url.int "token" model.token :: Position.urlEncode pos
                    )

        mUrl =
            mQuery |> Maybe.map (Url.absolute [ "moves", "popularities.json" ])
    in
    Maybe.withDefault Cmd.none <|
        Maybe.map
            (\url ->
                Http.get
                    { url = url
                    , expect =
                        Http.expectJson
                            PopularitiesReceived
                            Popularities.decoder
                    }
            )
            mUrl


type alias Flags =
    { id : Int, halfmoveNumber : Int }


init : Flags -> ( Model, Cmd Msg )
init { id, halfmoveNumber } =
    let
        model =
            { game = Loading
            , move = halfmoveNumber
            , token = 1
            , popularities = Loading
            }
    in
    ( model
    , Cmd.batch
        [ Http.get
            { url = "/games/" ++ String.fromInt id ++ ".json"
            , expect = Http.expectJson GameReceived gameDecoder
            }
        , if halfmoveNumber == -1 then
            cmdFetchPopularitiesFor model

          else
            Cmd.none
        ]
    )


keyDecoder : Int -> Decode.Decoder Msg
keyDecoder move =
    let
        stringToMsg str =
            case str of
                "ArrowLeft" ->
                    SetMoveNumberTo (move - 1) Scroll

                "h" ->
                    SetMoveNumberTo (move - 1) Scroll

                "ArrowRight" ->
                    SetMoveNumberTo (move + 1) Scroll

                "l" ->
                    SetMoveNumberTo (move + 1) Scroll

                _ ->
                    Noop
    in
    Decode.map stringToMsg (Decode.field "key" Decode.string)


subscriptions : Model -> Sub Msg
subscriptions { move } =
    Events.onKeyPress (keyDecoder move)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ------------------------------------------------------------------------
        GameReceived eGame ->
            let
                newModel =
                    { model | game = Loaded eGame }

                fen =
                    getMoveProperty
                        model.move
                        newModel
                        .fenPosition
                        "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR"
                        |> Maybe.withDefault "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR"
            in
            ( newModel
            , case eGame of
                Ok game ->
                    Cmd.batch
                        [ signalDomRendered fen
                        , if model.move == -1 then
                            Cmd.none

                          else
                            cmdFetchPopularitiesFor newModel
                        ]

                Err _ ->
                    Cmd.none
            )

        ------------------------------------------------------------------------
        PopularitiesReceived receivedData ->
            let
                receivedValid =
                    Result.toMaybe receivedData
                        |> Maybe.map (Popularities.validateToken model.token)
            in
            ( if receivedValid == Just True then
                { model | popularities = Loaded receivedData }

              else
                case ( receivedData, model.popularities ) of
                    ( Err oops, Loading ) ->
                        { model | popularities = Loaded (Err oops) }

                    _ ->
                        model
            , Cmd.none
            )

        PopularitiesEvent (Popularities.MoveClicked san) ->
            let
                move =
                    Result.mapError Parser.deadEndsToString <|
                        Parser.run Board.moveParser san

                -- TODO this code is horrendous
                mFen =
                    getMoveProperty
                        model.move
                        model
                        .fenPosition
                        "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR"

                mCastlingAvailability =
                    getMoveProperty
                        model.move
                        model
                        .castlingAvailability
                        15

                mActiveColour =
                    getMoveProperty
                        model.move
                        model
                        .activeColour
                        0

                mEnPassant =
                    getMoveProperty
                        model.move
                        model
                        .enPassant
                        Nothing

                position =
                    Maybe.map4 (\a b c d -> ( a, b, ( c, d ) ))
                        mFen
                        mCastlingAvailability
                        mActiveColour
                        mEnPassant
                        |> Result.fromMaybe "move property Nothing"
                        |> Result.andThen
                            (\( a, b, ( c, d ) ) -> Position.specific a b c d)

                newPosition =
                    Result.map2 Tuple.pair move position
                        |> Result.andThen
                            (\( m, p ) -> Position.make m p)

                eUrl =
                    Result.map Position.urlEncode newPosition

                cmd =
                    case eUrl of
                        Ok url ->
                            Browser.load
                                (Url.absolute
                                    [ "moves", "explorer" ]
                                    url
                                )

                        Err _ ->
                            Cmd.none
            in
            ( model, cmd )

        ------------------------------------------------------------------------
        SetMoveNumberTo newMoveNumber scroll ->
            if model.move /= newMoveNumber then
                let
                    mfen =
                        getMoveProperty newMoveNumber
                            model
                            .fenPosition
                            "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR"

                    newModel =
                        { model
                            | move = newMoveNumber
                            , token = model.token + 1
                            , popularities = Loading
                        }
                in
                case mfen of
                    Just fen ->
                        ( newModel
                        , Cmd.batch
                            [ signalFenChanged fen
                            , cmdFetchPopularitiesFor newModel
                            , scrollTo scroll newMoveNumber
                            ]
                        )

                    Nothing ->
                        ( model, Cmd.none )

            else
                ( model, Cmd.none )

        ------------------------------------------------------------------------
        Noop ->
            ( model, Cmd.none )


scrollTo : Scrolling -> Int -> Cmd Msg
scrollTo scroll moveNumber =
    let
        y =
            toFloat (((moveNumber // 2) - 12) * 24)
    in
    case scroll of
        Scroll ->
            Task.attempt (always Noop)
                (Dom.getViewportOf "movelist-scroll"
                    |> Task.andThen
                        (\{ viewport } ->
                            Dom.setViewportOf "movelist-scroll" viewport.x y
                        )
                )

        NoScroll ->
            Cmd.none
