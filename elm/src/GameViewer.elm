port module GameViewer exposing (main)

import Array
import Browser
import Browser.Dom as Dom
import Browser.Events as Events
import Game exposing (..)
import Game.Decoder exposing (..)
import GameViewer.Model exposing (..)
import GameViewer.Msg exposing (Msg(..), Scrolling(..))
import GameViewer.View exposing (..)
import Http
import Json.Decode as Decode
import Loadable exposing (..)
import Platform.Cmd
import Platform.Sub
import Task
import Url.Builder as Url


port signalDomRendered : () -> Cmd msg


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
        initialQuery =
            [ Url.string "fen" "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR"
            , Url.int "castle" 15
            , Url.int "active_colour" 1
            ]

        moveQuery move =
            List.filterMap identity
                [ Just (Url.string "fen" move.fenPosition)
                , Just (Url.int "castle" move.castlingAvailability)
                , Just (Url.int "active_colour" move.activeColour)
                , move.enPassant |> Maybe.map (Url.int "en_passant")
                ]

        mQuery =
            getMoveProperty model.move model moveQuery initialQuery
                |> Maybe.map (\l -> Url.int "token" model.token :: l)

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
                            popularitiesDecoder
                    }
            )
            mUrl


init : Int -> ( Model, Cmd Msg )
init id =
    let
        model =
            { game = Loading, move = -1, token = 1, popularities = Loading }
    in
    ( model
    , Cmd.batch
        [ Http.get
            { url = "/games/" ++ String.fromInt id ++ ".json"
            , expect = Http.expectJson GameReceived gameDecoder
            }
        , cmdFetchPopularitiesFor model
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
        GameReceived game ->
            ( { model | game = Loaded game }
            , case game of
                Ok _ ->
                    signalDomRendered ()

                Err _ ->
                    Cmd.none
            )

        ------------------------------------------------------------------------
        PopularitiesReceived receivedData ->
            ( if getToken receivedData == Just model.token then
                { model | popularities = Loaded receivedData }

              else
                case ( receivedData, model.popularities ) of
                    ( Err oops, Loading ) ->
                        { model | popularities = Loaded (Err oops) }

                    _ ->
                        model
            , Cmd.none
            )

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


getToken : Result Http.Error Popularities -> Maybe Int
getToken popularities =
    Result.toMaybe popularities |> Maybe.map .token
