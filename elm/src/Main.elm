port module GameViewer exposing (Model, cmdFetchPopularitiesFor, getFen, getToken, init, keyDecoder, main, scrollTo, signalDomRendered, signalFenChanged, subscriptions, update, view)

import Array
import Browser
import Browser.Dom as Dom
import Browser.Events as Events
import Game exposing (..)
import GameDecoder exposing (..)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class, id, style)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode
import Loadable exposing (..)
import Msg exposing (Msg(..), Scrolling(..))
import Platform.Cmd
import Platform.Sub
import Task
import Url.Builder as Url
import View as V



--------------------------------------------------------------------------------


port signalDomRendered : () -> Cmd msg


port signalFenChanged : String -> Cmd msg


main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }



--------------------------------------------------------------------------------
-- Model


type alias Model =
    { game : Loadable Game
    , move : Int
    , token : Int
    , popularities : Loadable Popularities
    }


cmdFetchPopularitiesFor : String -> Int -> Cmd Msg
cmdFetchPopularitiesFor fen token =
    let
        url =
            Url.absolute [ "moves", "popularities" ]
                [ Url.string "fen" fen
                , Url.int "token" token
                ]
    in
    Http.get
        { url = url
        , expect = Http.expectJson PopularitiesReceived popularitiesDecoder
        }



--------------------------------------------------------------------------------


init : Int -> ( Model, Cmd Msg )
init id =
    ( { game = Loading, move = -1, token = 1, popularities = Loading }
    , Cmd.batch
        [ Http.get
            { url = "/games/" ++ String.fromInt id ++ ".json"
            , expect = Http.expectJson GameReceived gameDecoder
            }
        , cmdFetchPopularitiesFor "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR" 1
        ]
    )



--------------------------------------------------------------------------------


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



--------------------------------------------------------------------------------


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        --------------------------------------------------------------------------------
        GameReceived game ->
            ( { model | game = Loaded game }
            , case game of
                Ok _ ->
                    signalDomRendered ()

                Err _ ->
                    Cmd.none
            )

        --------------------------------------------------------------------------------
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

        --------------------------------------------------------------------------------
        SetMoveNumberTo newMoveNumber scroll ->
            if model.move /= newMoveNumber then
                let
                    mfen =
                        getFen newMoveNumber model
                in
                case mfen of
                    Just fen ->
                        ( { model
                            | move = newMoveNumber
                            , token = model.token + 1
                            , popularities = Loading
                          }
                        , Cmd.batch
                            [ signalFenChanged fen
                            , cmdFetchPopularitiesFor fen (model.token + 1)
                            , scrollTo scroll newMoveNumber
                            ]
                        )

                    Nothing ->
                        ( model, Cmd.none )

            else
                ( model, Cmd.none )

        --------------------------------------------------------------------------------
        Noop ->
            ( model, Cmd.none )


scrollTo : Scrolling -> Int -> Cmd Msg
scrollTo scroll moveNumber =
    let
        y =
            toFloat <| ((moveNumber // 2) - 12) * 24
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


getFen : Int -> Model -> Maybe String
getFen ix model =
    if ix == -1 then
        Just "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR"

    else
        case model.game of
            Loading ->
                Nothing

            Loaded errGame ->
                Result.toMaybe errGame
                    |> Maybe.map .moves
                    |> Maybe.andThen (Array.get ix)
                    |> Maybe.map .fenPosition


getToken : Result Http.Error Popularities -> Maybe Int
getToken popularities =
    Result.toMaybe popularities
        |> Maybe.map .token


view : Model -> Html Msg
view model =
    viewLoadable model.game
        (\gameData ->
            let
                { moves } =
                    gameData
            in
            div [ class "grid-x", class "grid-margin-x" ]
                [ div [ class "cell", class "small-6" ]
                    [ div [ class "grid-y", class "grid-margin-y" ]
                        [ div [ class "cell" ]
                            [ div [ id "board-container", style "position" "relative" ]
                                [ div [ id "chessboard", style "width" "400px" ] [] ]
                            ]
                        , div [ class "cell" ]
                            [ V.viewButtons
                                { moveNumber = model.move
                                , lastMoveNumber = Array.length moves - 1
                                }
                            ]
                        , div [ class "cell" ] [ V.viewPopularities model.popularities ]
                        ]
                    ]
                , div [ class "cell", class "small-4" ]
                    [ V.viewMoveList (Array.toList moves) model.move ]
                ]
        )
