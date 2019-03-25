port module GameViewer exposing (..)

import Browser
import Platform.Sub
import Platform.Cmd
import Array
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Html.Attributes exposing (class, id, style)
import Http
import String.Conversions exposing(fromHttpError)
import Debug exposing (todo)

import Game exposing (..)
import GameDecoder exposing (game)
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
type Model
  = Loading
  | Error Http.Error
  | Loaded Game Int


type Msg
  = Received (Result Http.Error Game)
  | ButtonAction V.Button

--------------------------------------------------------------------------------
init : Int -> (Model, Cmd Msg)
init id =
  ( Loading
  , Http.get
    { url = "/games/" ++ (String.fromInt id) ++ ".json"
    , expect = Http.expectJson Received game
    }
  )

subscriptions : Model -> Sub Msg
subscriptions = always Sub.none

--------------------------------------------------------------------------------
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case (msg, model) of
    (Received (Ok game), _) -> (Loaded game -1, signalDomRendered ())
    (Received (Err oops), _) -> (Error oops, Cmd.none)

    (ButtonAction V.ToStartPosition, Loaded game move) ->
      setMove game move -1

--------------------------------------------------------------------------------
    (ButtonAction V.ToLeft, Loaded game move) ->
      let nextMove = move - 1
      in setMove game move nextMove

--------------------------------------------------------------------------------
    (ButtonAction V.ToRight, Loaded game move) ->
      let nextMove = move + 1
      in setMove game move nextMove

--------------------------------------------------------------------------------
    (ButtonAction V.ToEndPosition, Loaded game move) ->
      let
        { moves } = game
        nextMove = Array.length moves - 1
      in setMove game move nextMove

--------------------------------------------------------------------------------
    _ -> todo "Whoops"


getFenPosition : Move -> String
getFenPosition {fenPosition} = fenPosition


getNextFenPosition : Int -> Array.Array Move -> Maybe String
getNextFenPosition nextMove moves =
  if nextMove == -1 then
    Just "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR"
  else
    Maybe.map getFenPosition <| Array.get nextMove moves


setMove : Game -> Int -> Int -> (Model, Cmd Msg)
setMove game move nextMove =
  let
    { moves } = game
    nextFen = getNextFenPosition nextMove moves
  in case nextFen of
    Just fen ->
      if move /= nextMove then
        (Loaded game nextMove, signalFenChanged fen)
      else
        (Loaded game move, Cmd.none)
    Nothing -> (Loaded game move, Cmd.none)


view : Model -> Html Msg
view model =
  case model of
    Loaded game currentMove ->
      let
          { moves } = game
      in
          div [ class "grid-x", class "grid-margin-x"]
            [ div [class "cell", class "small-6"]
              [ div [class "grid-y", class "grid-margin-y"]
                [ div [class "cell"]
                  [ div [id "board-container", style "position" "relative"]
                    [ div [id "chessboard", style "width" "400px"] []]
                  ]
                , div [class "cell"] [Html.map ButtonAction V.viewButtons]
                ]
              ]
            , div [class "cell", class "small-4"] [V.viewMoveList (Array.toList moves) currentMove]
            ]
    Loading -> text "Loading..."
    Error (httpError) -> text <| fromHttpError httpError
