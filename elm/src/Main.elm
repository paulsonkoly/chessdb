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
port signalDomRendered : () -> Cmd msg

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
update msg model = -- (model, Cmd.none)
  case (msg, model) of
    (Received (Ok game), _) -> (Loaded game -1, signalDomRendered ())
    (Received (Err oops), _) -> (Error oops, Cmd.none)
    (ButtonAction (V.ToStartPosition), Loaded game _) ->
      (Loaded game -1, Cmd.none)
    (ButtonAction (V.ToLeft), Loaded game move) ->
      if move < 0 then
        (Loaded game move, Cmd.none)
      else
        (Loaded game (move - 1), Cmd.none)
    (ButtonAction (V.ToRight), Loaded game move) ->
      let { moves } = game
      in
        if move >= Array.length moves - 1 then
          (Loaded game move, Cmd.none)
        else
          (Loaded game (move + 1), Cmd.none)
    (ButtonAction (V.ToEndPosition), Loaded game move) ->
      let { moves } = game
      in (Loaded game (Array.length moves - 1), Cmd.none)
    _ -> todo "Whoops"


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
