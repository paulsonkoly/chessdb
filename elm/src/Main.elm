import Browser
import Platform.Sub
import Platform.Cmd
import Array
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Html.Attributes exposing (class, id, style)
import Http
import String.Conversions exposing(fromHttpError)

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

type Msg = Received (Result Http.Error Game)

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
update msg model = -- (model, Cmd.none)
  case msg of
    Received (Ok game) -> (Loaded game 0, Cmd.none)
    Received (Err oops) -> (Error oops, Cmd.none)


view : Model -> Html msg
view model =
  case model of
    Loaded game currentMove ->
      let
          { moves } = game
      in V.viewMoveList (Array.toList moves) currentMove
    Loading -> text "Loading..."
    Error (httpError) -> text <| fromHttpError httpError
