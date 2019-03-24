import Browser
import Platform.Sub
import Platform.Cmd
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Http

import Game exposing (..)
import GameDecoder exposing (game)
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
type Model = Loading | Loaded Game Int

type Msg
  = Increment
  | Decrement
  | Received (Result Http.Error Game)

--------------------------------------------------------------------------------
init : Int -> (Model, Cmd Msg)
init id =
  ( Loading
  , Http.get
    { url = "games/" ++ (String.fromInt id) ++ ".json"
    , expect = Http.expectJson Received game
    }
  )

subscriptions = always Platform.Sub.none

--------------------------------------------------------------------------------
update msg model = (model, Platform.Cmd.none)

view model =
  div []
    [ button [ onClick Decrement ] [ text "-" ]
    , button [ onClick Increment ] [ text "+" ]
    ]
