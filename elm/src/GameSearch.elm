module GameSearch exposing (main)

import Browser
import Date exposing (Date)
import Game exposing (..)
import GameSearch.Model exposing (Model)
import GameSearch.View exposing (view)
import Html exposing (Html)
import Loadable exposing (..)


type Msg
    = Noop


main =
    Browser.element
        { init = init
        , subscriptions = always Sub.none
        , update = update
        , view = view
        }


init : () -> ( Model, Cmd Msg )
init flags =
    ( { white = Nothing
      , black = Nothing
      , eitherColour = Nothing
      , minimumElo = Nothing
      , maximumElo = Nothing
      , event = Nothing
      , site = Nothing
      , date = Nothing
      , round = Nothing
      , result = Nothing
      , eco = Nothing
      , games = Loading
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd msg )
update _ m =
    ( m, Cmd.none )
