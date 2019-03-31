module GameViewer.Model exposing
    ( Model
    , init
    )

import Game exposing (Game, Popularities)
import Loadable exposing (Loadable(..))


type alias Model =
    { game : Loadable Game
    , move : Int
    , token : Int
    , popularities : Loadable Popularities
    }


init : Model
init =
    { game = Loading, move = -1, token = 1, popularities = Loading }
