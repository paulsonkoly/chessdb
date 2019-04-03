module GameViewer.Model exposing
    ( Model
    , init
    )

import Game exposing (Game)
import Loadable exposing (Loadable(..))
import Popularities exposing (Popularities)


type alias Model =
    { game : Loadable Game
    , move : Int
    , token : Int
    , popularities : Loadable Popularities
    }


init : Model
init =
    { game = Loading, move = -1, token = 1, popularities = Loading }
