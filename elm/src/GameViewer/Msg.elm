module GameViewer.Msg exposing (Msg(..), Scrolling(..))

import Game exposing (..)
import Http
import Popularities exposing (Popularities)


type Scrolling
    = Scroll
    | NoScroll


type Msg
    = GameReceived (Result Http.Error Game)
    | PopularitiesReceived (Result Http.Error Popularities)
    | SetMoveNumberTo Int Scrolling
    | PopularitiesEvent Popularities.Msg
    | Noop
