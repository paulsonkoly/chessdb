module Msg exposing (Msg(..), Scrolling(..))

import Game exposing (..)
import Http


type Scrolling
    = Scroll
    | NoScroll


type Msg
    = GameReceived (Result Http.Error Game)
    | PopularitiesReceived (Result Http.Error Popularities)
    | SetMoveNumberTo Int Scrolling
    | Noop
