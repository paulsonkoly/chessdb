module Msg exposing (..)

import Http

import Game exposing (..)


type Msg
  = GameReceived (Result Http.Error Game)
  | PopularitiesReceived (Result Http.Error Popularities)
  | SetMoveNumberTo Int
  | Noop
