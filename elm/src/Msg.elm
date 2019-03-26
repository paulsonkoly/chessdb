module Msg exposing (..)

import Http

import Game exposing (Game)

type Msg
  = Received (Result Http.Error Game)
  | SetMoveNumberTo Int


