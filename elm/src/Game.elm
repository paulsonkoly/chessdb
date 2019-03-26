-- | Game and related structures
module Game exposing (..)

import Array exposing(Array)

type alias Move =
  { id : Int
  , fenPosition : String
  , san : String
  , activeColour : Int
  , fullMoveNumber : Int
  , castlingAvailability : Int -- TODO
  , halfmoveClock : Int
  , enPassant : Maybe Int -- TODO
  }


type alias Game =
  { todoGameProperties : ()
  , moves : Array Move
  }

type alias Popularities =
  { token : Int
  , stats : List MoveStats
  }

type alias MoveStats =
  { san : String
  , whiteWon : Int
  , draw : Int
  , blackWon : Int
  , total : Int
  }
