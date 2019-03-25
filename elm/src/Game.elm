-- | Game and related structures
module Game exposing (Game, Move)

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
