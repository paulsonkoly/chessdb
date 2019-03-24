-- | Game and related structures
module Game exposing (Game, Move, Colour(..))

import Array exposing(Array)

type Colour = White | Black


type alias Move =
  { id : Int
  , fenPosition : String
  , san : String
  , activeColour : Colour
  , fullMoveNumber : Int
  , castlingAvailability : Int -- TODO
  , halfmoveClock : Int
  , enPassant : Maybe Int -- TODO
  }


type alias Game =
  { todoGameProperties : ()
  , moves : Array Move
  }
