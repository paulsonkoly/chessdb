module Game exposing (Game, Move, Popularities, PopularityItem)

import Array exposing (Array)


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
    , items : List PopularityItem
    }


type alias PopularityItem =
    { nextSan : String
    , whiteWon : Int
    , draw : Int
    , blackWon : Int
    , totalCount : Int
    }
