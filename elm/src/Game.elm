module Game exposing
    ( Game
    , GameProperties
    , Move
    , Outcome(..)
    , Popularities
    , PopularityItem
    , outcomeFromString
    )

import Array exposing (Array)
import Date exposing (Date)


type Outcome
    = WhiteWon
    | BlackWon
    | Draw


outcomeFromString : String -> Maybe Outcome
outcomeFromString str =
    case str of
        "White won" ->
            Just WhiteWon

        "Black won" ->
            Just BlackWon

        "Draw" ->
            Just Draw

        _ ->
            Nothing


type alias GameProperties =
    { id : Int
    , white : String
    , black : String
    , whiteElo : Int
    , blackElo : Int
    , event : String
    , site : String
    , date : Date
    , round : String
    , result : Outcome
    , eco : String
    }


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
