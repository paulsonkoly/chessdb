module Game exposing
    ( Game
    , GameProperties
    , Move
    , Outcome(..)
    , Popularities
    , PopularityItem
    , outcomeFromString
    , outcomeToString
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
        "1-0" ->
            Just WhiteWon

        "0-1" ->
            Just BlackWon

        "1/2-1/2" ->
            Just Draw

        _ ->
            Nothing


outcomeToString : Outcome -> String
outcomeToString outcome =
    case outcome of
        WhiteWon ->
            "1-0"

        BlackWon ->
            "0-1"

        Draw ->
            "1/2-1/2"


type alias GameProperties =
    { id : Int
    , white : String
    , black : String
    , whiteElo : Int
    , blackElo : Int
    , event : String
    , site : String
    , date : Maybe Date
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
    { properties : GameProperties
    , moves : Array Move
    }


type alias Popularities =
    { token : Int
    , items : List PopularityItem
    }


type alias PopularityItem =
    { nextSan : String
    , whiteWon : Int
    , blackWon : Int
    , draw : Int
    , totalCount : Int
    }
