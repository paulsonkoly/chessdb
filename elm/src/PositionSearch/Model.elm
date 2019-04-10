module PositionSearch.Model exposing (Model)

import Position exposing (Position)


type alias Model =
    { position : Position
    , enPassantStringError : Maybe String
    }
