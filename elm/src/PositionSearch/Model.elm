module PositionSearch.Model exposing (Model)

import FormError exposing (Error)
import Position exposing (Position)


type alias Model =
    { position : Position
    , enPassantStringError : Error
    }
