module PositionSearch.Msg exposing (Msg(..))

import Board exposing (Castle(..))
import Board.Colour exposing (Colour(..))


type Msg
    = BoardFenChanged String
    | CastleChecked Colour Castle Bool
    | EnPassantInputted String
