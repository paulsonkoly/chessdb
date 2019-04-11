module PositionSearch.Msg exposing (Msg(..))

import Board exposing (Castle(..))
import Board.Colour exposing (Colour(..))
import Http
import Pagination
import PositionSearch.ServerResponse as ServerResponse exposing (ServerResponse)


type Msg
    = BoardFenChanged String
    | ActiveColourChecked Colour
    | CastleChecked Colour Castle Bool
    | EnPassantInputted String
    | SearchClicked
    | GamesReceived (Result Http.Error ServerResponse)
    | PaginationRequested Pagination.Msg
