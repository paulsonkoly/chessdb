module Position exposing (Position)

import Bitwise as Bit
import Board
    exposing
        ( Board
        , Colour(..)
        , File(..)
        , Kind(..)
        , Move(..)
        , Rank(..)
        , Square(..)
        )
import State


type Position
    = Position Record


type alias Record =
    { board : Board
    , castlingAvailability : Int
    , activeColour : Colour
    , enPassant : Maybe Int
    }


sourceSquare : Move -> Record -> Result String Square
sourceSquare move position =
    case move of
        Castle _ ->
            case position.activeColour of
                White ->
                    Ok Board.e1

                Black ->
                    Ok Board.e8

        Normal King Nothing _ destination Nothing ->
            let
                piece =
                    Board.Piece position.activeColour King

                msource =
                    Board.run
                        (Board.kingScanner position.board
                            (\b ix ->
                                Board.get ix b == Just piece
                            )
                            destination
                        )
            in
            Result.fromMaybe "King not found" msource

        _ ->
            Err "Unexpected move case"


updateCastlingAvailability : Move -> Position -> Position
updateCastlingAvailability move (Position position) =
    let
        colourVal =
            case position.activeColour of
                White ->
                    0

                Black ->
                    1

        mask =
            Bit.complement (Bit.shiftLeftBy (colourVal * 2) 3)

        clearMask =
            Bit.complement <|
                case move of
                    Castle _ ->
                        mask

                    Normal King _ _ _ _ ->
                        mask

                    Normal Rook _ _ _ _ ->
                        case
                            ( sourceSquare move position
                            , position.activeColour
                            )
                        of
                            h1 ->
                                1

                    -- a1 ->
                    --     2
                    -- h8 ->
                    --     4
                    -- a8 ->
                    --     8
                    -- _ ->
                    --     0
                    _ ->
                        0
    in
    Position
        { position
            | castlingAvailability =
                Bit.and position.castlingAvailability clearMask
        }


updateActiveColour : Move -> Position -> Position
updateActiveColour _ (Position position) =
    Position
        { position | activeColour = Board.flip position.activeColour }



{-
   updateEnPassant : Move -> Position -> Position
   updateEnPassant : Move (Position position) =
       case move of
           Move Pawn
           ()
-}
