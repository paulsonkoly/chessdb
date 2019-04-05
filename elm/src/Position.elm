module Position exposing (Position, makeMove)

import Bitwise as Bit
import Board
    exposing
        ( Board
        , Colour(..)
        , File(..)
        , Kind(..)
        , Move(..)
        , Piece(..)
        , Rank(..)
        , Square(..)
        )
import State


type alias Position =
    { board : Board
    , castlingAvailability : Int
    , activeColour : Colour
    , enPassant : Maybe Int
    }


makeMove : Move -> Position -> Result String Position
makeMove move position =
    case move of
        Normal King Nothing _ destination nothing ->
            let
                src =
                    sourceSquare move position

                pc =
                    Piece position.activeColour King

                upd sq =
                    position.board
                        |> Board.putPiece destination (Just pc)
                        |> Board.putPiece sq Nothing

                castle =
                    case position.activeColour of
                        White ->
                            Bit.and position.castlingAvailability 12

                        Black ->
                            Bit.and position.castlingAvailability 3
            in
            src
                |> Result.map
                    (\sq ->
                        { position
                            | board = upd sq
                            , castlingAvailability = castle
                        }
                    )

        _ ->
            Err "Not implemented yet"


sourceSquare : Move -> Position -> Result String Square
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
updateCastlingAvailability move position =
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
    { position
        | castlingAvailability =
            Bit.and position.castlingAvailability clearMask
    }


updateActiveColour : Move -> Position -> Position
updateActiveColour _ position =
    { position | activeColour = Board.flip position.activeColour }



{-
   updateEnPassant : Move -> Position -> Position
   updateEnPassant : Move (Position position) =
       case move of
           Move Pawn
           ()
-}
