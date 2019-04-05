module Position exposing (Position, makeMove)

import Bitwise as Bit
import Board
    exposing
        ( Board
        , Castle(..)
        , Colour(..)
        , Disambiguity(..)
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
        Castle kind ->
            let
                ( rank, castleMask ) =
                    case position.activeColour of
                        White ->
                            ( Rank 1, 12 )

                        Black ->
                            ( Rank 8, 3 )

                ( kToFile, rFromFile, rToFile ) =
                    case kind of
                        Short ->
                            ( Board.fileG, Board.fileH, Board.fileF )

                        Long ->
                            ( Board.fileC, Board.fileA, Board.fileD )

                king =
                    Just (Piece position.activeColour King)

                rook =
                    Just (Piece position.activeColour Rook)

                castle =
                    Bit.and position.castlingAvailability castleMask

                newB =
                    position.board
                        |> Board.putPiece (Board.square Board.fileE rank) Nothing
                        |> Board.putPiece (Board.square rFromFile rank) Nothing
                        |> Board.putPiece (Board.square kToFile rank) king
                        |> Board.putPiece (Board.square rToFile rank) rook
            in
            Ok { position | board = newB, castlingAvailability = castle }

        Normal King Nothing _ destination Nothing ->
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

        Normal kind disambiguity _ destination Nothing ->
            let
                src =
                    sourceSquare move position

                pc =
                    Piece position.activeColour kind

                upd sq =
                    position.board
                        |> Board.putPiece destination (Just pc)
                        |> Board.putPiece sq Nothing
            in
            src
                |> Result.map (\sq -> { position | board = upd sq })

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

        Normal kind disambiguity _ destination Nothing ->
            let
                piece =
                    Board.Piece position.activeColour kind

                condition b ix =
                    case disambiguity of
                        Just (FileDisambiguity fd) ->
                            Board.get ix b == Just piece && Board.file ix == fd

                        Just (RankDisambiguity rd) ->
                            Board.get ix b == Just piece && Board.rank ix == rd

                        Nothing ->
                            Board.get ix b == Just piece

                scan scanner =
                    Board.run (scanner position.board condition destination)
            in
            case kind of
                Knight ->
                    scan Board.knightScanner
                        |> Result.fromMaybe "Knight not found"

                Bishop ->
                    scan Board.bishopScanner
                        |> Result.fromMaybe "Bishop not found"

                Queen ->
                    scan Board.queenScanner
                        |> Result.fromMaybe "Queen not found"

                Rook ->
                    scan Board.rookScanner
                        |> Result.fromMaybe "Rook not found"

                _ ->
                    Err "Unexpected piece kind"

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
