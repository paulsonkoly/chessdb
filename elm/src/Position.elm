module Position exposing (Position, makeMove)

import Bitwise as Bit
import Board
    exposing
        ( Board
        , Castle(..)
        , Colour(..)
        , Disambiguity(..)
        , Kind(..)
        , Move(..)
        , Piece(..)
        )
import Board.Square as Board exposing (File(..), Rank(..), Square(..))
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
                rSource =
                    sourceSquare move position

                pc =
                    Piece position.activeColour kind

                upd sq =
                    position.board
                        |> Board.putPiece destination (Just pc)
                        |> Board.putPiece sq Nothing

                castle source =
                    updateCastlingAvailability
                        source
                        destination
                        position.castlingAvailability
            in
            rSource
                |> Result.map
                    (\source ->
                        { position
                            | board = upd source
                            , castlingAvailability = castle source
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


updateCastlingAvailability : Square -> Square -> Int -> Int
updateCastlingAvailability source destination castling =
    let
        a1Mask =
            if source == Board.a1 || destination == Board.a1 then
                2

            else
                0

        e1Mask =
            if source == Board.e1 || destination == Board.e1 then
                3

            else
                0

        h1Mask =
            if source == Board.h1 || destination == Board.h1 then
                1

            else
                0

        a8Mask =
            if source == Board.a8 || destination == Board.a8 then
                8

            else
                0

        e8Mask =
            if source == Board.e8 || destination == Board.e8 then
                12

            else
                0

        h8Mask =
            if source == Board.h8 || destination == Board.h8 then
                4

            else
                0

        mask =
            Bit.or a1Mask e1Mask
                |> Bit.or h1Mask
                |> Bit.or a8Mask
                |> Bit.or e8Mask
                |> Bit.or h8Mask
    in
    Bit.and (Bit.complement mask) castling


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
