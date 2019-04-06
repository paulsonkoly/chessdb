module Position exposing (Position, make)

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
import Board.Scanner as Scanner
import Board.Square as Board exposing (File(..), Rank(..), Square(..))
import State


type alias Position =
    { board : Board
    , castlingAvailability : Int
    , activeColour : Colour
    , enPassant : Maybe Square
    }


make : Move -> Position -> Result String Position
make moveE position =
    let
        rSource =
            sourceSquare moveE position

        newBoard =
            pieces moveE position.activeColour position.board

        newCastle =
            castles moveE position.castlingAvailability

        newEnPassant =
            enPassant moveE position.activeColour
    in
    rSource
        |> Result.map
            (\source ->
                { position
                    | board = newBoard source
                    , castlingAvailability = newCastle source
                    , enPassant = newEnPassant source
                }
            )


pieces : Move -> Colour -> Board -> Square -> Board
pieces moveE colour board source =
    let
        movedPiece move =
            Piece colour <|
                Maybe.withDefault move.kind move.promotion
    in
    case ( colour, moveE ) of
        ( White, Castle Short ) ->
            board
                |> Board.putPiece Board.g1 (Just (Piece White King))
                |> Board.putPiece Board.f1 (Just (Piece White Rook))
                |> Board.putPiece Board.e1 Nothing
                |> Board.putPiece Board.h1 Nothing

        ( White, Castle Long ) ->
            board
                |> Board.putPiece Board.c1 (Just (Piece White King))
                |> Board.putPiece Board.d1 (Just (Piece White Rook))
                |> Board.putPiece Board.e1 Nothing
                |> Board.putPiece Board.a1 Nothing

        ( Black, Castle Short ) ->
            board
                |> Board.putPiece Board.g8 (Just (Piece Black King))
                |> Board.putPiece Board.f8 (Just (Piece Black Rook))
                |> Board.putPiece Board.e8 Nothing
                |> Board.putPiece Board.h8 Nothing

        ( Black, Castle Long ) ->
            board
                |> Board.putPiece Board.c8 (Just (Piece Black King))
                |> Board.putPiece Board.d8 (Just (Piece Black Rook))
                |> Board.putPiece Board.e8 Nothing
                |> Board.putPiece Board.a8 Nothing

        ( _, Normal move ) ->
            board
                |> Board.putPiece move.destination (Just (movedPiece move))
                |> Board.putPiece source Nothing


sourceSquare : Move -> Position -> Result String Square
sourceSquare moveE position =
    case moveE of
        Castle _ ->
            case position.activeColour of
                White ->
                    Ok Board.e1

                Black ->
                    Ok Board.e8

        Normal move ->
            let
                piece =
                    Board.Piece position.activeColour move.kind

                condition b ix =
                    case move.disambiguity of
                        Just (FileDisambiguity fd) ->
                            Board.get ix b == Just piece && Board.file ix == fd

                        Just (RankDisambiguity rd) ->
                            Board.get ix b == Just piece && Board.rank ix == rd

                        Nothing ->
                            Board.get ix b == Just piece

                scan scanner =
                    Scanner.run
                        (scanner position.board condition move.destination)
            in
            case move.kind of
                Knight ->
                    scan Scanner.knight
                        |> Result.fromMaybe "Knight not found"

                Bishop ->
                    scan Scanner.bishop
                        |> Result.fromMaybe "Bishop not found"

                Queen ->
                    scan Scanner.queen
                        |> Result.fromMaybe "Queen not found"

                Rook ->
                    scan Scanner.rook
                        |> Result.fromMaybe "Rook not found"

                King ->
                    scan Scanner.king
                        |> Result.fromMaybe "King not found"

                Pawn ->
                    scan (Scanner.pawn position.activeColour)
                        |> Result.fromMaybe "Pawn not found"


castles : Move -> Int -> Square -> Int
castles moveE castling source =
    let
        destination =
            case moveE of
                -- doesn't matter what, the source of the move will set no
                -- castling to the side
                Castle _ ->
                    Board.e4

                Normal move ->
                    move.destination

        a1Mask =
            if source == Board.a1 || destination == Board.a1 then
                2

            else
                0

        e1Mask =
            if source == Board.e1 then
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
            if source == Board.e8 then
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


enPassant : Move -> Colour -> Square -> Maybe Square
enPassant moveE colour source =
    case moveE of
        Castle _ ->
            Nothing

        Normal move ->
            case
                ( ( colour, move.kind )
                , Board.rank source
                , Board.rank move.destination
                )
            of
                ( ( White, Pawn ), Rank 2, Rank 4 ) ->
                    Board.offsetBy 8 move.destination

                ( ( Black, Pawn ), Rank 7, Rank 5 ) ->
                    Board.offsetBy -8 move.destination

                _ ->
                    Nothing
