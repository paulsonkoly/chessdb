module BoardTests exposing (suite)

import Array
import Board exposing (Board, Kind(..), Piece(..))
import Board.Colour exposing (Colour(..))
import Board.Scanner as Scanner exposing (Scanner)
import Board.Square as Board exposing (File(..), Rank(..), Square)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string, tuple)
import Parser
import Random
import Shrink
import Test exposing (..)


fsquare : Fuzzer Square
fsquare =
    let
        r =
            Random.map Rank (Random.int 1 8)

        f =
            Random.map File (Random.int 0 7)

        sq =
            Random.map2 Board.square f r
    in
    Fuzz.custom
        sq
        Shrink.noShrink


fKind : Fuzzer Kind
fKind =
    Fuzz.oneOf <|
        List.map Fuzz.constant
            [ Pawn, Rook, Knight, Bishop, Queen, King ]


fColour : Fuzzer Colour
fColour =
    Fuzz.oneOf <| List.map Fuzz.constant [ White, Black ]


fPiece : Fuzzer Piece
fPiece =
    Fuzz.map2 Piece fColour fKind


fBoard : Fuzzer Board
fBoard =
    let
        put =
            Fuzz.tuple ( fsquare, fPiece )
                |> Fuzz.map
                    (\( square, piece ) -> Board.putPiece square (Just piece))
    in
    Fuzz.list put |> Fuzz.map (List.foldl (<|) Board.empty)


suite : Test
suite =
    describe "Board scanners"
        [ describe "kingScanner"
            [ test "it finds the king from neighbouring cells" <|
                \_ ->
                    let
                        b =
                            Board.empty
                                |> Board.putPiece Board.c2 (Just (Piece White King))

                        p _ sq =
                            Board.get sq b == Just (Piece White King)

                        s =
                            Scanner.king b p Board.d3
                    in
                    Expect.equal (Just Board.c2) (Scanner.run s)
            , test "it doesn't find king too far away" <|
                \_ ->
                    let
                        b =
                            Board.empty
                                |> Board.putPiece Board.c2 (Just (Piece White King))

                        p _ sq =
                            Board.get sq b == Just (Piece White King)

                        s =
                            Scanner.king b p Board.e3
                    in
                    Expect.equal Nothing (Scanner.run s)
            , fuzz (tuple ( fsquare, fsquare )) "on random square" <|
                \( k, o ) ->
                    let
                        b =
                            Board.empty
                                |> Board.putPiece k (Just (Piece White King))

                        p _ sq =
                            Board.get sq b == Just (Piece White King)

                        s =
                            Scanner.king b p o

                        vd =
                            Board.vDist (Board.rank k) (Board.rank o)

                        hd =
                            Board.hDist (Board.file k) (Board.file o)
                    in
                    if vd <= 1 && hd <= 1 && k /= o then
                        Expect.equal (Just k) (Scanner.run s)

                    else
                        Expect.equal Nothing (Scanner.run s)
            , fuzz fBoard "random board -> toFen -> parser -> same board" <|
                \board ->
                    let
                        fen =
                            Board.toFen board

                        parsed =
                            Parser.run Board.boardParser fen
                    in
                    Expect.equal (Ok board) parsed
            ]
        ]
