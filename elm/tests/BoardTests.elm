module BoardTests exposing (suite)

import Board exposing (..)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string, tuple)
import Random
import Shrink
import Test exposing (..)


fsquare : Fuzzer Square
fsquare =
    let
        r =
            Random.map Rank (Random.int 0 7)

        f =
            Random.map File (Random.int 0 7)

        sq =
            Random.map2 Board.square f r
    in
    Fuzz.custom
        sq
        Shrink.noShrink


suite : Test
suite =
    describe "Board scanners"
        [ describe "kingScanner"
            [ test "it finds the king from neighbouring cells" <|
                \_ ->
                    let
                        b =
                            emptyBoard |> putPiece c2 (Piece White King)

                        p _ sq =
                            get sq b == Just (Piece White King)

                        s =
                            kingScanner b p d3
                    in
                    Expect.equal (Just c2) (run s)
            , test "it doesn't find king too far away" <|
                \_ ->
                    let
                        b =
                            emptyBoard |> putPiece c2 (Piece White King)

                        p _ sq =
                            get sq b == Just (Piece White King)

                        s =
                            kingScanner b p e3
                    in
                    Expect.equal Nothing (run s)
            , fuzz (tuple ( fsquare, fsquare )) "on random square" <|
                \( k, o ) ->
                    let
                        b =
                            emptyBoard |> putPiece k (Piece White King)

                        p _ sq =
                            get sq b == Just (Piece White King)

                        s =
                            kingScanner b p o

                        vd =
                            vDist (rank k) (rank o)

                        hd =
                            hDist (file k) (file o)
                    in
                    if vd <= 1 && hd <= 1 && k /= o then
                        Expect.equal (Just k) (run s)

                    else
                        Expect.equal Nothing (run s)
            ]
        ]
