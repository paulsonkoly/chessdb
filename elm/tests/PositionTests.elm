module PositionTests exposing (suite)

import Board exposing (..)
import Expect exposing (Expectation)
import Position exposing (..)
import Random
import Shrink
import Test exposing (..)


suite : Test
suite =
    describe "Position"
        [ let
            wk =
                Just (Piece White King)

            b =
                emptyBoard
                    |> putPiece e3 wk
                    |> putPiece e6 wk

            pos =
                { board = b
                , castlingAvailability = 0
                , activeColour = White
                , enPassant = Nothing
                }

            move sq =
                Normal King Nothing False sq Nothing

            newPos sq =
                makeMove (move sq) pos
          in
          describe "king moves"
            [ test "removes the king from where it came from" <|
                \_ ->
                    Expect.equal
                        (Result.map (.board >> get e3) (newPos e4))
                        (Ok Nothing)
            , test "it puts the king on the new square" <|
                \_ ->
                    Expect.equal
                        (Result.map (.board >> get e4) (newPos e4))
                        (Ok wk)
            , test "it reports king not found if king is not there" <|
                \_ ->
                    Expect.equal (newPos a1) (Err "King not found")
            ]
        ]
