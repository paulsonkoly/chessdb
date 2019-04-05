module PositionTests exposing (suite)

import Board exposing (..)
import Expect exposing (Expectation)
import Position exposing (..)
import Random
import Shrink
import Test exposing (..)


suite : Test
suite =
    let
        wk =
            Just (Piece White King)

        wn =
            Just (Piece White Knight)

        b =
            emptyBoard
                |> putPiece e3 wk
                |> putPiece e6 wk
                |> putPiece b1 wn

        pos =
            { board = b
            , castlingAvailability = 15
            , activeColour = White
            , enPassant = Nothing
            }

        move kind to =
            makeMove (Normal kind Nothing False to Nothing) pos
    in
    describe "Position"
        [ describe "king moves"
            [ test "removes the king from where it came from" <|
                \_ ->
                    Expect.equal
                        (Result.map (.board >> get e3) (move King e4))
                        (Ok Nothing)
            , test "puts the king on the new square" <|
                \_ ->
                    Expect.equal
                        (Result.map (.board >> get e4) (move King e4))
                        (Ok wk)
            , test "reports king not found if king is not there" <|
                \_ ->
                    Expect.equal (move King a1) (Err "King not found")
            , test "removes all castling rights for the moving side" <|
                \_ ->
                    Expect.equal
                        (Result.map .castlingAvailability (move King e4))
                        (Ok 12)
            ]
        , describe "knight moves"
            [ test "removes the knight from where it came from" <|
                \_ ->
                    Expect.equal
                        (Result.map (.board >> get b1) (move Knight c3))
                        (Ok Nothing)
            , test "puts the Knights on the new square" <|
                \_ ->
                    Expect.equal
                        (Result.map (.board >> get c3) (move Knight c3))
                        (Ok wn)
            , test "reports knights not found if knight is not there" <|
                \_ ->
                    Expect.equal (move Knight d3) (Err "Knight not found")
            , test "disambiguates correctly" <|
                \_ ->
                    let
                        knightsPos =
                            { pos
                                | board =
                                    pos.board
                                        |> putPiece d5 wn
                                        |> putPiece b5 wn
                                        |> putPiece a4 wn
                                        |> putPiece a4 wn
                            }

                        disambiguity =
                            Just (RankDisambiguity (Rank 1))

                        fromB1 =
                            makeMove (Normal Knight disambiguity False c3 Nothing) knightsPos
                    in
                    Expect.equal
                        (Result.map (.board >> get b1) fromB1)
                        (Ok Nothing)
            ]
        ]
