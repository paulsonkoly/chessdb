module PositionTests exposing (suite)

import Board exposing (..)
import Expect exposing (Expectation)
import Position exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "Position"
        [ let
            wk =
                Just (Piece White King)

            b =
                emptyBoard
                    |> putPiece e1 wk
                    |> putPiece e8 wk

            pos =
                { board = b
                , castlingAvailability = 15
                , activeColour = White
                , enPassant = Nothing
                }

            move kind to =
                makeMove (Normal kind Nothing False to Nothing) pos
          in
          describe "king moves"
            [ test "removes the king from where it came from" <|
                \_ ->
                    Expect.equal
                        (Result.map (.board >> get e1) (move King e2))
                        (Ok Nothing)
            , test "puts the king on the new square" <|
                \_ ->
                    Expect.equal
                        (Result.map (.board >> get f2) (move King f2))
                        (Ok wk)
            , test "reports king not found if king is not there" <|
                \_ ->
                    Expect.equal (move King a1) (Err "King not found")
            , test "removes all castling rights for the moving side" <|
                \_ ->
                    Expect.equal
                        (Result.map .castlingAvailability (move King d1))
                        (Ok 12)
            ]
        , let
            wn =
                Just (Piece White Knight)

            b =
                emptyBoard
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
          describe "knight moves"
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
        , let
            wk =
                Just (Piece White King)

            wr =
                Just (Piece White Rook)

            bk =
                Just (Piece Black King)

            br =
                Just (Piece Black Rook)

            b =
                emptyBoard
                    |> putPiece e1 wk
                    |> putPiece e8 bk
                    |> putPiece a1 wr
                    |> putPiece h1 wr
                    |> putPiece a8 br
                    |> putPiece h8 br

            pos col =
                { board = b
                , castlingAvailability = 15
                , activeColour = col
                , enPassant = Nothing
                }

            castle kind colour =
                makeMove (Castle kind) (pos colour)
          in
          describe "castles"
            [ describe "white short"
                [ test "removes the king from e1" <|
                    \_ ->
                        Expect.equal
                            (Result.map (.board >> get e1) (castle Short White))
                            (Ok Nothing)
                , test "puts the king to g1" <|
                    \_ ->
                        Expect.equal
                            (Result.map (.board >> get g1) (castle Short White))
                            (Ok wk)
                , test "removes the rook from h1" <|
                    \_ ->
                        Expect.equal
                            (Result.map (.board >> get h1) (castle Short White))
                            (Ok Nothing)
                , test "puts the rook to f1" <|
                    \_ ->
                        Expect.equal
                            (Result.map (.board >> get f1) (castle Short White))
                            (Ok wr)
                , test "changes the castling rights" <|
                    \_ ->
                        Expect.equal
                            (Result.map .castlingAvailability (castle Short White))
                            (Ok 12)
                ]
            , describe "white long"
                [ test "removes the king from e1" <|
                    \_ ->
                        Expect.equal
                            (Result.map (.board >> get e1) (castle Long White))
                            (Ok Nothing)
                , test "puts the king to c1" <|
                    \_ ->
                        Expect.equal
                            (Result.map (.board >> get c1) (castle Long White))
                            (Ok wk)
                , test "removes the rook from a1" <|
                    \_ ->
                        Expect.equal
                            (Result.map (.board >> get a1) (castle Long White))
                            (Ok Nothing)
                , test "puts the rook to d1" <|
                    \_ ->
                        Expect.equal
                            (Result.map (.board >> get d1) (castle Long White))
                            (Ok wr)
                , test "changes the castling rights" <|
                    \_ ->
                        Expect.equal
                            (Result.map .castlingAvailability (castle Long White))
                            (Ok 12)
                ]
            , describe "black short"
                [ test "removes the king from e8" <|
                    \_ ->
                        Expect.equal
                            (Result.map (.board >> get e8) (castle Short Black))
                            (Ok Nothing)
                , test "puts the king to g8" <|
                    \_ ->
                        Expect.equal
                            (Result.map (.board >> get g8) (castle Short Black))
                            (Ok bk)
                , test "removes the rook from h8" <|
                    \_ ->
                        Expect.equal
                            (Result.map (.board >> get h8) (castle Short Black))
                            (Ok Nothing)
                , test "puts the rook to f8" <|
                    \_ ->
                        Expect.equal
                            (Result.map (.board >> get f8) (castle Short Black))
                            (Ok br)
                , test "changes the castling rights" <|
                    \_ ->
                        Expect.equal
                            (Result.map .castlingAvailability (castle Short Black))
                            (Ok 3)
                ]
            , describe "black long"
                [ test "removes the king from e8" <|
                    \_ ->
                        Expect.equal
                            (Result.map (.board >> get e8) (castle Long Black))
                            (Ok Nothing)
                , test "puts the king to c8" <|
                    \_ ->
                        Expect.equal
                            (Result.map (.board >> get c8) (castle Long Black))
                            (Ok bk)
                , test "removes the rook from a8" <|
                    \_ ->
                        Expect.equal
                            (Result.map (.board >> get a8) (castle Long Black))
                            (Ok Nothing)
                , test "puts the rook to d8" <|
                    \_ ->
                        Expect.equal
                            (Result.map (.board >> get d8) (castle Long Black))
                            (Ok br)
                , test "changes the castling rights" <|
                    \_ ->
                        Expect.equal
                            (Result.map .castlingAvailability (castle Long Black))
                            (Ok 3)
                ]
            ]
        , let
            wb =
                Just (Piece White Bishop)

            b =
                emptyBoard
                    |> putPiece d3 wb

            pos =
                { board = b
                , castlingAvailability = 15
                , activeColour = White
                , enPassant = Nothing
                }

            move kind to =
                makeMove (Normal kind Nothing False to Nothing) pos
          in
          describe "bishop moves"
            [ test "removes the bishop from where it came from" <|
                \_ ->
                    Expect.equal
                        (Result.map (.board >> get d3) (move Bishop g6))
                        (Ok Nothing)
            , test "puts the bishop on the new square" <|
                \_ ->
                    Expect.equal
                        (Result.map (.board >> get g6) (move Bishop g6))
                        (Ok wb)
            , test "reports Bishop not found if bishop is not there" <|
                \_ ->
                    Expect.equal (move Bishop f6) (Err "Bishop not found")
            ]
        ]
