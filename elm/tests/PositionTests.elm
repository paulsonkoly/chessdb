module PositionTests exposing (suite)

import Board exposing (..)
import Board.Square exposing (..)
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
                            makeMove
                                (Normal Knight disambiguity False c3 Nothing)
                                knightsPos
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
            , test "stops at pieces with ray casting" <|
                \_ ->
                    let
                        rayBoard =
                            emptyBoard
                                |> putPiece a1 wb
                                |> putPiece c3 (Just (Piece White King))

                        fromA1 =
                            makeMove (Normal Bishop Nothing False e5 Nothing)
                                { pos | board = rayBoard }
                    in
                    Expect.equal
                        (Result.map (.board >> get e5) fromA1)
                        (Err "Bishop not found")
            , test "disambiguates correctly" <|
                \_ ->
                    let
                        rayBoard =
                            emptyBoard
                                |> putPiece a1 wb
                                |> putPiece h8 wb

                        disambiguity =
                            Just (FileDisambiguity fileA)

                        fromA1 =
                            makeMove (Normal Bishop disambiguity False e5 Nothing)
                                { pos | board = rayBoard }
                    in
                    Expect.equal
                        (Result.map (.board >> get a1) fromA1)
                        (Ok Nothing)
            ]
        , let
            wr =
                Just (Piece White Rook)

            b =
                emptyBoard
                    |> putPiece d3 wr

            pos =
                { board = b
                , castlingAvailability = 15
                , activeColour = White
                , enPassant = Nothing
                }

            move kind to =
                makeMove (Normal kind Nothing False to Nothing) pos
          in
          describe "rook moves"
            [ test "removes the rook from where it came from" <|
                \_ ->
                    Expect.equal
                        (Result.map (.board >> get d3) (move Rook f3))
                        (Ok Nothing)
            , test "puts the rook on the new square" <|
                \_ ->
                    Expect.equal
                        (Result.map (.board >> get f3) (move Rook f3))
                        (Ok wr)
            , test "reports Rook not found if rook is not there" <|
                \_ ->
                    Expect.equal (move Rook e4) (Err "Rook not found")
            , test "stops at pieces with ray casting" <|
                \_ ->
                    let
                        rayBoard =
                            emptyBoard
                                |> putPiece d3 wr
                                |> putPiece e3 (Just (Piece White King))

                        fromD3 =
                            makeMove (Normal Rook Nothing False f3 Nothing)
                                { pos | board = rayBoard }
                    in
                    Expect.equal
                        (Result.map (.board >> get f3) fromD3)
                        (Err "Rook not found")
            , test "disambiguates correctly" <|
                \_ ->
                    let
                        rayBoard =
                            emptyBoard
                                |> putPiece a2 wr
                                |> putPiece h2 wr

                        disambiguity =
                            Just (FileDisambiguity fileA)

                        fromA2 =
                            makeMove (Normal Rook disambiguity False e2 Nothing)
                                { pos | board = rayBoard }
                    in
                    Expect.equal
                        (Result.map (.board >> get a2) fromA2)
                        (Ok Nothing)
            ]
        , let
            wq =
                Just (Piece White Queen)

            b =
                emptyBoard |> putPiece d3 wq

            pos =
                { board = b
                , castlingAvailability = 15
                , activeColour = White
                , enPassant = Nothing
                }

            move kind to =
                makeMove (Normal kind Nothing False to Nothing) pos
          in
          describe "queen moves"
            [ test "can do bishop moves" <|
                \_ ->
                    Expect.equal
                        (Result.map (.board >> get g6) (move Queen g6))
                        (Ok wq)
            , test "can do rook moves" <|
                \_ ->
                    Expect.equal
                        (Result.map (.board >> get f3) (move Queen f3))
                        (Ok wq)
            ]
        , let
            wr =
                Just (Piece White Rook)

            wk =
                Just (Piece White King)

            br =
                Just (Piece Black Rook)

            bk =
                Just (Piece Black King)

            wn =
                Just (Piece White Knight)

            bn =
                Just (Piece Black Knight)

            b =
                emptyBoard
                    |> putPiece a1 wr
                    |> putPiece e1 wk
                    |> putPiece h1 wr
                    |> putPiece b3 bn
                    |> putPiece g3 bn
                    |> putPiece b6 wn
                    |> putPiece g6 wn
                    |> putPiece a8 br
                    |> putPiece e8 bk
                    |> putPiece h8 br

            position =
                { board = b
                , castlingAvailability = 15
                , activeColour = White
                , enPassant = Nothing
                }

            move colour kind to =
                makeMove (Normal kind Nothing False to Nothing)
                    { position | activeColour = colour }

            capture colour kind to =
                makeMove (Normal kind Nothing True to Nothing)
                    { position | activeColour = colour }
          in
          describe "castling right changes"
            [ test "a move from a1 removes whites long castle" <|
                \_ ->
                    Expect.equal
                        (Result.map .castlingAvailability (move White Rook c1))
                        (Ok 13)
            , test "a move to a1 removes whites long castle" <|
                \_ ->
                    Expect.equal
                        (Result.map .castlingAvailability (capture Black Knight a1))
                        (Ok 13)
            , test "a move from e1 removes whites all castles" <|
                \_ ->
                    Expect.equal
                        (Result.map .castlingAvailability (move White King e2))
                        (Ok 12)
            , test "a move from h1 removes whites short castle" <|
                \_ ->
                    Expect.equal
                        (Result.map .castlingAvailability (move White Rook g1))
                        (Ok 14)
            , test "a move to h1 removes whites short castle" <|
                \_ ->
                    Expect.equal
                        (Result.map .castlingAvailability (capture Black Knight h1))
                        (Ok 14)
            , test "a move from a8 removes blacks long castle" <|
                \_ ->
                    Expect.equal
                        (Result.map .castlingAvailability (move Black Rook c8))
                        (Ok 7)
            , test "a move to a8 removes blacks long castle" <|
                \_ ->
                    Expect.equal
                        (Result.map .castlingAvailability (capture White Knight a8))
                        (Ok 7)
            , test "a move from e8 removes blacks all castles" <|
                \_ ->
                    Expect.equal
                        (Result.map .castlingAvailability (move Black King e7))
                        (Ok 3)
            , test "a move from h8 removes blacks short castle" <|
                \_ ->
                    Expect.equal
                        (Result.map .castlingAvailability (move Black Rook g8))
                        (Ok 11)
            , test "a move to h8 removes blacks short castle" <|
                \_ ->
                    Expect.equal
                        (Result.map .castlingAvailability (capture White Knight h8))
                        (Ok 11)
            , test "a move from h1 to h8 removes both sides short castle" <|
                \_ ->
                    Expect.equal
                        (Result.map .castlingAvailability (capture White Rook h8))
                        (Ok 10)
            ]
        ]
