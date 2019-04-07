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
          in
          describe "king moves"
            [ test "removes the king from where it came from" <|
                \_ ->
                    Expect.equal
                        (Result.map
                            (.board >> get e1)
                            (make (move King e2) pos)
                        )
                        (Ok Nothing)
            , test "puts the king on the new square" <|
                \_ ->
                    Expect.equal
                        (Result.map
                            (.board >> get f2)
                            (make (move King f2) pos)
                        )
                        (Ok wk)
            , test "reports king not found if king is not there" <|
                \_ ->
                    Expect.equal
                        (make (move King a1) pos)
                        (Err "King not found")
            ]
        , let
            wp =
                Just (Piece White Pawn)

            wq =
                Just (Piece White Queen)

            bp =
                Just (Piece Black Pawn)

            b =
                emptyBoard
                    |> putPiece a2 wp
                    |> putPiece b3 wp
                    |> putPiece c4 wp
                    |> putPiece d7 wp
                    |> putPiece d5 bp
                    |> putPiece g5 bp
                    |> putPiece e4 wp
                    |> putPiece h4 wp

            pos =
                { board = b
                , castlingAvailability = 15
                , activeColour = White
                , enPassant = Nothing
                }
          in
          describe "pawn move"
            [ test "removes the pawn from where it came from" <|
                \_ ->
                    Expect.equal
                        (Result.map
                            (.board >> get a2)
                            (make (move Pawn a3) pos)
                        )
                        (Ok Nothing)
            , test "puts the pawn on the new square" <|
                \_ ->
                    Expect.equal
                        (Result.map
                            (.board >> get b4)
                            (make (move Pawn b4) pos)
                        )
                        (Ok wp)
            , test "puts the promotion piece when needed" <|
                \_ ->
                    Expect.equal
                        (Result.map
                            (.board >> get d8)
                            (make (promote Queen d8) pos)
                        )
                        (Ok wq)
            , test "can make double pawn advances from the right squares" <|
                \_ ->
                    Expect.equal
                        (Result.map
                            (.board >> get a4)
                            (make (move Pawn a4) pos)
                        )
                        (Ok wp)
            , test "double advances set the en passant square" <|
                \_ ->
                    Expect.equal
                        (Result.map
                            .enPassant
                            (make (move Pawn a4) pos)
                        )
                        (Ok (Just a3))
            , test "can't make double pawn advances from the wrong squares" <|
                \_ ->
                    Expect.err
                        (Result.map
                            (.board >> get b5)
                            (make (move Pawn b5) pos)
                        )
            , test "reports Pawn not found if pawn is not there" <|
                \_ ->
                    Expect.equal
                        (make (move Pawn h4) pos)
                        (Err "Pawn not found")
            , test "captures sideways (c4xd5)" <|
                \_ ->
                    Expect.equal
                        (Result.map
                            (.board >> get d5)
                            (make (capture Pawn d5) pos)
                        )
                        (Ok wp)
            , test "captures sideways disambiuguates (f4xg5)" <|
                \_ ->
                    Expect.equal
                        (Result.map
                            (.board >> get d5)
                            (make (capture Pawn d5) pos)
                        )
                        (Ok wp)
            , test "captures sideways disambiuguates (h4xg5)" <|
                \_ ->
                    Expect.equal
                        (Result.map
                            (.board >> get g5)
                            (make (capture Pawn g5) pos)
                        )
                        (Ok wp)
            , todo "en passant capture removes the opponents pawn"
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
          in
          describe "knight moves"
            [ test "removes the knight from where it came from" <|
                \_ ->
                    Expect.equal
                        (Result.map
                            (.board >> get b1)
                            (make (move Knight c3) pos)
                        )
                        (Ok Nothing)
            , test "puts the Knights on the new square" <|
                \_ ->
                    Expect.equal
                        (Result.map
                            (.board >> get c3)
                            (make (move Knight c3) pos)
                        )
                        (Ok wn)
            , test "reports knights not found if knight is not there" <|
                \_ ->
                    Expect.equal
                        (make (move Knight d3) pos)
                        (Err "Knight not found")
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

                        knightMove =
                            move Knight c3
                                |> disambiguate (RankDisambiguity (Rank 1))
                    in
                    Expect.equal
                        (Result.map
                            (.board >> get b1)
                            (make knightMove knightsPos)
                        )
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
                make (Castle kind) (pos colour)
          in
          describe "castles"
            [ describe "white short"
                [ test "removes the king from e1" <|
                    \_ ->
                        Expect.equal
                            (Result.map
                                (.board >> get e1)
                                (castle Short White)
                            )
                            (Ok Nothing)
                , test "puts the king to g1" <|
                    \_ ->
                        Expect.equal
                            (Result.map
                                (.board >> get g1)
                                (castle Short White)
                            )
                            (Ok wk)
                , test "removes the rook from h1" <|
                    \_ ->
                        Expect.equal
                            (Result.map
                                (.board >> get h1)
                                (castle Short White)
                            )
                            (Ok Nothing)
                , test "puts the rook to f1" <|
                    \_ ->
                        Expect.equal
                            (Result.map
                                (.board >> get f1)
                                (castle Short White)
                            )
                            (Ok wr)
                , test "changes the castling rights" <|
                    \_ ->
                        Expect.equal
                            (Result.map
                                .castlingAvailability
                                (castle Short White)
                            )
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
                            (Result.map
                                .castlingAvailability
                                (castle Long White)
                            )
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
                            (Result.map
                                .castlingAvailability
                                (castle Short Black)
                            )
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
                            (Result.map
                                .castlingAvailability
                                (castle Long Black)
                            )
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
          in
          describe "bishop moves"
            [ test "removes the bishop from where it came from" <|
                \_ ->
                    Expect.equal
                        (Result.map
                            (.board >> get d3)
                            (make (move Bishop g6) pos)
                        )
                        (Ok Nothing)
            , test "puts the bishop on the new square" <|
                \_ ->
                    Expect.equal
                        (Result.map
                            (.board >> get g6)
                            (make (move Bishop g6) pos)
                        )
                        (Ok wb)
            , test "reports Bishop not found if bishop is not there" <|
                \_ ->
                    Expect.equal
                        (make (move Bishop f6) pos)
                        (Err "Bishop not found")
            , test "stops at pieces with ray casting" <|
                \_ ->
                    let
                        rayBoard =
                            emptyBoard
                                |> putPiece a1 wb
                                |> putPiece c3 (Just (Piece White King))

                        fromA1 =
                            make (move Bishop e5)
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

                        bishopMove =
                            move Bishop e5
                                |> disambiguate (FileDisambiguity fileA)

                        bishopPos =
                            { pos | board = rayBoard }
                    in
                    Expect.equal
                        (Result.map
                            (.board >> get a1)
                            (make bishopMove bishopPos)
                        )
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
          in
          describe "rook moves"
            [ test "removes the rook from where it came from" <|
                \_ ->
                    Expect.equal
                        (Result.map
                            (.board >> get d3)
                            (make (move Rook f3) pos)
                        )
                        (Ok Nothing)
            , test "puts the rook on the new square" <|
                \_ ->
                    Expect.equal
                        (Result.map
                            (.board >> get f3)
                            (make (move Rook f3) pos)
                        )
                        (Ok wr)
            , test "reports Rook not found if rook is not there" <|
                \_ ->
                    Expect.equal
                        (make (move Rook e4) pos)
                        (Err "Rook not found")
            , test "stops at pieces with ray casting" <|
                \_ ->
                    let
                        rayBoard =
                            emptyBoard
                                |> putPiece d3 wr
                                |> putPiece e3 (Just (Piece White King))

                        fromD3 =
                            make (move Rook f3)
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

                        rookMove =
                            move Rook e2
                                |> disambiguate (FileDisambiguity fileA)

                        rookPos =
                            { pos | board = rayBoard }
                    in
                    Expect.equal
                        (Result.map (.board >> get a2) (make rookMove rookPos))
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
          in
          describe "queen moves"
            [ test "can do bishop moves" <|
                \_ ->
                    Expect.equal
                        (Result.map
                            (.board >> get g6)
                            (make (move Queen g6) pos)
                        )
                        (Ok wq)
            , test "can do rook moves" <|
                \_ ->
                    Expect.equal
                        (Result.map
                            (.board >> get f3)
                            (make (move Queen f3) pos)
                        )
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

            moveBy colour kind to =
                make (move kind to) { position | activeColour = colour }

            captureWith colour kind to =
                make (capture kind to) { position | activeColour = colour }
          in
          describe "castling right changes"
            [ test "a move from a1 removes whites long castle" <|
                \_ ->
                    Expect.equal
                        (Result.map
                            .castlingAvailability
                            (moveBy White Rook c1)
                        )
                        (Ok 13)
            , test "a move to a1 removes whites long castle" <|
                \_ ->
                    Expect.equal
                        (Result.map
                            .castlingAvailability
                            (captureWith Black Knight a1)
                        )
                        (Ok 13)
            , test "a move from e1 removes whites all castles" <|
                \_ ->
                    Expect.equal
                        (Result.map
                            .castlingAvailability
                            (moveBy White King e2)
                        )
                        (Ok 12)
            , test "a move from h1 removes whites short castle" <|
                \_ ->
                    Expect.equal
                        (Result.map
                            .castlingAvailability
                            (moveBy White Rook g1)
                        )
                        (Ok 14)
            , test "a move to h1 removes whites short castle" <|
                \_ ->
                    Expect.equal
                        (Result.map
                            .castlingAvailability
                            (captureWith Black Knight h1)
                        )
                        (Ok 14)
            , test "a move from a8 removes blacks long castle" <|
                \_ ->
                    Expect.equal
                        (Result.map
                            .castlingAvailability
                            (moveBy Black Rook c8)
                        )
                        (Ok 7)
            , test "a move to a8 removes blacks long castle" <|
                \_ ->
                    Expect.equal
                        (Result.map
                            .castlingAvailability
                            (captureWith White Knight a8)
                        )
                        (Ok 7)
            , test "a move from e8 removes blacks all castles" <|
                \_ ->
                    Expect.equal
                        (Result.map
                            .castlingAvailability
                            (moveBy Black King e7)
                        )
                        (Ok 3)
            , test "a move from h8 removes blacks short castle" <|
                \_ ->
                    Expect.equal
                        (Result.map
                            .castlingAvailability
                            (moveBy Black Rook g8)
                        )
                        (Ok 11)
            , test "a move to h8 removes blacks short castle" <|
                \_ ->
                    Expect.equal
                        (Result.map
                            .castlingAvailability
                            (captureWith White Knight h8)
                        )
                        (Ok 11)
            , test "a move from h1 to h8 removes both sides short castle" <|
                \_ ->
                    Expect.equal
                        (Result.map
                            .castlingAvailability
                            (captureWith White Rook h8)
                        )
                        (Ok 10)
            ]
        ]
