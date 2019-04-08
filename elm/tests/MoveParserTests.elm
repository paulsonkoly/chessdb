module MoveParserTests exposing (suite)

import Board exposing (Kind(..), Move(..))
import Board.Square as Board
import Expect exposing (Expectation)
import Parser
import Test exposing (..)


suite : Test
suite =
    describe "Move parser"
        [ test "e4" <|
            \_ ->
                Expect.equal
                    (Parser.run Board.moveParser "e4")
                    (Ok
                        (Normal
                            { kind = Pawn
                            , disambiguity = Nothing
                            , capture = False
                            , destination = Board.e4
                            , promotion = Nothing
                            }
                        )
                    )
        , test "Nf3" <|
            \_ ->
                Expect.equal
                    (Parser.run Board.moveParser "Nf3")
                    (Ok
                        (Normal
                            { kind = Knight
                            , disambiguity = Nothing
                            , capture = False
                            , destination = Board.f3
                            , promotion = Nothing
                            }
                        )
                    )
        ]
