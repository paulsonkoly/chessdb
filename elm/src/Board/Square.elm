module Board.Square exposing
    ( File(..)
    , Rank(..)
    , Square(..)
    , a1
    , a2
    , a3
    , a4
    , a5
    , a6
    , a7
    , a8
    , b1
    , b2
    , b3
    , b4
    , b5
    , b6
    , b7
    , b8
    , c1
    , c2
    , c3
    , c4
    , c5
    , c6
    , c7
    , c8
    , d1
    , d2
    , d3
    , d4
    , d5
    , d6
    , d7
    , d8
    , e1
    , e2
    , e3
    , e4
    , e5
    , e6
    , e7
    , e8
    , f1
    , f2
    , f3
    , f4
    , f5
    , f6
    , f7
    , f8
    , file
    , fileA
    , fileB
    , fileC
    , fileD
    , fileE
    , fileF
    , fileG
    , fileH
    , fileParser
    , fromEncoded
    , g1
    , g2
    , g3
    , g4
    , g5
    , g6
    , g7
    , g8
    , h1
    , h2
    , h3
    , h4
    , h5
    , h6
    , h7
    , h8
    , hDist
    , offsetBy
    , parser
    , rank
    , rankParser
    , square
    , toUrlQueryParameter
    , vDist
    )

import Parser exposing ((|.), (|=), Parser)
import Url.Builder as Url



------------------------------------------------------------------------
--                               Types                                --
------------------------------------------------------------------------


type File
    = File Int


type Rank
    = Rank Int


type Square
    = Square Int



------------------------------------------------------------------------
--                               Values                               --
------------------------------------------------------------------------


fileA =
    File 0


fileB =
    File 1


fileC =
    File 2


fileD =
    File 3


fileE =
    File 4


fileF =
    File 5


fileG =
    File 6


fileH =
    File 7


a8 =
    Square 0


b8 =
    Square 1


c8 =
    Square 2


d8 =
    Square 3


e8 =
    Square 4


f8 =
    Square 5


g8 =
    Square 6


h8 =
    Square 7


a7 =
    Square 8


b7 =
    Square 9


c7 =
    Square 10


d7 =
    Square 11


e7 =
    Square 12


f7 =
    Square 13


g7 =
    Square 14


h7 =
    Square 15


a6 =
    Square 16


b6 =
    Square 17


c6 =
    Square 18


d6 =
    Square 19


e6 =
    Square 20


f6 =
    Square 21


g6 =
    Square 22


h6 =
    Square 23


a5 =
    Square 24


b5 =
    Square 25


c5 =
    Square 26


d5 =
    Square 27


e5 =
    Square 28


f5 =
    Square 29


g5 =
    Square 30


h5 =
    Square 31


a4 =
    Square 32


b4 =
    Square 33


c4 =
    Square 34


d4 =
    Square 35


e4 =
    Square 36


f4 =
    Square 37


g4 =
    Square 38


h4 =
    Square 39


a3 =
    Square 40


b3 =
    Square 41


c3 =
    Square 42


d3 =
    Square 43


e3 =
    Square 44


f3 =
    Square 45


g3 =
    Square 46


h3 =
    Square 47


a2 =
    Square 48


b2 =
    Square 49


c2 =
    Square 50


d2 =
    Square 51


e2 =
    Square 52


f2 =
    Square 53


g2 =
    Square 54


h2 =
    Square 55


a1 =
    Square 56


b1 =
    Square 57


c1 =
    Square 58


d1 =
    Square 59


e1 =
    Square 60


f1 =
    Square 61


g1 =
    Square 62


h1 =
    Square 63



------------------------------------------------------------------------
--                              Parsers                               --
------------------------------------------------------------------------


fileParser : Parser File
fileParser =
    Parser.oneOf
        [ Parser.succeed fileA |. Parser.token "a"
        , Parser.succeed fileB |. Parser.token "b"
        , Parser.succeed fileC |. Parser.token "c"
        , Parser.succeed fileD |. Parser.token "d"
        , Parser.succeed fileE |. Parser.token "e"
        , Parser.succeed fileF |. Parser.token "f"
        , Parser.succeed fileG |. Parser.token "g"
        , Parser.succeed fileH |. Parser.token "h"
        ]


rankParser : Parser Rank
rankParser =
    Parser.int
        |> Parser.andThen
            (\r ->
                if 1 <= r && r <= 8 then
                    Parser.succeed (Rank r)

                else
                    Parser.problem
                        ("Rank should be between 1 and 8 (was "
                            ++ String.fromInt r
                            ++ ")"
                        )
            )


parser : Parser Square
parser =
    Parser.succeed square
        |= fileParser
        |= rankParser



------------------------------------------------------------------------
--                            Transformers                            --
------------------------------------------------------------------------


transform : Int -> Int
transform ix =
    (7 - (ix // 8)) * 8 + modBy 8 ix


toUrlQueryParameter : String -> Square -> Url.QueryParameter
toUrlQueryParameter str (Square ix) =
    Url.int str (transform ix)


fromEncoded : Int -> Result String Square
fromEncoded ix =
    if ix >= 0 && ix < 64 then
        Ok (Square (transform ix))

    else
        Err ("Square encoding is out of range " ++ String.fromInt ix)



------------------------------------------------------------------------
--                         Data manipulation                          --
------------------------------------------------------------------------


offsetBy : Int -> Square -> Maybe Square
offsetBy d (Square ix) =
    let
        dest =
            ix + d
    in
    if 0 <= dest && dest < 64 then
        Just (Square dest)

    else
        Nothing


file : Square -> File
file (Square ix) =
    File (modBy 8 ix)


rank : Square -> Rank
rank (Square ix) =
    Rank (8 - (ix // 8))


hDist : File -> File -> Int
hDist (File x) (File y) =
    abs (x - y)


vDist : Rank -> Rank -> Int
vDist (Rank x) (Rank y) =
    abs (x - y)


square : File -> Rank -> Square
square (File f) (Rank r) =
    Square (8 * (8 - r) + f)
