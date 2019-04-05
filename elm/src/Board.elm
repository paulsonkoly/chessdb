module Board exposing
    ( Board
    , Colour(..)
    , File(..)
    , Kind(..)
    , Move(..)
    , Piece(..)
    , Rank(..)
    , Square
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
    , boardParser
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
    , emptyBoard
    , f1
    , f2
    , f3
    , f4
    , f5
    , f6
    , f7
    , f8
    , file
    , flip
    , g1
    , g2
    , g3
    , g4
    , g5
    , g6
    , g7
    , g8
    , get
    , h1
    , h2
    , h3
    , h4
    , h5
    , h6
    , h7
    , h8
    , hDist
    , kingScanner
    , knightScanner
    , moveParser
    , putPiece
    , rank
    , run
    , square
    , vDist
    )

import Array exposing (Array)
import Maybe.Extra as Maybe
import Parser exposing ((|.), (|=), Parser, Step)
import State exposing (State)


type Kind
    = Pawn
    | Rook
    | Knight
    | Bishop
    | Queen
    | King


kindParser : Parser Kind
kindParser =
    Parser.oneOf
        [ Parser.succeed Pawn |. Parser.token "P"
        , Parser.succeed Rook |. Parser.token "R"
        , Parser.succeed Knight |. Parser.token "N"
        , Parser.succeed Bishop |. Parser.token "B"
        , Parser.succeed Queen |. Parser.token "Q"
        , Parser.succeed King |. Parser.token "K"
        ]


type Colour
    = White
    | Black


flip : Colour -> Colour
flip colour =
    case colour of
        White ->
            Black

        Black ->
            White


type Piece
    = Piece Colour Kind


pieceParser : Parser Piece
pieceParser =
    Parser.oneOf
        [ Parser.succeed (Piece White Pawn) |. Parser.token "P"
        , Parser.succeed (Piece White Rook) |. Parser.token "R"
        , Parser.succeed (Piece White Knight) |. Parser.token "N"
        , Parser.succeed (Piece White Bishop) |. Parser.token "B"
        , Parser.succeed (Piece White Queen) |. Parser.token "Q"
        , Parser.succeed (Piece White King) |. Parser.token "K"
        , Parser.succeed (Piece Black Pawn) |. Parser.token "p"
        , Parser.succeed (Piece Black Rook) |. Parser.token "r"
        , Parser.succeed (Piece Black Knight) |. Parser.token "n"
        , Parser.succeed (Piece Black Bishop) |. Parser.token "b"
        , Parser.succeed (Piece Black Queen) |. Parser.token "q"
        , Parser.succeed (Piece Black King) |. Parser.token "k"
        ]


type Board
    = Board (Array (Maybe Piece))


emptyBoard : Board
emptyBoard =
    Board (Array.repeat 64 Nothing)


putPiece : Square -> Maybe Piece -> Board -> Board
putPiece (Square ix) piece (Board data) =
    Board (Array.set ix piece data)


get : Square -> Board -> Maybe Piece
get (Square ix) (Board board) =
    Maybe.join (Array.get ix board)


type Castle
    = Short
    | Long


castleParser : Parser Castle
castleParser =
    Parser.oneOf
        [ Parser.succeed Long |. Parser.token "O-O-O"
        , Parser.succeed Short |. Parser.token "O-O"
        ]


type File
    = File Int


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


type Rank
    = Rank Int


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


type Disambiguity
    = FileDisambiguity File
    | RankDisambiguity Rank


disambiguityParser : Parser Disambiguity
disambiguityParser =
    Parser.oneOf
        [ fileParser |> Parser.map FileDisambiguity
        , rankParser |> Parser.map RankDisambiguity
        ]


type Square
    = Square Int


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
    Rank (7 - (ix // 8))


hDist : File -> File -> Int
hDist (File x) (File y) =
    abs (x - y)


vDist : Rank -> Rank -> Int
vDist (Rank x) (Rank y) =
    abs (x - y)


square : File -> Rank -> Square
square (File f) (Rank r) =
    Square (8 * (7 - r) + f)


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


squareParser : Parser Square
squareParser =
    Parser.succeed square
        |= fileParser
        |= rankParser


type Move
    = Castle Castle
    | Normal Kind (Maybe Disambiguity) Bool Square (Maybe Kind)


moveParser : Parser Move
moveParser =
    Parser.oneOf
        [ castleParser
            |. Parser.end
            |> Parser.map Castle
        , normalParser
        ]


promotionParser : Parser (Maybe Kind)
promotionParser =
    Parser.oneOf
        [ Parser.succeed Just
            |. Parser.token "="
            |= kindParser
            |. Parser.end
        , Parser.succeed Nothing
            |. Parser.end
        ]


normalParser : Parser Move
normalParser =
    -- two main branches, 1.) starts with file followed by rank
    --                    2.) everything else (backtracked 1.)
    Parser.oneOf
        [ Parser.succeed
            (\f r promotion ->
                Normal Pawn Nothing False (square f r) promotion
            )
            |= Parser.backtrackable fileParser
            |= rankParser
            |= promotionParser
        , Parser.succeed Normal
            |= Parser.oneOf [ kindParser, Parser.succeed Pawn ]
            |= Parser.oneOf
                [ disambiguityParser |> Parser.map Just
                , Parser.succeed Nothing
                ]
            |= Parser.oneOf
                [ Parser.succeed True |. Parser.token "x"
                , Parser.succeed False
                ]
            |= squareParser
            |= promotionParser
        ]


type FenItem
    = FenPiece Piece
    | FenGap Rank


fenItemParser : Parser FenItem
fenItemParser =
    Parser.oneOf
        [ pieceParser |> Parser.map FenPiece
        , rankParser |> Parser.map FenGap
        ]


updateWithFenItem : Int -> Board -> FenItem -> Parser (Step ( Int, Board ) t)
updateWithFenItem count board item =
    case item of
        FenPiece piece ->
            Parser.succeed
                (Parser.Loop
                    ( count + 1, putPiece (Square count) (Just piece) board )
                )

        FenGap (Rank i) ->
            Parser.succeed (Parser.Loop ( count + i, board ))


fenLineParser : Int -> Board -> Parser Board
fenLineParser startCount startBoard =
    Parser.loop
        ( startCount, startBoard )
        (\( count, board ) ->
            if count == startCount + 8 then
                Parser.succeed (Parser.Done board)

            else if count > startCount + 8 then
                Parser.problem "Encountered more than 8 fen items in a line"

            else
                fenItemParser
                    |> Parser.andThen (updateWithFenItem count board)
        )


boardParser : Parser Board
boardParser =
    Parser.loop
        ( 0, emptyBoard )
        (\( count, board ) ->
            if count == 56 then
                Parser.succeed (\lineUpdated -> Parser.Done lineUpdated)
                    |= fenLineParser count board

            else
                Parser.succeed
                    (\lineUpdated -> Parser.Loop ( count + 8, lineUpdated ))
                    |= fenLineParser count board
                    |. Parser.token "/"
        )


{-| Composable scanners for looking for pieces
-}
type alias Scanner =
    State () (Maybe Square)


{-| Finds the first Just
-}
find : Scanner -> Scanner -> Scanner
find st1 st2 =
    st1
        |> State.andThen
            (\v1 ->
                case v1 of
                    Just x ->
                        State.embed (always (Just x))

                    Nothing ->
                        st2
            )


run : Scanner -> Maybe Square
run scanner =
    State.finalValue () scanner


rayScanner :
    Board
    -> (Board -> Square -> Bool)
    -> Int
    -> (Square -> Bool)
    -> Square
    -> Scanner
rayScanner board condition delta limit start =
    start
        |> State.tailRecM
            (\sq ->
                State.state <|
                    if condition board sq then
                        State.Done (Just sq)

                    else if limit sq || get sq board /= Nothing then
                        State.Done Nothing

                    else
                        offsetBy delta sq
                            |> Maybe.unwrap (State.Done Nothing) State.Loop
            )


{-| Scans the board from a given square in bishop moves stopping at the edge
of the board, a non empty square, or the first index where condition is true.
If such found returns Just that index.
-}
bishopScanner : Board -> (Board -> Square -> Bool) -> Square -> Scanner
bishopScanner board condition start =
    List.foldl find
        (State.state Nothing)
        [ rayScanner board condition 9 (\ix -> file ix == fileH) start
        , rayScanner board condition 7 (\ix -> file ix == fileA) start
        , rayScanner board condition -9 (\ix -> file ix == fileA) start
        , rayScanner board condition -7 (\ix -> file ix == fileH) start
        ]


{-| Scans the board from a given square in rook moves stopping at the edge
of the board, a non empty square, or the first index where condition is true.
If such found returns Just that index.
-}
rookScanner : Board -> (Board -> Square -> Bool) -> Square -> Scanner
rookScanner board condition start =
    List.foldl find
        (State.state Nothing)
        [ rayScanner board condition 8 (always True) start
        , rayScanner board condition 1 (\ix -> file ix == fileH) start
        , rayScanner board condition -1 (\ix -> file ix == fileA) start
        , rayScanner board condition -8 (always True) start
        ]


{-| Scans the board from a given square in queen moves stopping at the edge
of the board, a non empty square, or the first index where condition is true.
If such found returns Just that index.
-}
queenScanner : Board -> (Board -> Square -> Bool) -> Square -> Scanner
queenScanner board condition start =
    find
        (bishopScanner board condition start)
        (rookScanner board condition start)


knightScanner : Board -> (Board -> Square -> Bool) -> Square -> Scanner
knightScanner board condition start =
    let
        checkSq other =
            if
                2
                    < hDist (file start) (file other)
                    || 2
                    < vDist (rank start) (rank other)
            then
                Nothing

            else if condition board other then
                Just other

            else
                Nothing

        forDelta delta =
            State.state <| Maybe.andThen checkSq <| offsetBy delta start
    in
    [ 17, 10, -6, -15, -7, -10, 6, 15 ]
        |> List.map forDelta
        |> List.foldl find (State.state Nothing)


kingScanner : Board -> (Board -> Square -> Bool) -> Square -> Scanner
kingScanner board condition start =
    let
        checkSq other =
            if
                1
                    < hDist (file start) (file other)
                    || 1
                    < vDist (rank start) (rank other)
            then
                Nothing

            else if condition board other then
                Just other

            else
                Nothing

        forDelta delta =
            State.state <| Maybe.andThen checkSq <| offsetBy delta start
    in
    [ 9, 1, -7, -8, -9, -1, 7, 8 ]
        |> List.map forDelta
        |> List.foldl find (State.state Nothing)
