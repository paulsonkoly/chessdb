module Board exposing
    ( Board
    , Castle(..)
    , Colour(..)
    , Disambiguity(..)
    , Kind(..)
    , Move(..)
    , Piece(..)
    , bishopScanner
    , boardParser
    , emptyBoard
    , flip
    , get
    , kingScanner
    , knightScanner
    , moveParser
    , putPiece
    , queenScanner
    , rookScanner
    , run
    )

import Array exposing (Array)
import Board.Square exposing (..)
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


type Disambiguity
    = FileDisambiguity File
    | RankDisambiguity Rank


disambiguityParser : Parser Disambiguity
disambiguityParser =
    Parser.oneOf
        [ fileParser |> Parser.map FileDisambiguity
        , rankParser |> Parser.map RankDisambiguity
        ]


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
    offsetBy delta start
        |> State.tailRecM
            (State.state
                << Maybe.unwrap
                    (State.Done Nothing)
                    (\sq ->
                        if condition board sq then
                            State.Done (Just sq)

                        else if limit sq || get sq board /= Nothing then
                            State.Done Nothing

                        else
                            State.Loop (offsetBy delta sq)
                    )
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
        [ rayScanner board condition 8 (always False) start
        , rayScanner board condition 1 (\ix -> file ix == fileH) start
        , rayScanner board condition -1 (\ix -> file ix == fileA) start
        , rayScanner board condition -8 (always False) start
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
    [ 17, 10, -6, -15, -17, -10, 6, 15 ]
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
