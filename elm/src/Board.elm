module Board exposing
    ( Board
    , Colour(..)
    , Kind(..)
    , Move
    , Piece(..)
    , boardParser
    , emptyBoard
    , moveParser
    , putPiece
    )

import Array exposing (Array)
import Parser exposing ((|.), (|=), Parser, Step)


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


putPiece : Int -> Piece -> Board -> Board
putPiece ix piece (Board data) =
    Board (Array.set ix (Just piece) data)


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
    = File Char


fileParser : Parser File
fileParser =
    Parser.oneOf
        [ Parser.succeed (File 'a') |. Parser.token "a"
        , Parser.succeed (File 'b') |. Parser.token "b"
        , Parser.succeed (File 'c') |. Parser.token "c"
        , Parser.succeed (File 'd') |. Parser.token "d"
        , Parser.succeed (File 'e') |. Parser.token "e"
        , Parser.succeed (File 'f') |. Parser.token "f"
        , Parser.succeed (File 'g') |. Parser.token "g"
        , Parser.succeed (File 'h') |. Parser.token "h"
        ]


type Rank
    = Rank Int


rankParser : Parser Rank
rankParser =
    Parser.int
        |> Parser.andThen
            (\rank ->
                if 1 <= rank && rank <= 8 then
                    Parser.succeed (Rank rank)

                else
                    Parser.problem
                        ("Rank should be between 1 and 8 (was "
                            ++ String.fromInt rank
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
    = Square File Rank


squareParser : Parser Square
squareParser =
    Parser.succeed Square
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
            (\file rank promotion ->
                Normal Pawn Nothing False (Square file rank) promotion
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
                    ( count + 1, putPiece count piece board )
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
