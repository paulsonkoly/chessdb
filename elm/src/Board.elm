module Board exposing
    ( Board
    , Castle(..)
    , Disambiguity(..)
    , Kind(..)
    , Move(..)
    , Piece(..)
    , boardParser
    , capture
    , disambiguate
    , empty
    , get
    , initial
    , move
    , moveParser
    , promote
    , putPiece
    , toFen
    )

import Array exposing (Array)
import Board.Colour exposing (Colour(..))
import Board.Square as Square exposing (File(..), Rank(..), Square(..))
import Maybe.Extra as Maybe
import Parser exposing ((|.), (|=), Parser, Step)
import State



------------------------------------------------------------------------
--                               Types                                --
------------------------------------------------------------------------


type Kind
    = Pawn
    | Rook
    | Knight
    | Bishop
    | Queen
    | King


type Piece
    = Piece Colour Kind


type Board
    = Board (Array (Maybe Piece))


empty : Board
empty =
    Board (Array.repeat 64 Nothing)


initial : Board
initial =
    empty
        |> putPiece Square.a1 (Just (Piece White Rook))
        |> putPiece Square.b1 (Just (Piece White Knight))
        |> putPiece Square.c1 (Just (Piece White Bishop))
        |> putPiece Square.d1 (Just (Piece White Queen))
        |> putPiece Square.e1 (Just (Piece White King))
        |> putPiece Square.f1 (Just (Piece White Bishop))
        |> putPiece Square.g1 (Just (Piece White Knight))
        |> putPiece Square.h1 (Just (Piece White Rook))
        |> putPiece Square.a2 (Just (Piece White Pawn))
        |> putPiece Square.b2 (Just (Piece White Pawn))
        |> putPiece Square.c2 (Just (Piece White Pawn))
        |> putPiece Square.d2 (Just (Piece White Pawn))
        |> putPiece Square.e2 (Just (Piece White Pawn))
        |> putPiece Square.f2 (Just (Piece White Pawn))
        |> putPiece Square.g2 (Just (Piece White Pawn))
        |> putPiece Square.h2 (Just (Piece White Pawn))
        |> putPiece Square.a7 (Just (Piece Black Pawn))
        |> putPiece Square.b7 (Just (Piece Black Pawn))
        |> putPiece Square.c7 (Just (Piece Black Pawn))
        |> putPiece Square.d7 (Just (Piece Black Pawn))
        |> putPiece Square.e7 (Just (Piece Black Pawn))
        |> putPiece Square.f7 (Just (Piece Black Pawn))
        |> putPiece Square.g7 (Just (Piece Black Pawn))
        |> putPiece Square.h7 (Just (Piece Black Pawn))
        |> putPiece Square.a8 (Just (Piece Black Rook))
        |> putPiece Square.b8 (Just (Piece Black Knight))
        |> putPiece Square.c8 (Just (Piece Black Bishop))
        |> putPiece Square.d8 (Just (Piece Black Queen))
        |> putPiece Square.e8 (Just (Piece Black King))
        |> putPiece Square.f8 (Just (Piece Black Bishop))
        |> putPiece Square.g8 (Just (Piece Black Knight))
        |> putPiece Square.h8 (Just (Piece Black Rook))


type Castle
    = Short
    | Long


type Disambiguity
    = FileDisambiguity File
    | RankDisambiguity Rank


type Move
    = Castle Castle
    | Normal
        { kind : Kind
        , disambiguity : Maybe Disambiguity
        , capture : Bool
        , destination : Square
        , promotion : Maybe Kind
        }


move : Kind -> Square -> Move
move kind destination =
    Normal
        { kind = kind
        , disambiguity = Nothing
        , capture = False
        , destination = destination
        , promotion = Nothing
        }


capture : Kind -> Square -> Move
capture kind destination =
    Normal
        { kind = kind
        , disambiguity = Nothing
        , capture = True
        , destination = destination
        , promotion = Nothing
        }


promote : Kind -> Square -> Move
promote kind destination =
    Normal
        { kind = Pawn
        , disambiguity = Nothing
        , capture = False
        , destination = destination
        , promotion = Just kind
        }


disambiguate : Disambiguity -> Move -> Move
disambiguate disambiguity moveE =
    case moveE of
        Normal nmove ->
            Normal { nmove | disambiguity = Just disambiguity }

        Castle _ ->
            moveE



------------------------------------------------------------------------
--                              Parsers                               --
------------------------------------------------------------------------


kindParser : Parser Kind
kindParser =
    Parser.oneOf
        [ Parser.succeed Pawn |. Parser.token "P"
        , Parser.succeed Rook |. Parser.token "R"
        , Parser.succeed Knight |. Parser.token "N"
        , Parser.succeed Bishop |. Parser.token "B"
        , Parser.succeed Queen |. Parser.token "Q"
        , Parser.succeed King |. Parser.token "K"
        , Parser.succeed Pawn
        ]


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


castleParser : Parser Castle
castleParser =
    Parser.oneOf
        [ Parser.succeed Long |. Parser.token "O-O-O"
        , Parser.succeed Short |. Parser.token "O-O"
        ]


disambiguityParser : Parser Disambiguity
disambiguityParser =
    Parser.oneOf
        [ Square.fileParser |> Parser.map FileDisambiguity
        , Square.rankParser |> Parser.map RankDisambiguity
        ]


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
        , Parser.succeed Nothing
        ]


captureParser : Parser Bool
captureParser =
    Parser.oneOf
        [ Parser.succeed True |. Parser.token "x"
        , Parser.succeed False
        ]


middleParser =
    -- parses disambiguity capture destination part of the san move
    -- main branches, 1.) e5 <= first e is backtracked to match 3.)
    --                2.) xe5
    --                    Note that this will accept moves like "xe5" which is
    --                    incorrect. Here we would have to know if it was
    --                    preceeded by a piece kind
    --                2.) e(x?)e5
    --                3.) 5(x?)e5
    Parser.oneOf
        [ Parser.succeed
            (\destination ->
                { disambiguity = Nothing
                , capture = False
                , destination = destination
                }
            )
            |= Parser.backtrackable Square.parser
        , Parser.succeed
            (\destination ->
                { disambiguity = Nothing
                , capture = True
                , destination = destination
                }
            )
            |. Parser.token "x"
            |= Square.parser
        , Parser.succeed
            (\file capture_ destination ->
                { disambiguity = Just (FileDisambiguity file)
                , capture = capture_
                , destination = destination
                }
            )
            |= Square.fileParser
            |= captureParser
            |= Square.parser
        , Parser.succeed
            (\rank capture_ destination ->
                { disambiguity = Just (RankDisambiguity rank)
                , capture = capture_
                , destination = destination
                }
            )
            |= Square.rankParser
            |= captureParser
            |= Square.parser
        ]


normalParser : Parser Move
normalParser =
    Parser.succeed
        (\kind mid promotion ->
            Normal
                { kind = kind
                , disambiguity = mid.disambiguity
                , capture = mid.capture
                , destination = mid.destination
                , promotion = promotion
                }
        )
        |= kindParser
        |= middleParser
        |= promotionParser
        |. Parser.oneOf
            [ Parser.end
            , Parser.token "+" |. Parser.end
            , Parser.token "#" |. Parser.end
            ]


type FenItem
    = FenPiece Piece
    | FenGap Rank


fenItemParser : Parser FenItem
fenItemParser =
    Parser.oneOf
        [ pieceParser |> Parser.map FenPiece
        , Square.rankParser |> Parser.map FenGap
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
        ( 0, empty )
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



------------------------------------------------------------------------
--                             transforms                             --
------------------------------------------------------------------------


pieceToFen piece =
    case piece of
        Piece White Pawn ->
            "P"

        Piece White Rook ->
            "R"

        Piece White Knight ->
            "N"

        Piece White Bishop ->
            "B"

        Piece White Queen ->
            "Q"

        Piece White King ->
            "K"

        Piece Black Pawn ->
            "p"

        Piece Black Rook ->
            "r"

        Piece Black Knight ->
            "n"

        Piece Black Bishop ->
            "b"

        Piece Black Queen ->
            "q"

        Piece Black King ->
            "k"


type alias ToFenState =
    { ix : Int, gapCount : Int, string : String }


toFenStep : Maybe Piece -> ToFenState -> ToFenState
toFenStep mPiece state =
    let
        separator =
            if modBy 8 state.ix == 7 && state.ix /= 63 then
                "/"

            else
                ""

        newIx =
            state.ix + 1

        ( newGapCount, gapString ) =
            case mPiece of
                Just piece ->
                    ( 0, gapOut )

                Nothing ->
                    if modBy 8 state.ix == 7 then
                        ( 0, String.fromInt <| state.gapCount + 1 )

                    else
                        ( state.gapCount + 1, "" )

        string =
            case mPiece of
                Just piece ->
                    pieceToFen piece

                Nothing ->
                    ""

        gapOut =
            if state.gapCount /= 0 then
                String.fromInt <| state.gapCount

            else
                ""
    in
    { ix = newIx
    , gapCount = newGapCount
    , string = state.string ++ gapString ++ string ++ separator
    }


toFen : Board -> String
toFen (Board data) =
    List.foldl toFenStep { ix = 0, gapCount = 0, string = "" } (Array.toList data)
        |> .string



------------------------------------------------------------------------
--                         Data manipulations                         --
------------------------------------------------------------------------


putPiece : Square -> Maybe Piece -> Board -> Board
putPiece (Square ix) piece (Board data) =
    Board (Array.set ix piece data)


get : Square -> Board -> Maybe Piece
get (Square ix) (Board board) =
    Maybe.join (Array.get ix board)
