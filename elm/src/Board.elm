module Board exposing
    ( Board
    , Castle(..)
    , Colour(..)
    , Disambiguity(..)
    , Kind(..)
    , Move(..)
    , Piece(..)
    , boardParser
    , capture
    , disambiguate
    , empty
    , flip
    , get
    , initial
    , move
    , moveParser
    , promote
    , putPiece
    , toFen
    )

import Array exposing (Array)
import Board.Square exposing (..)
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


type Colour
    = White
    | Black


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
        |> putPiece a1 (Just (Piece White Rook))
        |> putPiece b1 (Just (Piece White Knight))
        |> putPiece c1 (Just (Piece White Bishop))
        |> putPiece d1 (Just (Piece White Queen))
        |> putPiece e1 (Just (Piece White King))
        |> putPiece f1 (Just (Piece White Bishop))
        |> putPiece g1 (Just (Piece White Knight))
        |> putPiece h1 (Just (Piece White Rook))
        |> putPiece a2 (Just (Piece White Pawn))
        |> putPiece b2 (Just (Piece White Pawn))
        |> putPiece c2 (Just (Piece White Pawn))
        |> putPiece d2 (Just (Piece White Pawn))
        |> putPiece e2 (Just (Piece White Pawn))
        |> putPiece f2 (Just (Piece White Pawn))
        |> putPiece g2 (Just (Piece White Pawn))
        |> putPiece h2 (Just (Piece White Pawn))
        |> putPiece a7 (Just (Piece Black Pawn))
        |> putPiece b7 (Just (Piece Black Pawn))
        |> putPiece c7 (Just (Piece Black Pawn))
        |> putPiece d7 (Just (Piece Black Pawn))
        |> putPiece e7 (Just (Piece Black Pawn))
        |> putPiece f7 (Just (Piece Black Pawn))
        |> putPiece g7 (Just (Piece Black Pawn))
        |> putPiece h7 (Just (Piece Black Pawn))
        |> putPiece a8 (Just (Piece Black Rook))
        |> putPiece b8 (Just (Piece Black Knight))
        |> putPiece c8 (Just (Piece Black Bishop))
        |> putPiece d8 (Just (Piece Black Queen))
        |> putPiece e8 (Just (Piece Black King))
        |> putPiece f8 (Just (Piece Black Bishop))
        |> putPiece g8 (Just (Piece Black Knight))
        |> putPiece h8 (Just (Piece Black Rook))


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
        [ fileParser |> Parser.map FileDisambiguity
        , rankParser |> Parser.map RankDisambiguity
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
                Normal
                    { kind = Pawn
                    , disambiguity = Nothing
                    , capture = False
                    , destination = square f r
                    , promotion = promotion
                    }
            )
            |= Parser.backtrackable fileParser
            |= rankParser
            |= promotionParser
        , Parser.succeed
            (\kind disambiguity captrue destination promotion ->
                Normal
                    { kind = kind
                    , disambiguity = disambiguity
                    , capture = captrue
                    , destination = destination
                    , promotion = promotion
                    }
            )
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

        newGapCount =
            case mPiece of
                Just piece ->
                    0

                Nothing ->
                    if modBy 8 state.ix == 7 then
                        0

                    else
                        state.gapCount + 1

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

        gapString =
            case mPiece of
                Just piece ->
                    gapOut

                Nothing ->
                    if modBy 8 state.ix == 7 then
                        String.fromInt <| state.gapCount + 1

                    else
                        ""
    in
    { ix = newIx, gapCount = newGapCount, string = state.string ++ gapString ++ string ++ separator }


toFen : Board -> String
toFen (Board data) =
    List.foldl toFenStep { ix = 0, gapCount = 0, string = "" } (Array.toList data)
        |> .string



--     State.finalValue { ix = 0, gapCount = 0  }
--         |< State.foldlM
--             (\mPiece str ->
--                 State.get |> State.andThen (\{ ix , count } ->
--                     State.modify (\{ix, gapCount} -> { ix + 1, gapCount }) <|
--                         case (ix modBy 8, mPiece) of ->
--                             (_, Nothing) ->
--                             (_, Just piece) ->
--                                 State.modify (\{ix, gapCount} -> { ix, 0 })
--                                     |> State.state State.state (str ++ (pieceToFen piece))
--                             (7, Nothing) ->
--                             (7, Just piece) ->
--                  )
--             )
--             ""
--             (Array.toList data)
------------------------------------------------------------------------
--                         Data manipulations                         --
------------------------------------------------------------------------


flip : Colour -> Colour
flip colour =
    case colour of
        White ->
            Black

        Black ->
            White


putPiece : Square -> Maybe Piece -> Board -> Board
putPiece (Square ix) piece (Board data) =
    Board (Array.set ix piece data)


get : Square -> Board -> Maybe Piece
get (Square ix) (Board board) =
    Maybe.join (Array.get ix board)
