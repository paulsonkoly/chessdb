module Board.Scanner exposing
    ( Scanner
    , bishop
    , king
    , knight
    , pawn
    , queen
    , rook
    , run
    )

import Board exposing (Board, Colour(..))
import Board.Square as Square
    exposing
        ( Rank(..)
        , Square
        , file
        , fileA
        , fileE
        , fileH
        , hDist
        , offsetBy
        , rank
        , vDist
        )
import Maybe.Extra as Maybe
import State exposing (State)


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
    Square.offsetBy delta start
        |> State.tailRecM
            (State.state
                << Maybe.unwrap
                    (State.Done Nothing)
                    (\sq ->
                        if condition board sq then
                            State.Done (Just sq)

                        else if limit sq || Board.get sq board /= Nothing then
                            State.Done Nothing

                        else
                            State.Loop (Square.offsetBy delta sq)
                    )
            )


{-| Scans the board from a given square in bishop moves stopping at the edge
of the board, a non empty square, or the first index where condition is true.
If such found returns Just that index.
-}
bishop : Board -> (Board -> Square -> Bool) -> Square -> Scanner
bishop board condition start =
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
rook : Board -> (Board -> Square -> Bool) -> Square -> Scanner
rook board condition start =
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
queen : Board -> (Board -> Square -> Bool) -> Square -> Scanner
queen board condition start =
    find
        (bishop board condition start)
        (rook board condition start)


knight : Board -> (Board -> Square -> Bool) -> Square -> Scanner
knight board condition start =
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


king : Board -> (Board -> Square -> Bool) -> Square -> Scanner
king board condition start =
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


pawn : Colour -> Board -> (Board -> Square -> Bool) -> Square -> Scanner
pawn colour board condition start =
    let
        deltas =
            case ( colour, rank start ) of
                ( White, Rank 4 ) ->
                    [ 8, 16 ]

                ( White, _ ) ->
                    [ 8 ]

                ( Black, Rank 5 ) ->
                    [ -8, -16 ]

                ( Black, _ ) ->
                    [ -8 ]

        forDelta delta =
            State.state <|
                Maybe.filter (condition board) <|
                    offsetBy delta start
    in
    deltas
        |> List.map forDelta
        |> List.foldl find (State.state Nothing)
