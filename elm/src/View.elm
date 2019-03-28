module View exposing
    ( MoveNumbers
    , view
    , viewButtons
    , viewMoveList
    , viewPopularities
    )

import Array
import FontAwesome.Icon as I
import FontAwesome.Solid as S
import Game exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Loadable exposing (..)
import Model exposing (..)
import Msg exposing (..)


view : Model -> Html Msg
view model =
    viewLoadable model.game (viewLoaded model)


viewLoaded : Model -> Game -> Html Msg
viewLoaded model game =
    let
        moves =
            game.moves
    in
    div [ class "grid-x", class "grid-margin-x" ]
        [ div [ class "cell", class "small-6" ]
            [ div [ class "grid-y", class "grid-margin-y" ]
                [ div [ class "cell" ]
                    [ div [ id "board-container", style "position" "relative" ]
                        [ div [ id "chessboard", style "width" "400px" ] [] ]
                    ]
                , div [ class "cell" ]
                    [ viewButtons
                        { moveNumber = model.move
                        , lastMoveNumber = Array.length moves - 1
                        }
                    ]
                , div [ class "cell" ] [ viewPopularities model.popularities ]
                ]
            ]
        , div [ class "cell", class "small-4" ]
            [ viewMoveList (Array.toList moves) model.move ]
        ]



--------------------------------------------------------------------------------


viewMoveList : List Move -> Int -> Html Msg
viewMoveList moves currentMove =
    div [ class "card" ]
        [ div [ class "grid-y", style "height" "600px" ]
            [ div
                [ class "cell"
                , class "medium-cell-block-y"
                , id "movelist-scroll"
                ]
                (List.indexedMap (viewMovePair currentMove) (pairwise moves))
            ]
        ]


viewMovePair : Int -> Int -> ( Move, Maybe Move ) -> Html Msg
viewMovePair currentMove rowId ( left, mright ) =
    let
        strRowId =
            String.fromInt rowId

        idDiv =
            div
                [ class "cell"
                , class "medium-2"
                , class "medium-offset-1"
                , id ("movelist-row-" ++ strRowId)
                ]
                [ text (String.fromInt (rowId + 1) ++ ".") ]

        wrapInDivs s =
            div [ class "grid-x" ] (idDiv :: s)

        stuff =
            case mright of
                Just right ->
                    [ div [ class "cell", class "medium-4" ]
                        [ viewMove left currentMove ]
                    , div [ class "cell", class "auto" ]
                        [ viewMove right currentMove ]
                    ]

                Nothing ->
                    [ div [ class "cell", class "medium-4" ]
                        [ viewMove left currentMove ]
                    ]
    in
    wrapInDivs stuff


viewMove : Move -> Int -> Html Msg
viewMove { san, id, fullMoveNumber, activeColour } currentMove =
    let
        thisMove =
            fullMoveNumber * 2 + activeColour - 3

        status =
            if thisMove < currentMove then
                "passed"

            else if thisMove == currentMove then
                "current"

            else
                "upcoming"
    in
    div [ class status, onClick (SetMoveNumberTo thisMove NoScroll) ]
        [ text san ]


pairwise : List a -> List ( a, Maybe a )
pairwise xs =
    case xs of
        [] ->
            []

        [ x ] ->
            [ ( x, Nothing ) ]

        x :: y :: zs ->
            ( x, Just y ) :: pairwise zs



--------------------------------------------------------------------------------


type alias MoveNumbers =
    { moveNumber : Int
    , lastMoveNumber : Int
    }


viewButtons : MoveNumbers -> Html Msg
viewButtons { moveNumber, lastMoveNumber } =
    let
        setter newMoveNumber =
            SetMoveNumberTo newMoveNumber Scroll
    in
    viewCenterCell <|
        div [ class "button-group" ]
            [ viewButton (setter -1) S.angleDoubleLeft
            , viewButton (setter (moveNumber - 1)) S.angleLeft
            , viewButton (setter (moveNumber + 1)) S.angleRight
            , viewButton (setter lastMoveNumber) S.angleDoubleRight
            , button [ class "button" ] [ I.view S.info ] -- TODO : modal help
            ]


viewButton : msg -> I.Icon -> Html msg
viewButton msg icon =
    button [ class "button", onClick msg ] [ I.view icon ]


viewCenterCell : Html Msg -> Html Msg
viewCenterCell inner =
    div [ class "grid-x" ]
        [ div [ class "cell", class "auto" ] []
        , div [ class "cell", class "shrink" ] [ inner ]
        , div [ class "cell", class "auto" ] []
        ]



--------------------------------------------------------------------------------


viewPopularities : Loadable Popularities -> Html msg
viewPopularities popularities =
    div [ class "card", id "popularity-card" ]
        (div [ class "card-divider" ]
            [ text "Move popularities"
            , div [ class "popularity-bar", style "float" "right" ]
                [ div [ class "white-won" ] [ text "1-0" ]
                , div [ class "draw" ] [ text "1/2-1/2" ]
                , div [ class "black-won" ] [ text "0-1" ]
                ]
            ]
            :: viewLoadableList popularities viewNormalPopularities
        )


viewNormalPopularities : Popularities -> List (Html msg)
viewNormalPopularities { items } =
    items
        |> List.map viewPopularityItem


viewPopularityItem : PopularityItem -> Html msg
viewPopularityItem item =
    div [ class "grid-x" ]
        [ div [ class "cell", class "small-2" ] [ text item.nextSan ]
        , div [ class "cell", class "small-3" ]
            [ text <| String.fromInt <| item.totalCount ]
        , div [ class "cell", class "large-7" ]
            [ div [ class "popularity-bar" ] (viewPopularityItemBar item) ]
        ]


viewPopularityItemBar : PopularityItem -> List (Html msg)
viewPopularityItemBar item =
    let
        viewBarElem ( class_, selector ) =
            let
                count =
                    selector item

                a =
                    toFloat count

                b =
                    toFloat item.totalCount

                c =
                    String.fromFloat (a / b * 100) |> (\l -> l ++ "%")
            in
            if count > 0 then
                Just <|
                    div
                        [ class class_, style "width" c ]
                        [ text <| String.fromInt <| count ]

            else
                Nothing
    in
    List.filterMap viewBarElem
        [ ( "white-won", .whiteWon )
        , ( "draw", .draw )
        , ( "black-won", .blackWon )
        ]
