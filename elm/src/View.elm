module View exposing
  ( viewMoveList
  , viewButtons
  , Button(..)
  )

import FontAwesome.Icon as I
import FontAwesome.Solid as S
import Game exposing (..)
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events as E


--------------------------------------------------------------------------------
-- MoveList
viewMoveList : List Move -> Int -> Html msg
viewMoveList moves currentMove =
  H.div [A.class "card"]
    [ H.div [A.class "grid-y", A.style "height" "600px"]
      [ H.div
        [ A.class "cell"
        , A.class "medium-cell-block-y"
        , A.id "movelist-scroll"
        ] (List.indexedMap (viewMovePair currentMove) (pairwise moves))
      ]
    ]


viewMovePair : Int -> Int -> (Move, Maybe Move) -> Html msg
viewMovePair currentMove rowId (left, mright) =
  let
      strRowId = String.fromInt rowId

      idDiv =
        H.div
          [A.class "cell"
          , A.class "medium-2"
          , A.class "medium-offset-1"
          , A.id ("movelist-row-" ++ strRowId)
          ] [H.text (String.fromInt (rowId + 1) ++ ".")]

      wrapInDivs s = H.div [A.class "grid-x"] (idDiv :: s)

      stuff =
        case mright of
          Just right ->
            [ H.div [A.class "cell", A.class "medium-4"] [viewMove left currentMove]
            , H.div [A.class "cell", A.class "auto"] [viewMove right currentMove]
            ]
          Nothing ->
            [ H.div [A.class "cell", A.class "medium-4"] [viewMove left currentMove] ]
  in wrapInDivs stuff


viewMove : Move -> Int -> Html msg
viewMove {san, id, fullMoveNumber, activeColour } currentMove =
  let
      thisMove = fullMoveNumber * 2 + activeColour - 3
      status = if thisMove < currentMove then
          "passed"
        else if thisMove == currentMove then
          "current"
        else
          "upcoming"
  in H.div [A.class status] [H.text san]


pairwise : List a -> List (a, Maybe a)
pairwise xs =
  case xs of
    []  -> []
    [x] -> [(x, Nothing)]
    x::y::zs -> (x, Just y) :: pairwise zs


--------------------------------------------------------------------------------
type Button
  = ToStartPosition
  | ToLeft
  | ToRight
  | ToEndPosition


viewButtons : Html Button
viewButtons =
    H.div [ A.class "button-group" ]
        [ viewButton ToStartPosition S.angleDoubleLeft
        , viewButton ToLeft S.angleLeft
        , viewButton ToRight S.angleRight
        , viewButton ToEndPosition S.angleDoubleRight
        , H.button [ A.class "button" ] [ I.view S.info ] -- TODO : modal help
        ]

viewButton : msg -> I.Icon -> Html msg
viewButton msg icon =
  H.button [ A.class "button", E.onClick msg ] [ I.view icon ]
