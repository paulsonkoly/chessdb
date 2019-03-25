module View exposing
  ( viewMoveList
  , viewButtons
  )

import Html as H exposing (Html)
import Html.Attributes as A

import Game exposing (..)

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
          ] [H.text (strRowId ++ ".")]

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
      thisMove = fullMoveNumber + activeColour
      status = if thisMove < currentMove then
          "upcoming"
        else if thisMove == currentMove then
          "current"
        else
          "passed"
  in H.div [A.class "status"] [H.text san]


pairwise : List a -> List (a, Maybe a)
pairwise xs =
  case xs of
    []  -> []
    [x] -> [(x, Nothing)]
    x::y::zs -> (x, Just y) :: pairwise zs


--------------------------------------------------------------------------------
viewButtons : Html msg
viewButtons = 
  H.div [A.class "button-group"]
    [ H.button [A.class "button"] [H.text "<<"] -- TODO icons
    , H.button [A.class "button"] [H.text "<"]
    , H.button [A.class "button"] [H.text ">"]
    , H.button [A.class "button"] [H.text ">>"]
    , H.button [A.class "button"] [H.text "i"]
    ]
