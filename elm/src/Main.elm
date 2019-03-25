import Browser
import Platform.Sub
import Platform.Cmd
import Array
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Html.Attributes exposing (class, id, style)
import Http
import String.Conversions exposing(fromHttpError)

import Game exposing (..)
import GameDecoder exposing (game)
--------------------------------------------------------------------------------

main =
  Browser.element
    { init = init
    , subscriptions = subscriptions
    , update = update
    , view = view
    }

--------------------------------------------------------------------------------
-- Model
type Model
  = Loading
  | Error Http.Error
  | Loaded Game Int

type Msg = Received (Result Http.Error Game)

--------------------------------------------------------------------------------
init : Int -> (Model, Cmd Msg)
init id =
  ( Loading
  , Http.get
    { url = "/games/" ++ (String.fromInt id) ++ ".json"
    , expect = Http.expectJson Received game
    }
  )

subscriptions : Model -> Sub Msg
subscriptions = always Sub.none

--------------------------------------------------------------------------------
update : Msg -> Model -> (Model, Cmd Msg)
update msg model = -- (model, Cmd.none)
  case msg of
    Received (Ok game) -> (Loaded game 0, Cmd.none)
    Received (Err oops) -> (Error oops, Cmd.none)


view : Model -> Html msg
view model =
  case model of
    Loaded game currentMove ->
      let
          { moves } = game
      in viewMoveList (Array.toList moves) currentMove
    Loading -> text "Loading..."
    Error (httpError) -> text <| fromHttpError httpError


viewMoveList : List Move -> Int -> Html msg
viewMoveList moves currentMove =
  div [class "card"]
    [ div [class "grid-y", style "height" "600px"]
      [ div
        [ class "cell"
        , class "medium-cell-block-y"
        , id "movelist-scroll"
        ] (List.indexedMap (viewMovePair currentMove) (pairwise moves))
      ]
    ]


viewMovePair : Int -> Int -> (Move, Maybe Move) -> Html msg
viewMovePair currentMove rowId (left, mright) =
  let
      idDiv =
        div
          [class "cell"
          , class "medium-2"
          , class "medium-offset-1"
          , id (String.fromInt rowId)
          ] [text <| String.fromInt <| rowId]

      wrapInDivs s = div [class "grid-x"] (idDiv :: s)

      stuff =
        case mright of
          Just right ->
            [ div [class "cell", class "medium-4"] [viewMove left currentMove]
            , div [class "cell", class "auto"] [viewMove right currentMove]
            ]
          Nothing ->
            [ div [class "cell", class "medium-4"] [viewMove left currentMove] ]
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
  in div [class "status"] [text san]


pairwise : List a -> List (a, Maybe a)
pairwise xs =
  case xs of
    []  -> []
    [x] -> [(x, Nothing)]
    x::y::zs -> (x, Just y) :: pairwise zs

