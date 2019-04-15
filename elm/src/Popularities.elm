module Popularities exposing
    ( Msg(..)
    , Popularities
    , decoder
    , validateToken
    , view
    )

import Html exposing (Html, div, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class, id, style)
import Html.Events exposing (onClick)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Loadable exposing (Loadable(..))


type Popularities
    = Popularities
        { token : Int
        , items : List PopularityItem
        }


validateToken : Int -> Popularities -> Bool
validateToken other (Popularities { token }) =
    token == other


type alias PopularityItem =
    { nextSan : String
    , whiteWon : Int
    , blackWon : Int
    , draw : Int
    , totalCount : Int
    }


type Msg
    = MoveClicked String


decoder : Decoder Popularities
decoder =
    Decode.succeed (\a b -> Popularities { token = a, items = b })
        |> Pipeline.required "token" Decode.int
        |> Pipeline.required "moves" (Decode.list popularityItemDecoder)


popularityItemDecoder : Decoder PopularityItem
popularityItemDecoder =
    Decode.succeed PopularityItem
        |> Pipeline.required "next_san" Decode.string
        |> Pipeline.required "white_won" Decode.int
        |> Pipeline.required "black_won" Decode.int
        |> Pipeline.required "draw" Decode.int
        |> Pipeline.required "total_count" Decode.int


view : Loadable Popularities -> Html Msg
view popularities =
    let
        header =
            thead []
                [ th [] [ text "Move" ]
                , th [] [ text "# in DB" ]
                , th []
                    [ div [ class "popularity-bar" ]
                        [ div [ class "white-won" ] [ text "1-0" ]
                        , div [ class "draw" ] [ text "1/2-1/2" ]
                        , div [ class "black-won" ] [ text "0-1" ]
                        ]
                    ]
                ]

        body =
            tbody [] <|
                Loadable.viewLoadableList popularities viewNormalPopularities []
    in
    table [ class "hover", class "popularities-list" ] [ header, body ]


viewNormalPopularities : Popularities -> List (Html Msg)
viewNormalPopularities (Popularities { items }) =
    items
        |> List.map viewPopularityItem


viewPopularityItem : PopularityItem -> Html Msg
viewPopularityItem item =
    tr [ onClick (MoveClicked item.nextSan) ]
        [ td [] [ text item.nextSan ]
        , td [] [ text <| String.fromInt <| item.totalCount ]
        , td [] [ div [ class "popularity-bar" ] (viewPopularityItemBar item) ]
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
