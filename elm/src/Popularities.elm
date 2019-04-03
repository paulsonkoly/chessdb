module Popularities exposing (viewPopularities)

import Game exposing (Popularities, PopularityItem)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, id, style)
import Loadable exposing (Loadable(..))


viewPopularities : Loadable Popularities -> Html msg
viewPopularities popularities =
    div [ class "card", id "popularity-card" ]
        (div [ class "card-divider" ]
            [ text "Moves in this position"
            , div [ class "popularity-bar", style "float" "right" ]
                [ div [ class "white-won" ] [ text "1-0" ]
                , div [ class "draw" ] [ text "1/2-1/2" ]
                , div [ class "black-won" ] [ text "0-1" ]
                ]
            ]
            :: Loadable.viewLoadableList popularities viewNormalPopularities
        )


viewNormalPopularities : Popularities -> List (Html msg)
viewNormalPopularities { items } =
    items
        |> List.map viewPopularityItem


viewPopularityItem : PopularityItem -> Html msg
viewPopularityItem item =
    div [ class "grid-x" ]
        [ div [ class "cell", class "medium-2" ] [ text item.nextSan ]
        , div [ class "cell", class "medium-3" ]
            [ text <| String.fromInt <| item.totalCount ]
        , div [ class "cell", class "medium-7" ]
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
