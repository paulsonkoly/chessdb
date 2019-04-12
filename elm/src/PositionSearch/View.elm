module PositionSearch.View exposing (view)

{-| View for position search

@docs view

-}

import Board exposing (Castle(..))
import Board.Colour as Colour exposing (Colour(..))
import FormError
import Html
    exposing
        ( Html
        , button
        , div
        , input
        , label
        , span
        , table
        , tbody
        , td
        , text
        , th
        , thead
        , tr
        )
import Html.Attributes
    exposing
        ( attribute
        , checked
        , class
        , for
        , id
        , name
        , placeholder
        , style
        , type_
        , value
        )
import Html.Events exposing (onCheck, onClick, onInput)
import Loadable
import Pagination
import Position
import PositionSearch.Model as Model exposing (GameAtMove, Model)
import PositionSearch.Msg exposing (Msg(..))


view : Model -> Html Msg
view model =
    div [ class "grid-x", class "grid-padding-x" ]
        [ div [ class "cell", class "medium-4" ]
            [ div [ id "chessboard", style "width" "400px" ] [] ]
        , div [ class "cell", class "medium-3" ]
            [ viewBoardForm model ]
        , div [ class "cell", class "medium-5" ]
            [ Loadable.viewLoadable model.games viewGameProperties
            , Html.map PaginationRequested (Pagination.view model.pagination)
            ]
        ]


viewGameProperties : List GameAtMove -> Html Msg
viewGameProperties games =
    let
        header =
            thead []
                [ th [] [ text "White" ]
                , th [] [ text "Black" ]
                , th [] [ text "Move #" ]
                ]

        eloString int =
            " (" ++ String.fromInt int ++ ")"

        white game =
            game.white ++ eloString game.whiteElo

        black game =
            game.black ++ eloString game.blackElo

        body =
            tbody [] <|
                List.map
                    (\{ game, fullmoveNumber } ->
                        tr
                            -- [ onClick (GameLoadRequested game.id) ]
                            []
                            [ td [] [ text (white game) ]
                            , td [] [ text (black game) ]
                            , td [] [ text (String.fromInt fullmoveNumber) ]
                            ]
                    )
                    games
    in
    table [ class "hover", class "game-list" ] [ header, body ]


viewCastleToggle : Model -> String -> String -> Colour -> Castle -> Html Msg
viewCastleToggle model lbl iden colour castle =
    label []
        [ text lbl
        , div [ class "switch" ]
            [ input
                [ class "switch-input"
                , id iden
                , name iden
                , type_ "checkbox"
                , checked
                    (Position.canCastle colour castle model.position)
                , onCheck (CastleChecked colour castle)
                ]
                []
            , label [ class "switch-paddle", for iden ]
                [ span
                    [ attribute "aria-hidden" "true"
                    , class "switch-active"
                    ]
                    [ text "Yes" ]
                , span
                    [ attribute "aria-hidden" "true"
                    , class "switch-inactive"
                    ]
                    [ text "No" ]
                ]
            ]
        ]


viewBoardForm : Model -> Html Msg
viewBoardForm model =
    div [ class "grid-x" ]
        [ div [ class "cell medium-6" ]
            [ label []
                [ text "Side to move"
                , div [ class "switch" ]
                    [ input
                        [ class "switch-input"
                        , id "active_colour"
                        , name "active_colour"
                        , type_ "checkbox"
                        , Colour.toHtmlAttribute model.position.activeColour
                        , onCheck (Colour.fromHtmlToggle >> ActiveColourChecked)
                        ]
                        []
                    , label [ class "switch-paddle", for "active_colour" ]
                        [ span
                            [ attribute "aria-hidden" "true"
                            , class "switch-active"
                            ]
                            [ text "W" ]
                        , span
                            [ attribute "aria-hidden" "true"
                            , class "switch-inactive"
                            ]
                            [ text "B" ]
                        ]
                    ]
                ]
            ]
        , div [ class "cell medium-6" ]
            [ label []
                [ text "En passant"
                , input
                    ([ placeholder "square"
                     , type_ "text"
                     , onInput EnPassantInputted
                     ]
                        ++ FormError.errorAttribute model.enPassantStringError
                    )
                    []
                ]
            , FormError.viewError model.enPassantStringError
            ]
        , div [ class "cell medium-6" ]
            [ viewCastleToggle
                model
                "White can short castle"
                "white_short_castle"
                White
                Short
            ]
        , div [ class "cell medium-6" ]
            [ viewCastleToggle
                model
                "White can long castle"
                "white_long_castle"
                White
                Long
            ]
        , div [ class "cell medium-6" ]
            [ viewCastleToggle
                model
                "Black can short castle"
                "black_short_castle"
                Black
                Short
            ]
        , div [ class "cell medium-6" ]
            [ viewCastleToggle
                model
                "Black can long castle"
                "black_long_castle"
                Black
                Long
            ]
        , div [ class "cell medium-6" ]
            [ div [ class "button-group" ]
                [ button
                    [ class "button"
                    , class "secondary"
                    , onClick ClearClicked
                    ]
                    [ text "Clear" ]
                , button
                    [ class "button"
                    , class "secondary"
                    , onClick InitalClicked
                    ]
                    [ text "Initial" ]
                ]
            ]
        , div [ class "cell medium-6" ]
            [ button [ class "button", onClick SearchClicked ]
                [ text "Search" ]
            ]
        ]
