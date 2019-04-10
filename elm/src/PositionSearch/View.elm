module PositionSearch.View exposing (view)

import Board exposing (Castle(..))
import Board.Colour as Colour exposing (Colour(..))
import FormError
import Html exposing (Html, div, input, label, span, text)
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
import Html.Events exposing (onCheck, onInput)
import Position
import PositionSearch.Model as Model exposing (Model)
import PositionSearch.Msg exposing (Msg(..))


view : Model -> Html Msg
view model =
    div [ class "grid-x", class "grid-padding-x" ]
        [ div [ class "cell", class "medium-4" ]
            [ div [ id "chessboard", style "width" "400px" ] [] ]
        , div [ class "cell", class "medium-2" ]
            [ viewBoardForm model ]
        , div [ class "cell", class "medium-6" ]
            [ text <| Debug.toString model ]
        ]


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
        ]
