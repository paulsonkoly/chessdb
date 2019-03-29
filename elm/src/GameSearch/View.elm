module GameSearch.View exposing (view)

import Debug as Debug
import GameSearch.Model as Model exposing (Error, Model)
import GameSearch.Msg exposing (FieldChange(..), Msg(..))
import Html exposing (Html, button, div, form, input, label, span, text)
import Html.Attributes
    exposing
        ( action
        , class
        , disabled
        , for
        , id
        , name
        , type_
        , value
        )
import Html.Events exposing (onInput, onSubmit)


viewEloFields : Error -> Html Msg
viewEloFields err =
    let
        errorAttribute =
            case err of
                Just _ ->
                    [ class "is-invalid-input" ]

                Nothing ->
                    []

        errorMessage =
            case err of
                Just oops ->
                    span [ class "form-error", class "is-visible" ]
                        [ text oops ]

                Nothing ->
                    span [ class "form-error" ] []
    in
    div [ class "grid-x grid-padding-x" ]
        [ div [ class "cell medium-6" ]
            [ label [ for "min_elo" ] [ text "Minimum ELO" ]
            , input
                ([ name "min_elo"
                 , type_ "text"
                 , onInput (FormFieldChange << MinimumEloChanged)
                 ]
                    ++ errorAttribute
                )
                []
            ]
        , div [ class "cell medium-6" ]
            [ label [ for "man_elo" ] [ text "Maximum ELO" ]
            , input
                ([ name "max_elo"
                 , type_ "text"
                 , onInput (FormFieldChange << MaxiumEloChanged)
                 ]
                    ++ errorAttribute
                )
                []
            ]
        , div [ class "cell medium-12" ] [ errorMessage ]
        ]


view : Model -> Html Msg
view model =
    div [ class "grid-x grid-margin-x" ]
        [ div [ class "medium-6 cell" ]
            [ div [ class "callout" ]
                [ form [ onSubmit FormSubmitted ]
                    [ div [ class "grid-x grid-padding-x" ]
                        [ div [ class "cell medium-6" ]
                            [ label [ for "white" ] [ text "White" ]
                            , input
                                [ name "white"
                                , type_ "text"
                                , onInput (FormFieldChange << WhiteChanged)
                                ]
                                []
                            ]
                        , div [ class "cell medium-6" ]
                            [ label [ for "black" ] [ text "Black" ]
                            , input
                                [ name "black"
                                , type_ "text"
                                , onInput (FormFieldChange << BlackChanged)
                                ]
                                []
                            ]
                        ]
                    , div [ class "grid-x grid-padding-x" ]
                        [ div [ class "cell medium-6" ]
                            [ label [ for "black" ] [ text "Either colour" ]
                            , input
                                [ name "either_colour"
                                , type_ "text"
                                , onInput (FormFieldChange << EitherColourChanged)
                                ]
                                []
                            ]
                        ]
                    , viewEloFields model.elosDontMatch
                    , div [ class "grid-x grid-padding-x" ]
                        [ div [ class "cell medium-6" ]
                            [ label [ for "min_elo" ] [ text "Event" ]
                            , input
                                [ name "event"
                                , type_ "text"
                                , onInput (FormFieldChange << EventChanged)
                                ]
                                []
                            ]
                        , div [ class "cell medium-6" ]
                            [ label [ for "man_elo" ] [ text "Site" ]
                            , input
                                [ name "site"
                                , type_ "text"
                                , onInput (FormFieldChange << SiteChanged)
                                ]
                                []
                            ]
                        ]
                    , div [ class "grid-x grid-padding-x" ]
                        [ div [ class "cell medium-6" ]
                            [ label [ for "min_elo" ] [ text "Date" ]
                            , input
                                [ name "date"
                                , type_ "text"
                                , onInput (FormFieldChange << DateChanged)
                                ]
                                []
                            ]
                        , div [ class "cell medium-6" ]
                            [ label [ for "man_elo" ] [ text "Round" ]
                            , input
                                [ name "round"
                                , type_ "text"
                                , onInput (FormFieldChange << RoundChanged)
                                ]
                                []
                            ]
                        ]
                    , div [ class "grid-x grid-padding-x" ]
                        [ div [ class "cell medium-6" ]
                            [ label [ for "min_elo" ] [ text "Result" ]
                            , input
                                [ name "result"
                                , type_ "text"
                                , onInput (FormFieldChange << ResultChanged)
                                ]
                                []
                            ]
                        , div [ class "cell medium-6" ]
                            [ label [ for "man_elo" ] [ text "ECO" ]
                            , input
                                [ name "eco"
                                , type_ "text"
                                , onInput (FormFieldChange << EcoChanged)
                                ]
                                []
                            ]
                        ]
                    , button
                        [ class "button"
                        , disabled (not (Model.isModelValid model))
                        ]
                        [ text "Query" ]
                    ]
                ]
            ]
        , div [ class "medium-6 cell" ]
            [ div [ class "callout" ]
                [ text <| Debug.toString model ]
            ]
        ]