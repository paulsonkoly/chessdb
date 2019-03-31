module GameSearch.View exposing (view)

import Date
import DatePicker
import Game exposing (GameProperties)
import GameSearch.Model as Model exposing (Error, Model)
import GameSearch.Msg exposing (FieldChange(..), Msg(..))
import Html
    exposing
        ( Attribute
        , Html
        , button
        , div
        , form
        , input
        , label
        , option
        , select
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
        ( action
        , class
        , disabled
        , for
        , id
        , name
        , type_
        , value
        )
import Html.Events exposing (onClick, onInput, onSubmit)
import Loadable
import Url.Builder as Url


errorAttribute : Error -> List (Attribute msg)
errorAttribute err =
    case err of
        Just _ ->
            [ class "is-invalid-input" ]

        Nothing ->
            []


viewError : Error -> Html Msg
viewError err =
    case err of
        Just oops ->
            span [ class "form-error", class "is-visible" ] [ text oops ]

        Nothing ->
            span [ class "form-error" ] []


viewEloFields : Error -> Html Msg
viewEloFields err =
    div [ class "grid-x grid-padding-x" ]
        [ div [ class "cell medium-6" ]
            [ label [ for "min_elo" ] [ text "Minimum ELO" ]
            , input
                ([ name "min_elo"
                 , type_ "text"
                 , onInput (FormFieldChange << MinimumEloChanged)
                 ]
                    ++ errorAttribute err
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
                    ++ errorAttribute err
                )
                []
            ]
        , div [ class "cell medium-12" ] [ viewError err ]
        ]


viewResultSelect : Html Msg
viewResultSelect =
    select [ onInput (FormFieldChange << ResultChanged) ]
        [ option [ value "Any" ] [ text "Any" ]
        , option [ value "1-0" ] [ text "1-0" ]
        , option [ value "0-1" ] [ text "0-1" ]
        , option [ value "Decisive" ] [ text "Decisive (1-0 or 0-1)" ]
        , option [ value "1/2-1/2" ] [ text "1/2-1/2" ]
        ]


viewLoadedGames : List GameProperties -> Html Msg
viewLoadedGames games =
    let
        header =
            thead []
                [ th [] [ text "White" ]
                , th [] [ text "Black" ]
                , th [] [ text "Result" ]
                , th [] [ text "Date" ]
                ]

        date =
            Maybe.map Date.toIsoString >> Maybe.withDefault "???"

        body =
            tbody [] <|
                List.map
                    (\game ->
                        tr [ onClick (GameLoadRequested game.id) ]
                            [ td [] [ text game.white ]
                            , td [] [ text game.black ]
                            , td [] [ text (Game.outcomeToString game.result) ]
                            , td [] [ text (date game.date) ]
                            ]
                    )
                    games
    in
    table [ class "hover", class "game-list" ] [ header, body ]


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
                                , disabled (Model.hasEitherOrOpponent model)
                                , onInput (FormFieldChange << WhiteChanged)
                                ]
                                []
                            ]
                        , div [ class "cell medium-6" ]
                            [ label [ for "black" ] [ text "Black" ]
                            , input
                                [ name "black"
                                , type_ "text"
                                , disabled (Model.hasEitherOrOpponent model)
                                , onInput (FormFieldChange << BlackChanged)
                                ]
                                []
                            ]
                        ]
                    , div [ class "grid-x grid-padding-x" ]
                        [ div [ class "cell medium-6" ]
                            [ label [ for "either_colour" ]
                                [ text "Either colour" ]
                            , input
                                [ name "either_colour"
                                , type_ "text"
                                , disabled (Model.hasWhiteOrBlack model)
                                , onInput (FormFieldChange << EitherColourChanged)
                                ]
                                []
                            ]
                        , div [ class "cell medium-6" ]
                            [ label [ for "opponent" ] [ text "Opponent" ]
                            , input
                                [ name "opponent"
                                , type_ "text"
                                , disabled (Model.hasWhiteOrBlack model)
                                , onInput (FormFieldChange << OpponentChanged)
                                ]
                                []
                            ]
                        ]
                    , viewEloFields model.elosDontMatch
                    , div [ class "grid-x grid-padding-x" ]
                        [ div [ class "cell medium-6" ]
                            [ label [ for "event" ] [ text "Event" ]
                            , input
                                [ name "event"
                                , type_ "text"
                                , onInput (FormFieldChange << EventChanged)
                                ]
                                []
                            ]
                        , div [ class "cell medium-6" ]
                            [ label [ for "site" ] [ text "Site" ]
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
                            [ label [ for "date" ] [ text "Date" ]
                            , DatePicker.view
                                model.date
                                Model.datePickerSettings
                                model.datePicker
                                |> Html.map SetDatePicker
                            ]
                        , div [ class "cell medium-6" ]
                            [ label [ for "round" ] [ text "Round" ]
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
                            [ label [ for "result" ] [ text "Result" ]
                            , viewResultSelect
                            ]
                        , div [ class "cell medium-6" ]
                            [ label [ for "eco" ] [ text "ECO" ]
                            , input
                                ([ name "eco"
                                 , type_ "text"
                                 , onInput (FormFieldChange << EcoChanged)
                                 ]
                                    ++ errorAttribute model.ecoInvalid
                                )
                                []
                            , viewError model.ecoInvalid
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
            [ Loadable.viewLoadable model.games viewLoadedGames ]
        ]
