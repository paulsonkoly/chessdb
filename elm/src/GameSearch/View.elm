module GameSearch.View exposing (view)

import Date
import DatePicker
import FormError exposing (Error)
import Game exposing (GameProperties)
import GameSearch.Model as Model exposing (Model)
import GameSearch.Msg as Msg exposing (FieldChange(..), Msg(..))
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
import Pagination exposing (Pagination)
import Url.Builder as Url


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
                    ++ FormError.errorAttribute err
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
                    ++ FormError.errorAttribute err
                )
                []
            ]
        , div [ class "cell medium-12" ] [ FormError.viewError err ]
        ]


viewDateFields : Model.FormFields -> Html Msg
viewDateFields fields =
    let
        settings =
            Model.datePickerSettings

        newSettings fieldError =
            { settings
                | inputAttributes =
                    settings.inputAttributes
                        ++ FormError.errorAttribute fields.datesDontMatch
                        ++ FormError.errorAttribute (fieldError fields)
            }
    in
    div [ class "grid-x grid-padding-x" ]
        [ div [ class "cell medium-6" ]
            [ label [ for "date" ] [ text "From date" ]
            , DatePicker.view
                fields.fromDate
                (newSettings .fromDateError)
                fields.fromDatePicker
                |> Html.map FromDatePicked
            , FormError.viewError fields.fromDateError
            ]
        , div [ class "cell medium-6" ]
            [ label [ for "date" ] [ text "To date" ]
            , DatePicker.view
                fields.toDate
                (newSettings .toDateError)
                fields.toDatePicker
                |> Html.map ToDatePicked
            , FormError.viewError fields.toDateError
            ]
        , div [ class "cell medium-12" ]
            [ FormError.viewError fields.datesDontMatch ]
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


viewGameProperties : List GameProperties -> Html Msg
viewGameProperties games =
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


viewForm : Model.FormFields -> Html Msg
viewForm fields =
    form [ onSubmit FormSubmitted ]
        [ div [ class "grid-x grid-padding-x" ]
            [ div [ class "cell medium-6" ]
                [ label [ for "white" ] [ text "White" ]
                , input
                    [ name "white"
                    , type_ "text"
                    , disabled (Model.hasEitherOrOpponent fields)
                    , onInput (FormFieldChange << WhiteChanged)
                    ]
                    []
                ]
            , div [ class "cell medium-6" ]
                [ label [ for "black" ] [ text "Black" ]
                , input
                    [ name "black"
                    , type_ "text"
                    , disabled (Model.hasEitherOrOpponent fields)
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
                    , disabled (Model.hasWhiteOrBlack fields)
                    , onInput (FormFieldChange << EitherColourChanged)
                    ]
                    []
                ]
            , div [ class "cell medium-6" ]
                [ label [ for "opponent" ] [ text "Opponent" ]
                , input
                    [ name "opponent"
                    , type_ "text"
                    , disabled (Model.hasWhiteOrBlack fields)
                    , onInput (FormFieldChange << OpponentChanged)
                    ]
                    []
                ]
            ]
        , viewEloFields fields.elosDontMatch
        , viewDateFields fields
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
                        ++ FormError.errorAttribute fields.ecoInvalid
                    )
                    []
                , FormError.viewError fields.ecoInvalid
                ]
            ]
        , button
            [ class "button"
            , disabled (not (Model.areFieldsValid fields))
            ]
            [ text "Query" ]
        ]


view : Model -> Html Msg
view model =
    div [ class "grid-x grid-margin-x" ]
        [ div [ class "medium-6 cell" ]
            [ div [ class "callout" ] [ viewForm model.formFields ] ]
        , div [ class "medium-6 cell" ]
            [ Loadable.viewLoadable model.games viewGameProperties (text "")
            , Html.map PaginationRequested (Pagination.view model.pagination)
            ]
        ]
