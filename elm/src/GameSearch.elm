module GameSearch exposing (main)

import Browser
import Browser.Navigation as Browser
import Date exposing (Date)
import DatePicker exposing (DateEvent(..))
import Debug
import Game exposing (..)
import Game.Decoder exposing (gamePropertiesDecoder)
import GameSearch.Model as Model exposing (Model, init, validateModel)
import GameSearch.Msg exposing (Msg(..))
import GameSearch.View exposing (view)
import Html exposing (Html)
import Http
import Json.Encode as Encode
import Loadable exposing (..)
import PaginatedList exposing (paginatedListDecoder)
import Url.Builder as Url


main =
    Browser.element
        { init = Model.init
        , subscriptions = always Sub.none
        , update = update
        , view = view
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FormFieldChange f ->
            ( model |> Model.updateModel f |> Model.validateModel, Cmd.none )

        SetDatePicker datePicked ->
            let
                ( newDatePicker, dateEvent ) =
                    DatePicker.update
                        Model.datePickerSettings
                        datePicked
                        model.datePicker

                date =
                    case dateEvent of
                        Picked newDate ->
                            Just newDate

                        _ ->
                            model.date
            in
            ( { model | date = date, datePicker = newDatePicker }, Cmd.none )

        FormSubmitted ->
            let
                gamesDecoder =
                    paginatedListDecoder gamePropertiesDecoder
            in
            if Model.isModelValid model then
                ( model
                , Http.post
                    { url = Url.absolute [ "games", "search" ] []
                    , body = Http.jsonBody (Model.jsonEncodedQuery model)
                    , expect = Http.expectJson GamesReceived gamesDecoder
                    }
                )

            else
                ( model, Cmd.none )

        PaginationRequested (PaginatedList.PaginationRequest offset) ->
            let
                newGames =
                    Loadable.map (PaginatedList.setOffset offset) model.games
            in
            update FormSubmitted { model | games = newGames }

        GamesReceived games ->
            ( { model | games = Loaded games }, Cmd.none )

        GameLoadRequested id ->
            ( model
            , Browser.load (Url.absolute [ "games", String.fromInt id ] [])
            )
