module GameSearch exposing (main)

import Browser
import Browser.Navigation as Browser
import Date exposing (Date)
import DatePicker exposing (DateEvent(..))
import Debug
import Game exposing (..)
import Game.Decoder exposing (gamePropertiesDecoder)
import GameSearch.Model as Model exposing (Model, init, validateModel)
import GameSearch.Msg as Msg exposing (Msg(..))
import GameSearch.View exposing (view)
import Html exposing (Html)
import Json.Encode as Encode
import Loadable exposing (..)
import Pagination exposing (paginationDecoder)
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
            if Model.isModelValid model then
                ( model, Model.sendQuery model )

            else
                ( model, Cmd.none )

        PaginationRequested (Pagination.Request offset) ->
            let
                newPagination =
                    model.pagination |> (\p -> { p | offset = offset })

                newModel =
                    { model | pagination = newPagination }
            in
            update FormSubmitted newModel

        GamesReceived (Ok (Msg.ServerResponse { games, offset, count })) ->
            ( { model
                | games = Loaded (Ok games)
                , pagination = { offset = offset, count = count }
              }
            , Cmd.none
            )

        GamesReceived (Err oops) ->
            ( { model
                | games = Loaded (Err oops)
                , pagination = { offset = 0, count = 0 }
              }
            , Cmd.none
            )

        GameLoadRequested id ->
            ( model
            , Browser.load (Url.absolute [ "games", String.fromInt id ] [])
            )
