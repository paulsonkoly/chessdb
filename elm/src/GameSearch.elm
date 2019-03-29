module GameSearch exposing (main)

import Browser
import Date exposing (Date)
import Game exposing (..)
import Game.Decoder exposing (gamesDecoder)
import GameSearch.Model as Model exposing (Model, init, validateModel)
import GameSearch.Msg exposing (Msg(..))
import GameSearch.View exposing (view)
import Html exposing (Html)
import Http
import Json.Encode as Encode
import Loadable exposing (..)
import Url.Builder as Url


main =
    Browser.element
        { init = init
        , subscriptions = always Sub.none
        , update = update
        , view = view
        }


init : () -> ( Model, Cmd Msg )
init flags =
    ( Model.init, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FormFieldChange f ->
            ( model |> Model.updateModel f |> Model.validateModel, Cmd.none )

        FormSubmitted ->
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

        GamesReceived games ->
            ( model, Cmd.none )
