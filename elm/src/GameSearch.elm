module GameSearch exposing (main)

import Browser
import Date exposing (Date)
import Game exposing (..)
import GameSearch.Model as Model exposing (Model, init, validateModel)
import GameSearch.Msg exposing (Msg(..))
import GameSearch.View exposing (view)
import Html exposing (Html)
import Loadable exposing (..)


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


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        FormFieldChange f ->
            ( model |> Model.updateModel f |> Model.validateModel, Cmd.none )

        FormSubmitted ->
            if Model.isModelValid model then
                ( model, Cmd.none )

            else
                ( model, Cmd.none )
