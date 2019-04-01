module GameSearch.Msg exposing
    ( FieldChange(..)
    , Msg(..)
    , ServerResponse(..)
    , jsonResponseDecoder
    )

import DatePicker
import Game exposing (GameProperties)
import Game.Decoder exposing (gamePropertiesDecoder)
import Http
import Json.Decode as Decode exposing (Decoder)
import Pagination exposing (Pagination)


type FieldChange
    = WhiteChanged String
    | BlackChanged String
    | EitherColourChanged String
    | OpponentChanged String
    | MinimumEloChanged String
    | MaxiumEloChanged String
    | EventChanged String
    | SiteChanged String
    | RoundChanged String
    | ResultChanged String
    | EcoChanged String


type ServerResponse
    = ServerResponse
        { games : List GameProperties
        , pagination : Pagination
        }


jsonResponseDecoder : Decoder ServerResponse
jsonResponseDecoder =
    Decode.map2 (\a b -> ServerResponse { games = a, pagination = b })
        (Decode.field "data" (Decode.list gamePropertiesDecoder))
        Pagination.decoder


type Msg
    = FormFieldChange FieldChange
    | FormSubmitted
    | SetDatePicker DatePicker.Msg
    | PaginationRequested Pagination.Msg
    | GamesReceived (Result Http.Error ServerResponse)
    | GameLoadRequested Int
