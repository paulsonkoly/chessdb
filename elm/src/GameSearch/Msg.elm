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
import Pagination


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
        , offset : Int
        , count : Int
        }


jsonResponseDecoder : Decoder ServerResponse
jsonResponseDecoder =
    Decode.map3 (\a b c -> ServerResponse { games = a, offset = b, count = c })
        (Decode.field "data" (Decode.list gamePropertiesDecoder))
        (Decode.field "offset" Decode.int)
        (Decode.field "count" Decode.int)


type Msg
    = FormFieldChange FieldChange
    | FormSubmitted
    | SetDatePicker DatePicker.Msg
    | PaginationRequested Pagination.Msg
    | GamesReceived (Result Http.Error ServerResponse)
    | GameLoadRequested Int
