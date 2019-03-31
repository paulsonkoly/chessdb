module GameSearch.Msg exposing (FieldChange(..), Msg(..))

import DatePicker
import Game exposing (GameProperties)
import Http
import PaginatedList exposing (PaginatedList(..))


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


type Msg
    = FormFieldChange FieldChange
    | FormSubmitted
    | SetDatePicker DatePicker.Msg
    | PaginationRequested PaginatedList.Msg
    | GamesReceived (Result Http.Error (PaginatedList GameProperties))
    | GameLoadRequested Int
