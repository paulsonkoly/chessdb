module GameSearch.Msg exposing (FieldChange(..), Msg(..))

import DatePicker
import Game exposing (GameProperties)
import Http


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
    | GamesReceived (Result Http.Error (List GameProperties))
    | GameLoadRequested Int
