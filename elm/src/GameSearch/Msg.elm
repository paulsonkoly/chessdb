module GameSearch.Msg exposing (FieldChange(..), Msg(..))


type FieldChange
    = WhiteChanged String
    | BlackChanged String
    | EitherColourChanged String
    | MinimumEloChanged String
    | MaxiumEloChanged String
    | EventChanged String
    | SiteChanged String
    | DateChanged String
    | RoundChanged String
    | ResultChanged String
    | EcoChanged String


type Msg
    = FormFieldChange FieldChange
    | FormSubmitted
