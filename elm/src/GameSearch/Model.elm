module GameSearch.Model exposing (Model)

import Date exposing (Date)
import Game exposing (GameProperties, Outcome)
import Loadable exposing (Loadable)


type alias Model =
    { white : Maybe String
    , black : Maybe String
    , eitherColour : Maybe String
    , minimumElo : Maybe Int
    , maximumElo : Maybe Int
    , event : Maybe String
    , site : Maybe String
    , date : Maybe Date
    , round : Maybe String
    , result : Maybe Outcome
    , eco : Maybe String
    , games : Loadable (List GameProperties)
    }
