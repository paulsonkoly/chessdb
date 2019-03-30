module GameSearch.Model exposing
    ( Error
    , Model
    , init
    , isModelValid
    , jsonEncodedQuery
    , updateModel
    , validateModel
    )

import Date exposing (Date)
import Game exposing (GameProperties, Outcome(..))
import GameSearch.Msg exposing (FieldChange(..), Msg(..))
import Json.Encode as Encode exposing (Value)
import Loadable exposing (Loadable(..))


type alias Error =
    Maybe String


type Either a b
    = Left a
    | Right b


type Decisive
    = Decisive


type alias Model =
    { white : String
    , black : String
    , eitherColour : String
    , elosDontMatch : Error
    , minimumElo : Maybe Int
    , maximumElo : Maybe Int
    , event : String
    , site : String
    , date : Maybe Date
    , round : String
    , result : String
    , ecoError : Error
    , eco : String
    , games : Loadable (List GameProperties)
    }


resultFromString : String -> Maybe (Either Decisive Outcome)
resultFromString str =
    case str of
        "Decisive" ->
            Just (Left Decisive)

        _ ->
            Game.outcomeFromString str
                |> Maybe.map Right


init : Model
init =
    { white = ""
    , black = ""
    , eitherColour = ""
    , elosDontMatch = Nothing
    , minimumElo = Nothing
    , maximumElo = Nothing
    , event = ""
    , site = ""
    , date = Nothing
    , round = ""
    , result = ""
    , ecoError = Nothing
    , eco = ""
    , games = Loading
    }


validateModel : Model -> Model
validateModel model =
    let
        compareElos =
            Maybe.map2 (<=) model.minimumElo model.maximumElo

        elosDontMatch =
            case compareElos of
                Just False ->
                    Just "Minimum ELO can't be larger than maximum elo"

                _ ->
                    Nothing
    in
    { model
        | elosDontMatch = elosDontMatch
        , ecoError = Nothing -- TODO
    }


isModelValid : Model -> Bool
isModelValid model =
    model.elosDontMatch == Nothing && model.ecoError == Nothing


updateModel : FieldChange -> Model -> Model
updateModel msg model =
    case msg of
        WhiteChanged str ->
            { model | white = str }

        BlackChanged str ->
            { model | black = str }

        EitherColourChanged str ->
            { model | eitherColour = str }

        MinimumEloChanged str ->
            { model | minimumElo = String.toInt str }

        MaxiumEloChanged str ->
            { model | maximumElo = String.toInt str }

        EventChanged str ->
            { model | event = str }

        SiteChanged str ->
            { model | site = str }

        DateChanged str ->
            { model | date = Date.fromIsoString str |> Result.toMaybe }

        RoundChanged str ->
            { model | round = str }

        ResultChanged str ->
            { model | result = str }

        EcoChanged str ->
            { model | eco = str }


jsonEncodedQuery : Model -> Encode.Value
jsonEncodedQuery model =
    let
        stringQuery ( name, str ) =
            case str of
                "" ->
                    Nothing

                _ ->
                    Just ( name, Encode.string str )

        numberQuery ( name, mint ) =
            mint |> Maybe.andThen (\int -> Just ( name, Encode.int int ))
    in
    Encode.object
        (List.filterMap stringQuery
            [ ( "white", model.white )
            , ( "black", model.black )
            , ( "either_colour", model.eitherColour )
            , ( "event", model.event )
            , ( "site", model.site )
            , ( "round", model.round )
            , ( "eco", model.eco )
            , ( "result", model.result )
            ]
            ++ List.filterMap numberQuery
                [ ( "minimum_elo", model.minimumElo )
                , ( "maximum_elo", model.maximumElo )
                ]
        )
