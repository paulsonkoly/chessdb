module GameSearch.Model exposing
    ( Error
    , FormFields
    , Model
    , areFieldsValid
    , datePickerSettings
    , hasEitherOrOpponent
    , hasWhiteOrBlack
    , init
    , jsonEncodedFields
    , sendQuery
    , updateFields
    , validateFields
    )

import Date exposing (Date)
import DatePicker exposing (DatePicker)
import Game exposing (GameProperties, Outcome(..))
import GameSearch.Msg as Msg exposing (FieldChange(..), Msg(..))
import Http
import Json.Encode as Encode exposing (Value)
import Loadable exposing (Loadable(..))
import Pagination exposing (Pagination)
import Url.Builder as Url


type alias Error =
    Maybe String


type alias FormFields =
    { white : String
    , black : String
    , eitherColour : String
    , opponent : String
    , elosDontMatch : Error
    , minimumElo : Maybe Int
    , maximumElo : Maybe Int
    , event : String
    , site : String
    , fromDate : Maybe Date
    , fromDatePicker : DatePicker
    , toDate : Maybe Date
    , toDatePicker : DatePicker
    , datesDontMatch : Error
    , round : String
    , result : String
    , ecoInvalid : Error
    , eco : String
    }


type alias Model =
    { formFields : FormFields
    , queriedFields : FormFields
    , pagination : Pagination
    , games : Loadable (List GameProperties)
    }


datePickerSettings : DatePicker.Settings
datePickerSettings =
    let
        default =
            DatePicker.defaultSettings
    in
    { default | dateFormatter = Date.format "yyyy-MM-dd" }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        ( datePicker, datePickerCmd ) =
            DatePicker.init

        newFields =
            { white = ""
            , black = ""
            , eitherColour = ""
            , opponent = ""
            , elosDontMatch = Nothing
            , minimumElo = Nothing
            , maximumElo = Nothing
            , event = ""
            , site = ""
            , fromDate = Nothing
            , fromDatePicker = datePicker
            , toDate = Nothing
            , toDatePicker = datePicker
            , datesDontMatch = Nothing
            , round = ""
            , result = ""
            , ecoInvalid = Nothing
            , eco = ""
            }

        model =
            { formFields = newFields
            , queriedFields = newFields
            , pagination = Pagination.init
            , games = Loading
            }
    in
    ( model
    , Cmd.batch
        [ Cmd.map FromDatePicked datePickerCmd
        , Cmd.map ToDatePicked datePickerCmd
        , sendQuery model.queriedFields model.pagination
        ]
    )


sendQuery : FormFields -> Pagination -> Cmd Msg
sendQuery fields pagination =
    Http.post
        { url = Url.absolute [ "games", "search" ] []
        , body = Http.jsonBody (jsonEncodedFields fields pagination)
        , expect = Http.expectJson GamesReceived Msg.jsonResponseDecoder
        }


validateFields : FormFields -> FormFields
validateFields fields =
    let
        compareElos =
            Maybe.map2 (<=) fields.minimumElo fields.maximumElo

        elosDontMatch =
            case compareElos of
                Just False ->
                    Just "Minimum ELO can't be larger than maximum elo"

                _ ->
                    Nothing

        ecoInvalid =
            case String.toList fields.eco of
                [ a, b, c ] ->
                    if 'A' <= a && a <= 'E' && '0' <= b && b <= '9' && '0' <= c && c <= '9' then
                        Nothing

                    else
                        Just "Invalid ECO code."

                [] ->
                    Nothing

                _ ->
                    Just "Invalid ECO code."

        datesDontMatch =
            case Maybe.map2 Date.compare fields.fromDate fields.toDate of
                Just GT ->
                    Just "From date can't be after To date"

                _ ->
                    Nothing
    in
    { fields
        | elosDontMatch = elosDontMatch
        , ecoInvalid = ecoInvalid
        , datesDontMatch = datesDontMatch
    }


areFieldsValid : FormFields -> Bool
areFieldsValid fields =
    fields.elosDontMatch
        == Nothing
        && fields.ecoInvalid
        == Nothing
        && fields.datesDontMatch
        == Nothing


hasWhiteOrBlack : FormFields -> Bool
hasWhiteOrBlack fields =
    fields.white /= "" || fields.black /= ""


hasEitherOrOpponent : FormFields -> Bool
hasEitherOrOpponent fields =
    fields.eitherColour /= "" || fields.opponent /= ""


updateFields : FieldChange -> FormFields -> FormFields
updateFields msg fields =
    case msg of
        WhiteChanged str ->
            { fields | white = str }

        BlackChanged str ->
            { fields | black = str }

        EitherColourChanged str ->
            { fields | eitherColour = str }

        OpponentChanged str ->
            { fields | opponent = str }

        MinimumEloChanged str ->
            { fields | minimumElo = String.toInt str }

        MaxiumEloChanged str ->
            { fields | maximumElo = String.toInt str }

        EventChanged str ->
            { fields | event = str }

        SiteChanged str ->
            { fields | site = str }

        RoundChanged str ->
            { fields | round = str }

        ResultChanged str ->
            { fields | result = str }

        EcoChanged str ->
            { fields | eco = str }


jsonEncodedFields : FormFields -> Pagination -> Encode.Value
jsonEncodedFields fields pagination =
    let
        stringQuery ( name, str ) =
            case str of
                "" ->
                    Nothing

                _ ->
                    Just ( name, Encode.string str )

        numberQuery ( name, mint ) =
            mint |> Maybe.map (\int -> ( name, Encode.int int ))

        dateQuery ( name, mdate ) =
            mdate
                |> Maybe.map
                    (\date -> ( name, Encode.string (Date.toIsoString date) ))
    in
    Encode.object
        (List.filterMap stringQuery
            [ ( "white", fields.white )
            , ( "black", fields.black )
            , ( "either_colour", fields.eitherColour )
            , ( "opponent", fields.opponent )
            , ( "event", fields.event )
            , ( "site", fields.site )
            , ( "round", fields.round )
            , ( "eco", fields.eco )
            , ( "result", fields.result )
            ]
            ++ List.filterMap numberQuery
                [ ( "minimum_elo", fields.minimumElo )
                , ( "maximum_elo", fields.maximumElo )
                ]
            ++ List.filterMap dateQuery
                [ ( "from_date", fields.fromDate )
                , ( "to_date", fields.toDate )
                ]
            ++ Pagination.encode pagination
        )
