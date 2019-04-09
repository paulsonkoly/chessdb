port module MoveExplorer exposing (main)

import Board
import Browser
import Html exposing (Html, text)
import Http
import Loadable exposing (Loadable(..))
import Maybe.Extra as Maybe
import Parser
import Popularities exposing (Popularities)
import Position exposing (Position)
import Url.Builder as Url


port signalFenChanged2 : String -> Cmd msg


type alias Model =
    { position : Result String Position
    , popularities : Loadable Popularities
    }


type Msg
    = PopularitiesReceived (Result Http.Error Popularities)
    | PopularitiesEvent Popularities.Msg


main =
    Browser.element
        { init = init
        , subscriptions = always Sub.none
        , update = update
        , view = view
        }


cmdFetchPopularitiesFor : Position -> Cmd Msg
cmdFetchPopularitiesFor position =
    let
        url =
            Url.absolute [ "moves", "popularities.json" ]
                (Position.urlEncode position)
    in
    Http.get
        { url = url
        , expect =
            Http.expectJson PopularitiesReceived Popularities.decoder
        }


type alias Flags =
    { fen : String
    , castlingAvailability : Int
    , activeColour : Int
    , enPassant : Maybe Int
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        position =
            Position.specific
                flags.fen
                flags.castlingAvailability
                flags.activeColour
                flags.enPassant

        model =
            { position = position
            , popularities = Loading
            }
    in
    case position of
        Ok p ->
            ( model
            , Cmd.batch
                [ signalFenChanged2 flags.fen
                , cmdFetchPopularitiesFor p
                ]
            )

        Err _ ->
            ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PopularitiesReceived receivedData ->
            ( { model | popularities = Loaded receivedData }, Cmd.none )

        PopularitiesEvent (Popularities.MoveClicked san) ->
            let
                move =
                    Result.mapError Parser.deadEndsToString <|
                        Parser.run Board.moveParser san

                newPosition =
                    Result.map2 Tuple.pair move model.position
                        |> Result.andThen
                            (\( m, p ) -> Position.make m p)

                cmds =
                    case newPosition of
                        Ok position ->
                            Cmd.batch
                                [ signalFenChanged2 (Position.fen position)
                                , cmdFetchPopularitiesFor position
                                ]

                        Err _ ->
                            Cmd.none
            in
            ( { position = newPosition, popularities = Loading }, cmds )


view : Model -> Html Msg
view model =
    case model.position of
        Ok _ ->
            Html.map PopularitiesEvent (Popularities.view model.popularities)

        Err oops ->
            text oops
