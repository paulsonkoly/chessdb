port module MoveExplorer exposing (main)

import Board
import Browser
import Html exposing (Html, div)
import Http
import Loadable exposing (Loadable(..))
import Maybe.Extra as Maybe
import Parser
import Popularities exposing (Popularities)
import Position exposing (Position)
import Url.Builder as Url


port signalFenChanged2 : String -> Cmd msg


type alias Model =
    { position : Position
    , popularities : Loadable Popularities
    }


type
    Msg
    --    = SetPosition String Int Int (Maybe Int)
    --    | PopularitiesReceived (Result Http.Error Popularities)
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


init : () -> ( Model, Cmd Msg )
init () =
    let
        model =
            { position = Position.init
            , popularities = Loading
            }
    in
    ( model, cmdFetchPopularitiesFor Position.init )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ------------------------------------------------------------------------
        PopularitiesReceived receivedData ->
            ( { model | popularities = Loaded receivedData }, Cmd.none )

        ------------------------------------------------------------------------
        -- SetPosition fenPosition castlingAvailability activeColour enPassant ->
        --     let
        --         newModel =
        --             { model
        --                 | fenPosition = fenPosition
        --                 , castlingAvailability = castlingAvailability
        --                 , activeColour = activeColour
        --                 , enPassant = enPassant
        --                 , popularities = Loading
        --             }
        --     in
        --     ( newModel
        --     , Cmd.batch
        --         [ signalFenChanged2 fenPosition
        --         , cmdFetchPopularitiesFor newModel
        --         ]
        --     )
        PopularitiesEvent (Popularities.MoveClicked san) ->
            let
                eMove =
                    Parser.run Board.moveParser san
            in
            case eMove of
                Ok move ->
                    let
                        ePosition =
                            Position.make move model.position
                    in
                    case ePosition of
                        Ok newPosition ->
                            ( { position = newPosition, popularities = Loading }
                            , Cmd.batch
                                [ signalFenChanged2 (Position.fen newPosition)
                                , cmdFetchPopularitiesFor newPosition
                                ]
                            )

                        Err _ ->
                            ( model, Cmd.none )

                Err oops ->
                    -- TODO
                    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    Html.map PopularitiesEvent (Popularities.view model.popularities)
