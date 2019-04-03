port module MoveExplorer exposing (main)

import Browser
import Game exposing (Popularities)
import Game.Decoder exposing (popularitiesDecoder)
import Html exposing (div)
import Http
import Loadable exposing (Loadable(..))
import Maybe.Extra as Maybe
import Popularities
import Url.Builder as Url


port signalDomRendered2 : () -> Cmd msg


port signalFenChanged2 : String -> Cmd msg


type alias Model =
    { fenPosition : String
    , castlingAvailability : Int
    , activeColour : Int
    , enPassant : Maybe Int
    , popularities : Loadable Popularities
    }


type Msg
    = SetPosition String Int Int (Maybe Int)
    | PopularitiesReceived (Result Http.Error Popularities)


main =
    Browser.element
        { init = init
        , subscriptions = always Sub.none
        , update = update
        , view = view
        }


cmdFetchPopularitiesFor : Model -> Cmd Msg
cmdFetchPopularitiesFor model =
    let
        url =
            Url.absolute [ "moves", "popularities.json" ]
                (Maybe.values
                    [ Just (Url.string "fen" model.fenPosition)
                    , Just (Url.int "castle" model.castlingAvailability)
                    , Just (Url.int "active_colour" model.activeColour)
                    , model.enPassant |> Maybe.map (Url.int "en_passant")
                    ]
                )
    in
    Http.get
        { url = url
        , expect =
            Http.expectJson PopularitiesReceived popularitiesDecoder
        }


init : () -> ( Model, Cmd Msg )
init () =
    let
        model =
            { fenPosition = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR"
            , castlingAvailability = 15
            , activeColour = 0
            , enPassant = Nothing
            , popularities = Loading
            }
    in
    ( model, cmdFetchPopularitiesFor model )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ------------------------------------------------------------------------
        PopularitiesReceived receivedData ->
            ( { model | popularities = Loaded receivedData }, Cmd.none )

        ------------------------------------------------------------------------
        SetPosition fenPosition castlingAvailability activeColour enPassant ->
            let
                newModel =
                    { model
                        | fenPosition = fenPosition
                        , castlingAvailability = castlingAvailability
                        , activeColour = activeColour
                        , enPassant = enPassant
                        , popularities = Loading
                    }
            in
            ( newModel
            , Cmd.batch
                [ signalFenChanged2 fenPosition
                , cmdFetchPopularitiesFor newModel
                ]
            )


view model =
    Popularities.viewPopularities model.popularities
