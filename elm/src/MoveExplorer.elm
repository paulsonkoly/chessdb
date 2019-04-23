port module MoveExplorer exposing (main)

import Board
import Browser
import FontAwesome.Icon as I
import FontAwesome.Solid as S
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class, id)
import Html.Events exposing (onClick)
import Http
import Loadable exposing (Loadable(..))
import Maybe.Extra as Maybe
import Parser
import Popularities exposing (Popularities)
import Position exposing (Position)
import Url.Builder as Url


port signalDomRendered2 : () -> Cmd msg


port signalFenChanged2 : String -> Cmd msg


type alias Errorable a =
    Result String a


type alias Model =
    { positions : Errorable (List Position)
    , token : Int
    , popularities : Loadable Popularities
    }


type Msg
    = PopularitiesReceived (Result Http.Error Popularities)
    | PopularitiesEvent Popularities.Msg
    | LeftButtonClicked
    | ResetButtonClicked


main =
    Browser.element
        { init = init
        , subscriptions = always Sub.none
        , update = update
        , view = view
        }


cmdFetchPopularitiesFor : Int -> Position -> Cmd Msg
cmdFetchPopularitiesFor token position =
    let
        url =
            Url.absolute [ "moves", "popularities.json" ]
                (Url.int "token" token :: Position.urlEncode position)
    in
    Http.get
        { url = url
        , expect =
            Http.expectJson PopularitiesReceived Popularities.decoder
        }


cmdChangeFen : Int -> Position -> Cmd Msg
cmdChangeFen token position =
    Cmd.batch
        [ signalFenChanged2 (Position.fen position)
        , cmdFetchPopularitiesFor token position
        ]


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

        positions =
            Result.map (\p -> [ p ]) position

        model =
            { positions = positions, token = 1, popularities = Loading }
    in
    case position of
        Ok p ->
            ( model
            , Cmd.batch
                [ signalDomRendered2 ()
                , cmdFetchPopularitiesFor 1 p
                ]
            )

        Err _ ->
            ( model, Cmd.none )


last : List a -> Maybe a
last l =
    case l of
        [] ->
            Nothing

        [ x ] ->
            Just x

        x :: xs ->
            last xs


makeMove :
    Errorable Board.Move
    -> Errorable (List Position)
    -> Errorable (List Position)
makeMove eM eP =
    case eP of
        Ok (p :: ps) ->
            eM
                |> Result.andThen (\m -> Position.make m p)
                |> Result.map (\np -> np :: p :: ps)

        Ok [] ->
            Err "Empty position list, cannot uncons"

        Err oops ->
            Err oops


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        newToken =
            model.token + 1
    in
    case msg of
        PopularitiesReceived receivedData ->
            let
                valid =
                    receivedData
                        |> Result.toMaybe
                        |> Maybe.map (Popularities.validateToken model.token)
            in
            if valid == Just True then
                ( { model | popularities = Loaded receivedData }, Cmd.none )

            else
                ( model, Cmd.none )

        PopularitiesEvent (Popularities.MoveClicked san) ->
            let
                move =
                    Result.mapError Parser.deadEndsToString <|
                        Parser.run Board.moveParser san

                newPositions =
                    makeMove move model.positions

                cmds =
                    case newPositions of
                        Ok (p :: ps) ->
                            cmdChangeFen newToken p

                        _ ->
                            Cmd.none
            in
            ( { positions = newPositions
              , token = newToken
              , popularities = Loading
              }
            , cmds
            )

        LeftButtonClicked ->
            case model.positions of
                Ok (_ :: p :: ps) ->
                    ( { model
                        | positions = Ok (p :: ps)
                        , token = newToken
                        , popularities = Loading
                      }
                    , cmdChangeFen newToken p
                    )

                _ ->
                    ( model, Cmd.none )

        ResetButtonClicked ->
            let
                mSize =
                    model.positions
                        |> Result.toMaybe
                        |> Maybe.map (\xs -> List.length xs >= 2)

                mPos =
                    model.positions
                        |> Result.toMaybe
                        |> Maybe.map last
                        |> Maybe.join
            in
            case ( mSize, mPos ) of
                ( Just True, Just p ) ->
                    ( { model
                        | positions = Ok [ p ]
                        , token = newToken
                        , popularities = Loading
                      }
                    , cmdChangeFen newToken p
                    )

                _ ->
                    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    case model.positions of
        Ok _ ->
            div [ class "grid-x", class "grid-padding-x" ]
                [ div [ class "cell", class "medium-6" ]
                    [ Html.map
                        PopularitiesEvent
                        (Popularities.view model.popularities)
                    ]
                , div [ class "cell", class "medium-6" ]
                    [ div [ class "grid-y", class "grid-margin-y" ]
                        [ div [ id "chessboard" ] []
                        , div [ class "cell" ]
                            [ viewButtons ]
                        ]
                    ]
                ]

        Err oops ->
            text oops


viewButtons : Html Msg
viewButtons =
    viewCenterCell <|
        div [ class "button-group" ]
            [ viewButton ResetButtonClicked S.angleDoubleLeft
            , viewButton LeftButtonClicked S.angleLeft
            ]


viewButton : msg -> I.Icon -> Html msg
viewButton msg icon =
    button [ class "button", onClick msg ] [ I.view icon ]


viewCenterCell : Html Msg -> Html Msg
viewCenterCell inner =
    div [ class "grid-x" ]
        [ div [ class "cell", class "auto" ] []
        , div [ class "cell", class "shrink" ] [ inner ]
        , div [ class "cell", class "auto" ] []
        ]
