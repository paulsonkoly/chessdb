module Game.Decoder exposing
    ( gameDecoder
    , gamesDecoder
    , popularitiesDecoder
    )

import Date exposing (Date)
import Game exposing (..)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline


gameDecoder : Decoder Game
gameDecoder =
    Decode.succeed Game
        |> Pipeline.hardcoded ()
        -- TODO
        |> Pipeline.required "moves" (Decode.array <| moveDecoder)


gamesDecoder : Decoder (List GameProperties)
gamesDecoder =
    Decode.list gamePropertiesDecoder


outcomeDecoder : Decoder Outcome
outcomeDecoder =
    let
        whoWon exp outcome =
            Decode.int
                |> Decode.andThen
                    (\i ->
                        if i == exp then
                            Decode.succeed outcome

                        else
                            Decode.fail ""
                    )
    in
    Decode.oneOf [ whoWon 0 WhiteWon, whoWon 1 BlackWon, whoWon 2 Draw ]


dateDecoder : Decoder Date
dateDecoder =
    Decode.string
        |> Decode.andThen
            (\dateString ->
                case Date.fromIsoString dateString of
                    Ok date ->
                        Decode.succeed date

                    Err oops ->
                        Decode.fail oops
            )


gamePropertiesDecoder : Decoder GameProperties
gamePropertiesDecoder =
    Decode.succeed GameProperties
        |> Pipeline.required "id" Decode.int
        |> Pipeline.required "white" Decode.string
        |> Pipeline.required "black" Decode.string
        |> Pipeline.required "white_elo" Decode.int
        |> Pipeline.required "black_elo" Decode.int
        |> Pipeline.required "event" Decode.string
        |> Pipeline.required "site" Decode.string
        |> Pipeline.required "date" (Decode.nullable dateDecoder)
        |> Pipeline.required "round" Decode.string
        |> Pipeline.required "result" outcomeDecoder
        |> Pipeline.required "eco" Decode.string


moveDecoder : Decoder Move
moveDecoder =
    Decode.succeed Move
        |> Pipeline.required "id" Decode.int
        |> Pipeline.required "fen_position" Decode.string
        |> Pipeline.required "san" Decode.string
        |> Pipeline.required "active_colour" Decode.int
        |> Pipeline.required "fullmove_number" Decode.int
        |> Pipeline.required "castling_availability" Decode.int
        |> Pipeline.required "halfmove_clock" Decode.int
        |> Pipeline.required "en_passant" (Decode.nullable Decode.int)


popularitiesDecoder : Decoder Popularities
popularitiesDecoder =
    Decode.succeed Popularities
        |> Pipeline.required "token" Decode.int
        |> Pipeline.required "moves" (Decode.list popularityItemDecoder)


popularityItemDecoder : Decoder PopularityItem
popularityItemDecoder =
    Decode.succeed PopularityItem
        |> Pipeline.required "next_san" Decode.string
        |> Pipeline.required "white_won" Decode.int
        |> Pipeline.required "black_won" Decode.int
        |> Pipeline.required "draw" Decode.int
        |> Pipeline.required "total_count" Decode.int
