module Game.Decoder exposing
    ( gameDecoder
    , popularitiesDecoder
    )

import Game exposing (..)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline


gameDecoder : Decoder Game
gameDecoder =
    Decode.succeed Game
        |> Pipeline.hardcoded ()
        -- TODO
        |> Pipeline.required "moves" (Decode.array <| moveDecoder)


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
