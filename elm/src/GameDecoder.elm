module GameDecoder exposing
  ( gameDecoder
  , popularitiesDecoder
  )


import Json.Decode exposing (..)
import Game exposing (..)


--------------------------------------------------------------------------------
-- | Json decoder for games
gameDecoder : Decoder Game
gameDecoder =
  map2 Game
    (succeed ()) -- TODO
    (field "moves" (array <| moveDecoder))


moveDecoder : Decoder Move
moveDecoder =
  map8 Move
    (field "id" int)
    (field "fen_position" string)
    (field "san" string)
    (field "active_colour" int)
    (field "fullmove_number" int)
    (field "castling_availability" int)
    (field "halfmove_clock" int)
    (field "en_passant" (nullable <| int))


--------------------------------------------------------------------------------
-- | Json decoder for popularity stats
popularitiesDecoder : Decoder Popularities
popularitiesDecoder =
  map2 Popularities
    (field "token" int)
    (field "moves" (list popularityItemDecoder))


popularityItemDecoder : Decoder PopularityItem
popularityItemDecoder =
  map5 (\a b c d e -> { nextSan = a, whiteWon = b, blackWon = c, draw = d, totalCount = e})
    (field "next_san" string)
    (field "white_won" int)
    (field "black_won" int)
    (field "draw" int)
    (field "total_count" int)
