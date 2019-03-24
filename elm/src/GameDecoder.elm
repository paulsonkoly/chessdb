module GameDecoder exposing (game)

import Json.Decode exposing (..)
import Game exposing (..)

--------------------------------------------------------------------------------
-- | Json decoder for games
game : Decoder Game
game =
  map2 Game
    (succeed ()) -- TODO
    (field "moves" (array <| moveDecoder))


moveDecoder : Decoder Move
moveDecoder =
  map8 Move
    (field "id" int)
    (field "fen_position" string)
    (field "san" string)
    (field "active_colour" colourDecoder)
    (field "fullmove_number" int)
    (field "castling_availability" int)
    (field "halfmove_clock" int)
    (field "en_passant" (nullable <| int))


colourDecoder : Decoder Colour
colourDecoder =
  let
      intToColour n =
        case n of
          0 -> succeed White
          1 -> succeed Black
          _ -> fail ""
  in int |> andThen intToColour
