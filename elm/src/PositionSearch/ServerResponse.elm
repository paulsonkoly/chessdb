module PositionSearch.ServerResponse exposing (ServerResponse, jsonDecoder)

import Game.Decoder as Game
import Json.Decode
import Pagination exposing (Pagination)
import PositionSearch.Model as Model


type alias ServerResponse =
    { games : List Model.GameAtMove
    , pagination : Pagination
    }


jsonGameAtMoveDecoder : Json.Decode.Decoder Model.GameAtMove
jsonGameAtMoveDecoder =
    Json.Decode.map2 Model.GameAtMove
        Game.gamePropertiesDecoder
        (Json.Decode.field "fullmove_number" Json.Decode.int)


jsonDecoder : Json.Decode.Decoder ServerResponse
jsonDecoder =
    Json.Decode.map2 ServerResponse
        (Json.Decode.field "data" (Json.Decode.list jsonGameAtMoveDecoder))
        Pagination.jsonDecoder
