module PositionSearch.Model exposing
    ( GameAtMove, Model, init
    , jsonEncode
    )

{-| PositionSearch element Model part.


# Types, and constructors

@docs GameAtMove, Model, init


# Conversions

@docs jsonEncode

-}

import FormError exposing (Error(..))
import Game exposing (GameProperties)
import Json.Decode
import Json.Encode
import Loadable exposing (Loadable(..))
import Pagination exposing (Pagination)
import Position exposing (Position)



------------------------------------------------------------------------
--                       Types and constructors                       --
------------------------------------------------------------------------


type alias GameAtMove =
    { game : GameProperties
    , fullmoveNumber : Int
    }


type alias Model =
    { position : Position
    , enPassantStringError : Error
    , pagination : Pagination
    , games : Loadable (List GameAtMove)
    }


init : Model
init =
    { position = Position.empty
    , enPassantStringError = NoError
    , pagination = Pagination.init
    , games = Loading
    }



------------------------------------------------------------------------
--                            Conversions                             --
------------------------------------------------------------------------


{-| JSON encode the relevant parts of the model to be sent to query the server.
-}
jsonEncode : Model -> Json.Encode.Value
jsonEncode model =
    Json.Encode.object
        [ ( "position", Position.jsonEncode model.position )
        , ( "pagination", Pagination.jsonEncode model.pagination )
        ]
