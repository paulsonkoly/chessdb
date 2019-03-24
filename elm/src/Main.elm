import Browser
import Platform.Sub
import Platform.Cmd
import Array exposing(Array)
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Http
import Json.Decode exposing (Decoder, field, array, string, int, map2, map8, nullable, succeed, fail, andThen)

--------------------------------------------------------------------------------

main =
  Browser.element { init = init, subscriptions = subscriptions, update = update, view = view }

--------------------------------------------------------------------------------
-- Model
type Colour = White | Black

type alias Move =
  { id : Int
  , fenPosition : String
  , san : String
  , activeColour : Colour
  , fullMoveNumber : Int
  , castlingAvailability : Int -- TODO
  , halfmoveClock : Int
  , enPassant : Maybe Int -- TODO
  }

type alias Game =
  { todoGameProperties : ()
  , moves : Array Move
  }

type Model = Loading | Loaded Game Int

type Msg
  = Increment
  | Decrement
  | Received (Result Http.Error Game)

--------------------------------------------------------------------------------
init : Int -> (Model, Cmd Msg)
init id =
  ( Loading
  , Http.get
    { url = "games/" ++ (String.fromInt id) ++ ".json"
    , expect = Http.expectJson Received game
    }
  )

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
    (field "en_passant" int |> nullable)

colourDecoder : Decoder Colour
colourDecoder =
  let
      intToColour n =
        case n of
          0 -> succeed White
          1 -> succeed Black
          _ -> fail ""
  in int |> andThen intToColour


subscriptions = always Platform.Sub.none

--------------------------------------------------------------------------------
update msg model = (model, Platform.Cmd.none)

view model =
  div []
    [ button [ onClick Decrement ] [ text "-" ]
    , button [ onClick Increment ] [ text "+" ]
    ]
