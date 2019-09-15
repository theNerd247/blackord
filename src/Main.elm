import Grid exposing (Grid)
import Grid.Position exposing (Position)
import Grid.Direction exposing (Direction(..), Coord)
import PixelEngine.Options as Options exposing (Options)
import PixelEngine
    exposing
        ( Area
        , Input(..)
        , PixelEngine
        , game
        )
import PixelEngine.Options exposing (Options)
import PixelEngine.Tile as Tile exposing (Tile)

type alias Player = 
  { position : Position
  }

type alias Model = 
  { player : Player
  }

type Msg = MovePlayer Direction

boardSize : Int 
boardSize = 30

tileSize : Int
tileSize = 30

width : Float
width = toFloat <| boardSize * tileSize

let initPlayer = { position = (0, 0) }

let initModel = { player = { position = initPosition })

init : () -> (Model, Cmd Msg)
init _ = (initModel, Cmd.none)


controls : Input -> Maybe Msg
controls input =
    case input of
        InputUp ->
            Just <| Look Up

        InputDown ->
            Just <| Look Down

        InputLeft ->
            Just <| Look Left

        InputRight ->
            Just <| Look Right

        _ ->
            Nothing

movePlayer : Direction -> Player -> Player
movePlayer direction player =
    let
        dirVec : Coord
        dirVec =
            direction |> Position.fromDirection

        newPos : Position
        newPos =
            player.position
                |> Position.add dirVec
                |> Tuple.mapBoth (modBy boardSize) (modBy boardSize)
    in
      player { position = newPos }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
  case msg of
    MovePlayer d -> (model { player = movePlayer d model.player }, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions = Sub.none

main : PixelEngine () Model Msg
main = game 
  { init = init
  , update = update
  , subscriptions = subscriptions
  , view
  , controls = controls
  , width = width
  }
