import Color exposing (rgb255)
import Grid exposing (Grid)
import Grid.Position as Position exposing (Position, Coord)
import Grid.Direction exposing (Direction(..))
import PixelEngine.Options as Options exposing (Options)
import PixelEngine
    exposing
        ( Area
        , Input(..)
        , PixelEngine
        , game
        , colorBackground
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

initPlayer : Player
initPlayer = { position = (0, 0) }

initModel : Model
initModel = { player = initPlayer }

init : () -> (Model, Cmd Msg)
init _ = (initModel, Cmd.none)


controls : Input -> Maybe Msg
controls input =
    case input of
        InputUp ->
            Just <| MovePlayer Up

        InputDown ->
            Just <| MovePlayer Down

        InputLeft ->
            Just <| MovePlayer Left

        InputRight ->
            Just <| MovePlayer Right

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
      { player | position = newPos }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
  case msg of
    MovePlayer d -> ({ model | player = movePlayer d model.player }, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none

options : Options Msg
options =
    Options.default
        |> Options.withMovementSpeed 0.8

viewPlayer : Player -> List (Position, Tile msg)
viewPlayer player = 
  [ (player.position, (0,0) |> Tile.fromPosition |> Tile.movable "player")
  ]

areas : Model -> List (Area Msg)
areas { player } =
    [ PixelEngine.tiledArea
        { rows = boardSize
        , tileset =
            { source       = "img/man.png"
            , spriteWidth  = tileSize
            , spriteHeight = tileSize
            }
        , background = colorBackground (rgb255 255 255 255)
        }
        ( viewPlayer player 
        )
    ]

view :
    Model
    -> { title : String, options : Maybe (Options Msg), body : List (Area Msg) }
view model =
    { title = "BlackOrd"
    , options = Just options
    , body = areas model
    }

main : PixelEngine () Model Msg
main = game 
  { init = init
  , update = update
  , subscriptions = subscriptions
  , view = view
  , controls = controls
  , width = width
  }
