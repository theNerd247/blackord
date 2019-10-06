import Grid.Bordered as Grid
import Color exposing (rgb255)
import Grid.Position as Position exposing (Position, Coord)
import Grid.Direction exposing (Direction(..))
import PixelEngine.Options as Options exposing (Options)
import Time
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

type Entity =
    Platform
  | Coin
  | Marker

type alias Platform = Grid.Grid Entity

type alias Model = 
  { player : Player
  , platform : Platform
  }

type Msg = MovePlayer Direction


main : PixelEngine () Model Msg
main = game 
  { init = init
  , update = update
  , subscriptions = subscriptions
  , view = view
  , controls = controls
  , width = width
  }

boardSize : Int 
boardSize = 30

tileSize : Int
tileSize = 30

width : Float
width = toFloat <| boardSize * tileSize

initPlayer : Player
initPlayer = { position = (0, 0) }

initPlatform : Platform
initPlatform = 
  Grid.fill 
    (\(x,y) -> 
      if y == 15 && x > 10 && x < 20
      then
        Just Platform
      else 
        Nothing
    )
    { rows    = boardSize
    , columns = boardSize 
    }

initModel : Model
initModel = { player = initPlayer, platform = initPlatform }

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

update : Msg -> Model -> (Model, Cmd Msg)
update msg ({player, platform} as model) = 
  case msg of
    MovePlayer direction -> 
      ({ model | player = checkAndMove platform direction player }
      , Cmd.none
      )

checkCollision : Position -> Position -> Bool
checkCollision p1 p2 = p1 == p2 

checkAndMove : Grid.Grid Entity -> Direction -> Player -> Player
checkAndMove grid direction player = 
  player.position
    |> Position.add (direction |> Position.fromDirection)
    |> (\newPos -> 
        case (Grid.get newPos grid) of
          Err _       -> player
          Ok (entity) -> 
            case entity of
              (Just Platform) -> player
              _               -> { player | position = newPos }
      )

subscriptions : Model -> Sub Msg
subscriptions _ =
   Time.every 20 (MovePlayer Down |> always)

options : Options Msg
options =
    Options.default
        |> Options.withMovementSpeed 0.8

view :
    Model
    -> { title : String, options : Maybe (Options Msg), body : List (Area Msg) }
view model =
    { title = "BlackOrd"
    , options = Just options
    , body = areas model
    }

areas : Model -> List (Area Msg)
areas { platform, player} =
    [ PixelEngine.tiledArea
        { rows = boardSize
        , tileset =
            { source       = "img/man.png"
            , spriteWidth  = tileSize
            , spriteHeight = tileSize
            }
        , background = colorBackground (rgb255 255 255 255)
        }
        ( platform
            |> Grid.toList
            |> List.map 
                (\(pos, entity) ->
                  ( pos
                  , case entity of 
                      Platform -> playerTile
                      _        -> emptyTile
                  )
                )
            |> (::) (player.position, playerTile)
        )
    ]

playerTile : Tile Msg
playerTile = 
  Tile.fromPosition (0,0) 
    |> Tile.movable "player"
    |> Tile.jumping 

platformTile : Tile Msg
platformTile = Tile.fromPosition (0,0)

emptyTile : Tile Msg
emptyTile = Tile.fromPosition (0, 0)
