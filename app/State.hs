module State (
  Board,
  cellDimentions,
  eventIsKeyPress,
  mkBoard,
  mkState,
  movePieceDown,
  State(..),
  updateState,
  screenSize,
  Position
) where

import Tetromino
import SDL

type Board = [[Int]]

type Position = (Int, Int)

data Direction = LeftD
               | RightD
               | DownD
               deriving (Show, Eq)

data State = State
    { board         :: Board
    , position      :: Position
    , currentPiece  :: Tetromino
    , isPieceSet    :: Bool
    , score         :: Int
    }

screenSize :: (Int, Int)
screenSize = (500, 900)

rowCount :: Int
rowCount = 20

columnCount :: Int
columnCount = 10

cellDimentions :: (Int, Int)
cellDimentions = (div width columnCount, div height rowCount)
  where
    (width, height) = screenSize

mkBoard :: Board
mkBoard = replicate cellHeight (replicate cellWidth 0)
  where (cellWidth, cellHeight) = cellDimentions

mkState :: State
mkState = State board' position' currentPiece' isPieceSet' score'
  where
    board' = mkBoard
    position' = (5, 1)
    currentPiece' = randomTetromino2
    isPieceSet' = False
    score' = 0

eventIsKeyPress :: Event -> Keycode -> Bool
eventIsKeyPress event keycode =
  case eventPayload event of
    KeyboardEvent keyboardEvent ->
      keyboardEventKeyMotion keyboardEvent == Pressed
        && keysymKeycode (keyboardEventKeysym keyboardEvent) == keycode
    _ -> False

move :: Position -> Direction -> Position
move (x, y) LeftD = (x - 1, y)
move (x, y) RightD = (x + 1, y)
move (x, y) DownD = (x, y + 1)

insertTetromino :: Board -> Tetromino -> Position -> Board
insertTetromino board' tetromino (cx, cy) =
    [ [ updateCell x y col | (col, y) <- zip row [0..] ] | (row, x) <- zip board' [0..] ]
    where updateCell x y col
              | (x, y) `elem` adjustedCoords = 1
              | otherwise = col
          adjustedCoords = [(x + cx, y + cy) | (x, y) <- shape tetromino]

handleEvent :: State -> Event -> State
handleEvent state event
  | eventIsKeyPress event KeycodeLeft  = state { position = move (position state) LeftD }
  | eventIsKeyPress event KeycodeRight = state { position = move (position state) RightD }
  | eventIsKeyPress event KeycodeDown  = state { position = move (position state) DownD }
  | otherwise                          = state

updateState :: State -> [Event] -> State
updateState state []     = state { board = insertTetromino (board state) (currentPiece state) (position state) }
updateState state (e:es) = updateState (handleEvent state e) es

movePieceDown :: State -> State
movePieceDown state = state { position = move (position state) DownD }
