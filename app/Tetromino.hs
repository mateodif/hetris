module Tetromino (Tetromino(..), newTetromino, TetrominoType, Color, randomTetromino2) where

import Color
import System.Random (randomRIO)

data TetrominoType = I | J | L | O | S | T | Z
                   deriving (Show, Eq)

type Shape = [(Int, Int)]

data Tetromino = Tetromino
    { shape  :: Shape
    , color  :: Color
    , tType  :: TetrominoType
    } deriving (Show, Eq)

newTetromino :: TetrominoType -> Tetromino
newTetromino t_type =
  let (shape', color') = case t_type of
        I -> ([( -1, 0), ( 0, 0), ( 1, 0), (2, 0)], skyblue)
        T -> ([(  0,-1), (-1, 0), ( 0, 0), (1, 0)], purple)
        O -> ([(  0,-1), ( 1,-1), ( 0, 0), (1, 0)], yellow)
        J -> ([( -1,-1), (-1, 0), ( 0, 0), (1, 0)], blue)
        L -> ([(  1,-1), (-1, 0), ( 0, 0), (1, 0)], orange)
        S -> ([(  0,-1), ( 1,-1), (-1, 0), (0, 0)], green)
        Z -> ([( -1,-1), ( 0,-1), ( 0, 0), (1, 0)], red)
  in Tetromino shape' color' t_type

randomTetromino2 :: Tetromino
randomTetromino2 = newTetromino L

randomTetromino :: IO Tetromino
randomTetromino = do
    randomNum <- randomRIO (0, 6) :: IO Int
    let tetrominoType = case randomNum of
            0 -> I
            1 -> T
            2 -> O
            3 -> J
            4 -> L
            5 -> S
            6 -> Z
            _ -> error "Unexpected random number"
    return $ newTetromino tetrominoType
