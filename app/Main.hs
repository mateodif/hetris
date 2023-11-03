{-# LANGUAGE OverloadedStrings #-}
module Main ( main ) where

import Data.Word ( Word8, Word32 )
import Foreign.C.Types ( CInt )
import Control.Monad.Reader ( MonadIO(..), forM_ )
import Control.Monad ( unless )
import Control.Lens
import Color
import SDL
import State

mkRect :: Int -> Int -> Int -> Int -> Rectangle CInt
mkRect x y w h = Rectangle rec_origin rec_size
  where
    (x', y', w', h') = over each fromIntegral (x, y, w, h)
    rec_origin = P (V2 x' y')
    rec_size = V2 w' h'

drawCell :: MonadIO m => Renderer -> Rectangle CInt -> V4 Word8 -> m ()
drawCell renderer rect color = do
  rendererDrawColor renderer $= color
  fillRect renderer (Just rect)

drawBoard :: (MonadIO m) => Renderer -> Board -> m ()
drawBoard renderer board' = do
  rendererDrawColor renderer $= Color.red
  forM_ (zip [0 ..] board') $ \(i, row) -> do
    forM_ (zip [0 ..] row) $ \(j, cell) -> do
      let rect = mkRect (i * width) (j * height) width height
          (width, height) = cellDimentions
      if cell == 1
        then fillRect renderer (Just rect)
        else drawRect renderer (Just rect)

pieceFallInterval :: Word32
pieceFallInterval = 1000

appLoop :: Renderer -> State -> Word32 -> IO ()
appLoop renderer state lastMoveTime = do
  events <- pollEvents
  rendererDrawColor renderer $= Color.black
  clear renderer
  currentTicks <- ticks
  let timeSinceLastMove = currentTicks - lastMoveTime
      newState = if timeSinceLastMove >= pieceFallInterval
                    then movePieceDown $ updateState state events
                    else updateState state events

  drawBoard renderer (board newState)
  present renderer
  let cleanState = if isPieceSet newState
        then newState
        else newState { board = mkBoard }

  unless (any (`eventIsKeyPress` KeycodeQ) events) $
    appLoop renderer cleanState (if timeSinceLastMove >= pieceFallInterval then currentTicks else lastMoveTime)

main :: IO ()
main = do
  initializeAll
  let (width, height) = over each fromIntegral screenSize
  window <- createWindow "Tetris" defaultWindow { windowInitialSize = V2 width height }
  renderer <- createRenderer window (-1) defaultRenderer
  let initialState = mkState
  initialTicks <- ticks
  appLoop renderer initialState initialTicks
  destroyWindow window
