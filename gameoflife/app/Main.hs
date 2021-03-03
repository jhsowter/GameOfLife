{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lib
import SDL
import Linear (V4(..))
import Control.Monad (unless, forM_)
import Control.Concurrent
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H

data Colour = Black | White

height = 1280
width = 720
cellSize = 12

main :: IO ()
main = do
    initializeAll
    window <- createWindow "Game of Life" defaultWindow {
      SDL.windowInitialSize = V2 1280 720
    }
    renderer <- createRenderer window (-1) defaultRenderer
    
    appLoop renderer

    return ()

blinker :: Int -> Int -> Grid -> Grid
blinker x y = setLiveN [(x,y), (x, y+1), (x, y+2)]

glider :: Int -> Int -> Grid -> Grid
glider x y = setLiveN [(x, y), (x+2, y), (x+1, y+1), (x+2, y+1), (x+1, y+2)]

appLoop :: Renderer -> IO ()
appLoop renderer = do
  now <- SDL.time
  -- let g = setLive 25 25 $ setLive 25 26 $ setLive 25 27 $ gridOf (width `div` cellSize) (height `div` cellSize)
  let initialGrid = blinker 25 25 $ gridOf (width `div` cellSize) (height `div` cellSize)
  innerLoop renderer now 0 initialGrid
  where
    innerLoop :: Renderer -> Double -> Int -> Grid -> IO ()
    innerLoop renderer startTime ticks grid = do
      events <- pollEvents
      let eventIsQPress event =
            case eventPayload event of
              KeyboardEvent keyboardEvent ->
                keyboardEventKeyMotion keyboardEvent == Pressed &&
                keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
              _ -> False
          qPressed = any eventIsQPress events
      let quit = elem SDL.QuitEvent $ map SDL.eventPayload events

      rendererDrawColor renderer $= V4 0 0 0 255

      clear renderer

      -- draw grid
      now <- SDL.time
      drawGridr renderer $ grid
      drawGrid grid
      present renderer

      -- next
      let oneSecond = startTime + (fromIntegral 1)
      if(oneSecond > now)
        then
          unless quit (innerLoop renderer startTime (ticks) grid)
        else
          unless quit (innerLoop renderer oneSecond (ticks + 1) $ tickGrid grid)

drawCell :: Renderer -> (Int, Int) -> Int -> Colour -> IO ()
drawCell renderer (x, y) size colour = do
  let x' = fromIntegral x
  let y' = fromIntegral y
  let size' = fromIntegral size
  case colour of 
    White -> rendererDrawColor renderer $= V4 255 255 255 255
    Black -> rendererDrawColor renderer $= V4 0 0 0 255
  fillRect renderer $ Just $ Rectangle (P (V2 x' y')) (V2 (size') (size'))

drawGridr :: Renderer -> HashMap (Int, Int) Cell -> IO ()
drawGridr renderer cells = do 
    forM_ [top..bottom] $ \y -> do
        forM_ [left..right] $ \x -> do
            case H.lookup (x, y) cells of
                Just (Live x y) -> drawCell renderer (toxy (x, y)) cellSize White
                Just (Dead x y) -> drawCell renderer (toxy (x, y)) cellSize Black
                Nothing -> putStr ""
        putStr "\n"
    where
        xs = map fst (H.keys cells)
        ys = map snd (H.keys cells)
        left = minimum xs
        right = maximum xs
        top = minimum ys
        bottom = maximum ys
        toxy (x, y) = (x*cellSize, y*cellSize)
