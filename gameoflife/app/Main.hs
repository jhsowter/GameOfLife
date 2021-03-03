{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lib
import SDL
import Linear (V4(..))
import Control.Monad (unless, forM_)
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
    
    let g = setLive 25 25 $ setLive 25 26 $ setLive 25 27 $ gridOf (width `div` cellSize) (height `div` cellSize)
    appLoop renderer g

    return ()

appLoop :: Renderer -> Grid -> IO ()
appLoop renderer grid = do
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
  -- let init = tickN ((width `div` cellSize), (height `div` cellSize)) tick
  drawGrid grid
  drawGridr renderer $ grid
  -- drawCell renderer (10, 10) 10 White
  -- rendererDrawColor renderer $= V4 255 255 255 255
  -- let list = [(0, 0), (10, 10), (0, 10), (10, 0)]
  -- forM_ list $ \(x,y) ->
  --     fillRect renderer $ Just $ Rectangle (P (V2 x y)) (V2 (x+10) (y+10))
  -- let rect = Rectangle (P (V2 10 10)) (V2 20 20)
  -- fillRect renderer $ Just rect

  present renderer
  unless quit (appLoop renderer (tickGrid grid))

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
