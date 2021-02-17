{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lib
import SDL
import Linear (V4(..))
import Control.Monad (unless)

main :: IO ()
main = do
    initializeAll
    window <- createWindow "My SDL Application" defaultWindow
    renderer <- createRenderer window (-1) defaultRenderer
    appLoop renderer

    return ()

appLoop :: Renderer -> IO ()
appLoop renderer = do
  events <- pollEvents
  let eventIsQPress event =
        case eventPayload event of
          KeyboardEvent keyboardEvent ->
            keyboardEventKeyMotion keyboardEvent == Pressed &&
            keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
          _ -> False
      qPressed = any eventIsQPress events

  let quit = elem SDL.QuitEvent $ map SDL.eventPayload events

  rendererDrawColor renderer $= V4 0 0 255 255
  clear renderer
  present renderer
  unless quit (appLoop renderer)