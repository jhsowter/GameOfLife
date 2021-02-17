{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( 
        Cell (..),
        Neighbourhood (..),
        tick, gridOf, drawGrid, cellsToHashMap, tickGrid, setLive, game
    ) where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H
import Control.Monad (forM_)
import Data.Maybe (catMaybes)
import SDL (Event, Point (P), V2 (V2), ($=))
import qualified SDL

game :: Int -> IO ()
game n = drawGrid $ loop n

loop n = last $ take n $ ticker initial
    where
        ticker grid = grid : ticker next
            where
                next = tickGrid grid
        initial = setLive 9 11 $ setLive 10 10 $ setLive 10 11 $setLive 11 11 $ setLive 11 12 $ setLive 12 12 $ gridOf 20 20


type Grid = HashMap (Int, Int) Cell

data Cell = Dead Int Int | Live Int Int deriving (Show, Eq)

isLive :: Cell -> Bool
isLive (Live _ _) = True
isLive (Dead _ _) = False

data Neighbourhood = Neighbourhood Cell [Cell] deriving Show

cellIn :: Neighbourhood -> Cell
cellIn (Neighbourhood cell neighbours) = cell

liveNeighbours :: Neighbourhood -> [Cell]
liveNeighbours (Neighbourhood cell neighbours) = filter isLive neighbours

tick :: Neighbourhood -> Neighbourhood
tick neighbourhood@(Neighbourhood cell neighbours) = Neighbourhood deadOrLive neighbours
    where 
        liveCount = length $ liveNeighbours neighbourhood
        deadOrLive = case cell of 
            (Live x y) -> if liveCount == 2 || liveCount == 3 then Live x y else Dead x y
            (Dead x y) -> if liveCount == 3 then Live x y else Dead x y

gridOf :: Int -> Int -> HashMap (Int, Int) Cell
gridOf rows columns = cellsToHashMap $ [(Dead x y) | x <- [0..rows], y <- [0..columns]]

cellsToHashMap :: [Cell] -> HashMap (Int, Int) Cell
cellsToHashMap cells = foldr insertCell H.empty cells
    where
        insertCell cell@(Live x y) = H.insert (x,y) cell
        insertCell cell@(Dead x y) = H.insert (x,y) cell

drawGrid :: HashMap (Int, Int) Cell -> IO ()
drawGrid cells = do 
    forM_ [top..bottom] $ \y -> do
        forM_ [left..right] $ \x -> do
            case H.lookup (x, y) cells of
                Just (Live x y) -> putStr "[o]"
                Just (Dead x y) -> putStr "[ ]"
                Nothing -> putStr ""
        putStr "\n"
    where
        xs = map fst (H.keys cells)
        ys = map snd (H.keys cells)
        left = minimum xs
        right = maximum xs
        top = minimum ys
        bottom = maximum ys

tickGrid :: Grid -> Grid
tickGrid grid = H.map nextCell grid
    where
        nextCell cell@(Live x y) = cellIn $ tick $ Neighbourhood cell (neighboursFor (x,y) grid)
        nextCell cell@(Dead x y) = cellIn $ tick $ Neighbourhood cell (neighboursFor (x,y) grid)

setLive :: Int -> Int -> Grid -> Grid
setLive x y grid = H.insert (x,y) (Live x y) grid

neighboursFor :: (Int, Int) -> Grid -> [Cell]
neighboursFor (x,y) grid = catMaybes cells
    where
        cells :: [Maybe Cell]
        cells = [(H.lookup (x,y) grid) | x <- [x-1, 1, x+1], y <- [y-1, y, y+1]]