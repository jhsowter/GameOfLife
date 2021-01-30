module Lib
    ( 
        Game (..),
        Cell (..),
        Neighborhood (..),
        tick
    ) where

import Data.HashMap.Strict (HashMap)
import qualified Data.Hashmap.Strict as H

data Game = Game [Cell] deriving Show

data Cell = Dead | Alive deriving (Show, Eq)

data Neighborhood = Neighborhood Cell [Cell] deriving Show

tick :: Neighborhood -> Neighborhood
tick (Neighborhood cell neighbours) = Neighborhood deadOrAlive neighbours
    where 
        liveNeighbours = filter (==Alive) neighbours
        aliveCount = length liveNeighbours
        deadOrAlive = case cell of 
            Alive -> if aliveCount == 2 || aliveCount == 3 then Alive else Dead
            Dead -> if aliveCount == 3 then Alive else Dead

