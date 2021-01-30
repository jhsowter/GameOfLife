module Lib
    ( 
        Cell (..),
        Neighbourhood (..),
        tick, gridOf
    ) where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H

data Cell = Dead Int Int | Live Int Int deriving (Show, Eq)

data Neighbourhood = Neighbourhood Cell [Cell] deriving Show

tick :: Neighbourhood -> Neighbourhood
tick (Neighbourhood cell neighbours) = Neighbourhood deadOrLive neighbours
    where 
        liveNeighbours = filter isLive neighbours
        liveCount = length liveNeighbours
        deadOrLive = case cell of 
            (Live x y) -> if liveCount == 2 || liveCount == 3 then Live x y else Dead x y
            (Dead x y) -> if liveCount == 3 then Live x y else Dead x y

        isLive (Live _ _) = True
        isLive (Dead _ _) = False

gridOf :: Int -> Int -> [[Cell]]
gridOf = undefined
-- gridOf x y = 
--     where
--         g = take y $ repeat (take x $ repeat $ H.singleton Dead)