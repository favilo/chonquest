module Chonquest where

import Data.Array.IArray
import Control.Monad.State
import Control.Monad.Reader

type CellArray = Array Int Cell

data Owner = Vacant
    | Neutral
    | PlayerO Player
    deriving (Show, Eq)

data PlayerType = Human
    | EasyComputer
    | MediumComputer
    | HardComputer
    deriving (Show, Eq)

data Player = Player 
    { name   :: String
    , number :: Int
    , t_player :: PlayerType
    }

data Cell = Cell 
    { icon   :: Int
    , owner  :: Owner
    , kill_p :: Double
    , prod   :: Int 
    }

data Board = Board 
    { cells       :: CellArray
    , width       :: Int
    , height      :: Int
    , nn_planets  :: Int
    , blind       :: Bool
    , cum_prod    :: Bool
    , p_after_cap :: Bool
    , players     :: [Player]
    } 


