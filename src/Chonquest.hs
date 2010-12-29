module Chonquest where

import Data.Array.IArray

import Control.Monad
import Control.Monad.State
import Control.Monad.Reader

import Graphics.UI.Gtk.Glade

type CellArray = Array Int Cell
type Coord     = (Int, Int)
type AppState  = StateT Board IO
type AppEnv    = ReaderT AppConfig AppState

data AppConfig = AppConfig
    { board    :: Board
    , xml      :: GladeXML
    }
    deriving (Eq)

data Owner = Vacant
    | Neutral
    | PlayerO Player
    deriving (Show, Eq)

data PlayerType = Human
    | EasyComputer
    | MediumComputer
    | HardComputer
    deriving (Show, Eq, Enum, Ord)

data Player = Player 
    { p_name :: String
    , number :: Int
    , p_type :: PlayerType
    }
    deriving (Show, Eq)

data Fleet = Fleet 
    { n_ships :: Int
    , origin  :: Cell
    , target  :: Cell
    }
    deriving (Show, Eq)

data Cell = Cell 
    { icon   :: Int
    , owner  :: Owner
    , c_name   :: String
    , kill_p :: Double
    , prod   :: Int 
    , ships  :: Int
    , pos    :: Coord
    }
    deriving (Show, Eq)

data Board = Board 
    { cells       :: CellArray
    , width       :: Int
    , height      :: Int
    , nn_planets  :: Int
    , blind       :: Bool
    , cum_prod    :: Bool
    , p_after_cap :: Bool
    , players     :: [Player]
    , fleets      :: [Fleet]
    } 
    deriving (Show, Eq)

initEnv :: GladeXML -> IO AppConfig
initEnv xml = do
    return undefined

