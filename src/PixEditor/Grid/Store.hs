{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module PixEditor.Grid.Store where

import Data.Functor.Compose (Compose(..))
import Data.Functor.Identity (Identity(..))
import Data.Bool (bool)
import Control.Arrow ((***))
import Control.Comonad.Representable.Store (Store(..), StoreT(..), store, experiment)
import Control.Comonad
import Control.Concurrent
import Data.Functor.Rep
import Data.Distributive
import qualified Data.Vector as V hiding (modify,length)


tickTime :: Int
tickTime = 200000

--main :: IO ()
--main = loop (extend basicRule) start

loop :: (GridS Bool -> GridS Bool) -> GridS Bool -> IO ()
loop stepper g = do
  --putStr "\ESC[2J" -- Clear terminal screen
  putStrLn (render g)
  threadDelay tickTime
  loop stepper (stepper g)

newtype VBounded a = VBounded (V.Vector a)
  deriving (Eq, Show, Functor, Foldable)

instance Distributive VBounded where
  distribute = distributeRep

gridSize :: Int
gridSize = 40

instance Representable VBounded where
  type Rep VBounded = Int
  index (VBounded v) i
    | i < 0  = v V.! 0
    | i >= gridSize = v V.! (gridSize - 1)
    | True = v V.! i
  tabulate desc = VBounded $ V.generate gridSize desc

type GridS a = Store (Compose VBounded VBounded) a
type Coord = (Int, Int)

type Rule = GridS Bool -> Bool
type Rule2 = GridS String -> Bool

-- Offsets for the neighbouring 8 tiles, avoiding (0, 0) which is the cell itself
neighbourCoords :: [(Int, Int)]
neighbourCoords = [(x, y) | x <- [-1, 0, 1], y <- [-1, 0, 1], (x, y) /= (0, 0)]

mkGridS2 :: [Coord] -> GridS String
mkGridS2 xs = store lookup (0, 0)
  where
    lookup crd = if crd `elem` xs then "blue" else "red"

mkGrid :: [Coord] -> GridS Bool
mkGrid xs = store lookup (0, 0)
  where
    lookup crd = crd `elem` xs

basicRule :: Rule
basicRule g =
  alive || (not alive && numNeighboursAlive >= 1)
  where
    alive = extract g
    addCoords (x, y) (x', y') = (x + x', y + y')
    neighbours = experiment (\s -> addCoords s <$> neighbourCoords) g
    numNeighboursAlive = length (filter id neighbours)

render :: GridS Bool -> String
render (StoreT (Identity (Compose g)) _) = foldMap ((++ "\n") . foldMap (bool "." "#")) g

pat :: [Coord] -> Coord -> [Coord]
pat xs (x, y) = fmap ((+x) *** (+y)) xs

glider, blinker, beacon :: [Coord]
glider = [(1, 0), (2, 1), (0, 2), (1, 2), (2, 2)]
blinker = [(0, 0), (1, 0), (2, 0)]
beacon = [(0, 0), (1, 0), (0, 1), (3, 2), (2, 3), (3, 3)]

start :: GridS Bool
start = mkGrid $
     glider `pat` (0, 0)
  ++ beacon `pat` (15, 5)
  ++ blinker `pat` (16, 4)
