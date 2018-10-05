{-# LANGUAGE OverloadedStrings #-}
module PixEditor.View.Model where

import GHCJS.Types
import qualified Data.Map as M
import Miso.Subscription.Keyboard

import PixEditor.Grid.Data
import PixEditor.Grid.Transform
import PixEditor.Grid.Zipper

type Grid =  M.Map Int JSString
type GridStore =  M.Map JSString Grid
data GridControl = FillSwitch | PaintSwitch | DragSwitch | NoopSwitch deriving (Eq, Show)-- changes actrions for clicking on the grid

-- | Type synonym for an application model
data Model = Model { mouseCoords :: (Int, Int) --mousetracking for color picker
                   , getArrows :: Arrows --arrows for index movement
                   , color :: JSString --selected color
                   , selected :: (Int,Int) --index on grid
                   , pix :: Int --how big to draw each pixel
                   , grid :: Grid --map from a pos. int to a color name or rgb value
                   , gridY :: YY JSString --current state of the grid
                   , gridCont :: GridControl --changes behavior of grid cells
                   
                   , store :: GridStore --map to store pictures along with titles
                   , title :: JSString --title to store current grid under
                   
                   }
           deriving (Show, Eq)

emptyModel :: Model
emptyModel = let oldList = zip [0..2] $ repeat "rgb(255,255,255)";
                 yList = zip [(x,y) | x <- [0..32], y <- [0..32]] $ repeat "rgba(255,255,255,255)"
             in Model (0,0) (Arrows 0 0) ("rgba(0,204,205,255)") (0,0) 1 (M.fromList oldList) (fromMap "brown" yList) PaintSwitch  mempty "My Pixel Art" 
