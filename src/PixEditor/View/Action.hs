{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module PixEditor.View.Action where

import Miso hiding (update)
import Miso.Subscription.Keyboard
import Miso.String (MisoString, (<>), pack)
import qualified Miso.Svg as Svg
import JavaScript.Web.Canvas
import GHCJS.Types
import GHCJS.Marshal
import GHCJS.Foreign.Callback
import Control.Concurrent.MVar
import qualified Data.Map as M
import Data.List

import PixEditor.Foreign
import PixEditor.Foreign.CanvasBS
import PixEditor.Grid.Data
import PixEditor.Grid.Zipper
import PixEditor.Grid.Transform
import PixEditor.View.Model


-- | Sum type for application events
data Action
  = HandleMouse (Int, Int)
  | GetArrows !Arrows
  | PickColor JSString
  | Rename JSString
  | Review JSString
  | RedrawGrid (YY JSString)
  | Fill (Int,Int)
  | Opacity JSString
  | Selected (Int,Int)
  | DragSelected (Int,Int)
  | PixelResize Int
  | PickSpectrum
  | UpdatePic
  | UpdateGrid
  | SwitchGrid (GridControl)
  | ReadFile
  | Begin
  | PixelPaint
  | HideGrid
  | Paint
  | Id

-- | Updates model, optionally introduces side effects
updateModel :: Action -> Model -> Effect Action Model
updateModel (HandleMouse newCoords) m = noEff  m { mouseCoords = newCoords }

updateModel (GetArrows a@(Arrows x y)) m = let (i,j) = selected m in (pure $ Selected (i-y,j+x) ) #>  m { getArrows = a }

updateModel (PickColor p) m = noEff m {color = p}

updateModel (Selected i) m = noEff m { selected = i, gridY = update (color m) i (gridY m) }

updateModel UpdatePic m = noEff m {
  store = M.insert (title m) (grid m) (store m)
  }
  where insert' j k l --if user doesnt want to rename (or to animate a set of frames..?)
          | j `elem` M.keys l = insert' (j <> "'") k l
          | True              = M.insert j k l       

updateModel (PixelResize i) m = noEff m { pix = i }

updateModel PickSpectrum m = m <# do
  let (x,y) = mouseCoords m
  ctx <- getCtx "spectrum"
  p <- getPixel ctx x y
  return $ PickColor p

updateModel (RedrawGrid i) m = noEff m { gridY = i }

updateModel (Rename r) m = noEff m { title = r }

--updateModel (Review r) m = noEff m { title = r, grid = maybe mempty id $ M.lookup r (store m) }

updateModel (SwitchGrid i) m = noEff m { gridCont = i}  -- not $ fillSwitch m }

updateModel (Fill i) m = {--  m <- mutateV $ _unvy $ (\c -> if c == vy ! i then (False,c) else (False,c)) <$> vy
  filled <- yySeek'' curColor $ MY m
  v' <- freezeM $ _unmy filled
  return (Fill $ fmap snd $ VY v')
--}
  --return Id -- (Selected i)
  noEff m { gridY = maybe yy id $ (fmap.fmap) (\(b,c) -> if b then curColor else c) $ foldl' merge' (Just yGrid) $ yyBuild (yy ! i) (adjustYTo i yGrid) }
  where cursor = selected m
        curColor = color m
        yy = gridY m
        yGrid = fmap (False,) yy
  
updateModel Paint m = m <# do
  let (d,e) = size $ gridY m
  ctx <- getCtx (title m)
  let g = gridY m
  let p = pix m
  v <- toJSVal $ toNumbBSN' p e $ toList g
  --log_ v
  drawImageData ctx (p*e) (p*d) (v)
  return Id

updateModel ReadFile m = m <# do
  let (d,e) = size $ gridY m
  
  g <- readImageData d e
  return $ RedrawGrid g
  
updateModel Id m = noEff m

updateModel Begin m = m <# do
  ctx <- getCtx "spectrum"
  getPic "../image/spectrum.jpg" ctx
  noArrowScrolling
  noDragging
  return Id

readImageData d e = do
  fileReaderInput <- getElementById "fileReader"
  file <- getFile fileReaderInput
  reader <- newReader
  canvas <- getCtx "hiddenCanvas"
  g <- newEmptyMVar
  setOnLoad1 reader =<< do
    img <- newImage
    cb <- asyncCallback $ do
        drawImage img 0 0 d e canvas
        picdata <- getPixels canvas 0 0 e d
        r <- return $ fromBSArray picdata
        putMVar g (fromMap "brown" $ zip [(x,y) | x <- [0..(d-1)], y <- [0..(e-1)]] r :: YY JSString)
    imgSetOnLoad img cb
    asyncCallback1 $ \file -> do
      r <- readResult file
      setSrc img r
  readResultURL reader file
  readMVar g
  
getPic p c = do
  canvasImage <- newImage
  --setCORS canvasImage
  drawImage' canvasImage c
  setSrc canvasImage p
  save c
