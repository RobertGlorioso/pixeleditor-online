{-# LANGUAGE TemplateHaskell #-}
module PixEditor.Grid.Data where

import qualified Data.Vector as V
import Data.Vector.Mutable (IOVector)
import qualified Data.Sequence as S
import Lens.Micro.TH



data Y a = Y { _yl :: S.Seq a
             , _yc :: Maybe a
             , _yi :: Int
             } deriving (Eq, Show)

newtype YY a = YY { _unyy :: Y (Y a) }
  deriving (Eq,Show)

makeLenses ''Y
makeLenses ''YY

data V a = V { _vl :: V.Vector a
             , _vc :: Maybe a
             , _vi :: Int
             } deriving (Eq, Show)

newtype VV a = VV { _unvv :: V (V a) }

makeLenses ''V
makeLenses ''VV

newtype VY a = VY { _unvy :: V (Y a) } deriving (Eq,Show)

makeLenses ''VY

data M a = M { _ml :: IOVector a
             , _mc :: Maybe a
             , _mi :: Int
             }

makeLenses ''M

newtype MY a = MY { _unmy :: M (Y a) }

makeLenses ''MY
