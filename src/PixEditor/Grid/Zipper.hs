{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
module PixEditor.Grid.Zipper where

import Control.Comonad

-- | Class for a (modular) bounded container
--
-- Examples of functions provided for a simple one dimensional list, where appropriate
class Zipper z where
  type Index z
  data Direction z

  -- | Shift in a direction
  shift :: Direction z -> z a -> z a

  -- | Retrieve current cursor value
  cursor :: z a -> Maybe a

  -- | Retrieve current index value
  index :: z a -> Index z

  -- | Retrieve neighborhood of current cursor.
  neighborhood :: z a -> [a]

  -- | Destruct to list maintaining order of @(Index z)@, e.g. @(Z ls c rs) -> ls ++ [c] ++ rs@.
  toList :: z a -> [a]

  -- | Destruct a list into a mapping with indices
  toMap :: (Comonad z) => z a -> [(Index z, a)]
  toMap = toList . extend ((,) <$> index <*> extract)

  -- | Construct zipper from mapping (provide default value so this is always safe, no bottoms)
  fromMap :: Ord (Index z) => a -> [(Index z, a)] -> z a

  -- | Lookup by possibly denormalised index (still safe from modularity).
  --
  -- e.g. [1,2] ! 2 == 1
  (!) :: z a -> (Index z) -> a

  -- | Adjust value at specified index
  adjust :: (a -> a) -> Index z -> z a -> z a

  -- | Update value at specified index
  update :: a -> Index z -> z a -> z a
  update = adjust . const

  resize :: a -> z a -> (Index z) -> z a

  -- | Get size (maximum of @Index z@).
  size :: z a -> (Index z)
