{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-} 
module PixEditor.Grid.Transform where

import qualified Data.Map as M
import qualified Data.Sequence as S
import qualified Data.JSString as J
import qualified Data.Vector.Generic
import qualified Data.Vector as V hiding (modify,length)
import qualified Data.Vector.Mutable as V (read, write, length, modify)
import Data.Sequence (ViewL(..), ViewR(..),(<|),(|>))
import Data.Maybe (catMaybes)
import Data.List
import Control.Comonad
import Data.Monoid
import Data.Ord
import Data.Tree
import Data.JSString (JSString,unpack)
import Lens.Micro.Platform

import PixEditor.Foreign
import PixEditor.Foreign.CanvasBS
import PixEditor.Grid.Data
import PixEditor.Grid.Zipper

compose :: Int -> (a -> a) -> (a -> a)
compose = (foldr (.) id .) . replicate

-- Functor instances

instance Functor Y where
  fmap f (Y l c i) = Y (fmap f l) (fmap f c) i

instance Functor V where
  fmap f (V l c i) = V (fmap f l) (fmap f c) i

instance Functor VV where
  fmap f = VV . (fmap . fmap) f . _unvv

instance Functor VY where
  fmap f = VY . (fmap . fmap) f . _unvy

instance Functor YY where
  fmap f = YY . (fmap . fmap) f . _unyy

-- Comonad instances

instance Comonad Y where
  extract = maybe (error "cursor not on grid") id . _yc 
  duplicate y = Y (S.fromFunction (size y - 1) fn) (Just y) ( y ^. yi )
    where fn k = compose (k + 1) ( shift YL ) y

instance Comonad YY where
  extract = maybe (error "cursor not on grid")  id . cursor
  duplicate z = YY $ Y
    (fromF (xT - 1) mkCol) (Just $ Y (fromF (yT - 1) ( mkRow z)) (Just z) y) x
    where
      mkRow zx j = compose (j + 1) (shift YS) zx
      mkCol i    = let zx = compose (i + 1) (shift YW) z
                    in Y (fromF (yT - 1) (mkRow zx)) (Just zx) (zx ^. to index  ^. _2)
      (xT,yT)    = size z
      (x,y)      = index z
      fromF      = S.fromFunction

-- Zipper instances

instance Zipper Y where
  type Index Y = Int
  data Direction Y = YL | YR deriving (Eq, Show)
  cursor = _yc
  index = _yi
  resize a y@(Y l c i) n = let s = size y in
    case compare s n of
      LT -> Y (l <> S.replicate (n - s) a) c i
      GT -> uncurry (Y (S.take n l)) $ if i < n then (c,i) else (Just a,n) 
      EQ -> y 
  size (Y l _ _) = S.length l 
  (!) (Y l c _) k = l `S.index` k
  adjust f k y@(Y l c i) = case l ?! k of
    Nothing  -> y
    (Just j) -> y { _yl = S.adjust f k l, _yc = if k == i then f <$> c else c  }
  toList (Y l c i) = foldr (:) [] l
  fromMap _ [] = error "Zipper must have length greater than zero."
  fromMap a m = Y (S.fromList ys) (Just . snd $ minimumBy (comparing fst) m) 0
    where ys = fmap snd m      
  shift d v@(Y l c i)
    | S.null l = v -- shifting length zero amounts to nothing
    | d == YL   = Y l ( l ?! (i - 1) ) (i - 1)
    | d == YR   = Y l ( l ?! (i + 1) ) (i + 1)

instance Zipper V where
  type Index V = Int
  data Direction V = VL | VR deriving (Eq, Show)
  cursor = _vc
  index = _vi
  size (V l _ _) = length l + 1
  (!) v k = (_vl v) V.! k
  adjust f k v@(V l c i) = case l V.!? k of
    Nothing  -> v
    (Just j) -> v { _vl = V.update l (V.fromList [(k,j)]), _vc = if k == i then f <$> c else c  }
  neighborhood (V l _ _)
    | length l <= 2 = V.toList l
    | otherwise       = map ((V.!) l) [0, length l - 1]
  toList (V l c i) = V.toList l
  fromMap _ [] = error "Zipper must have length greater than zero."
  fromMap a m = V (V.fromList vs) (Just . snd $ minimumBy (comparing fst) m) 0
    where vs = fmap snd m
  shift d v@(V l c i)
    | V.null l = v 
    | d == VL   = V l (l V.!? (i - 1) ) (i-1)
    | d == VR   = V l (l V.!? (i + 1) ) (i+1)

instance Zipper VY where
  type Index VY = (Int, Int)
  data Direction VY = VN | VE | VS | VW deriving (Eq, Show)
  cursor vy = cursor =<< (_vc $ _unvy vy)
  adjust f (a, b) vy@(VY v@(V l c r)) = case l V.!? a of
    Nothing  -> vy
    (Just p) -> let
      yc =  adjust f b p in
      shift VN $ shift VS $ VY $ v { _vl = V.update l (V.fromList [(a,yc)]) } 
  (!) z (x, y) = (_unvy z) ! x ! y
  resize a ( VY (v@(V l c i))) (n,m) = let s = length l
                                           nullRow = (Y (S.replicate m a) (Just a) 0)
                                           comparison = case compare s n of
                                             LT -> uncurry (V (V.take n l)) $ if i < n then (c,i) else (Just nullRow , n) 
                                             GT -> V (l <> V.replicate (n-s) nullRow) c i
                                             EQ -> v
                                       in VY $ fmap (\yrow -> resize a yrow m) comparison
  size z = (x, y)
    where x = z ^. unvy ^. to size
          y = maximum $ size <$> z ^. unvy ^. vl
  index z = (x, y)
    where x = z ^. unvy ^. vi
          y = maybe (error "out of bounds") _yi $ z ^. unvy ^. vc
  shift VE = (& unvy %~ shift VR)
  shift VW = (& unvy %~ shift VL)
  shift VN = (& unvy %~ fmap (shift YR))
  shift VS = (& unvy %~ fmap (shift YL))
  fromMap _ [] = error "Zipper must have length greater than zero."
  fromMap a m  =  VY $ V (V.fromList cs) (Just $ head cs) 0
    where cs        = fmap (fromMap a) $ (fmap.fmap) (& _1 %~ snd) g 
          g         = groupBy (\a b -> (fst $ fst a) == (fst $ fst b)) m
          l         = length m

instance Zipper YY where
  type Index YY = (Int, Int)
  data Direction YY = YN | YE | YS | YW deriving (Eq, Show)

  cursor y = cursor =<< (_yc $ _unyy y)
  toList = foldl (++) [] . fmap toList . _yl . _unyy
  adjust f (a, b) yy@(YY y@(Y l c r)) = case l ?! a of
    Nothing  -> yy
    (Just p) -> let
      yc =  adjust f b p in
      shift YN $ shift YS $ YY $ y { _yl = S.update a yc l }
  (!) z (x, y) = (_unyy z) ! x ! y
  resize a ( YY (v@(Y l c i))) (n,m) = let s = length l
                                           nullRow = (Y (S.replicate m a) (Just a) 0)
                                           comparison = case compare n s of
                                             LT -> uncurry (Y (S.take n l)) $ if i < n then (c,i) else (Just nullRow , n) 
                                             GT -> Y (l <> S.replicate (n-s) nullRow) c i
                                             EQ -> v
                                       in YY $ fmap (\yrow -> resize a yrow m) comparison
  size z = (x, y)
    where x = z ^. unyy ^. to size
          y = maximum $ size <$> z ^. unyy ^. yl
  index z = (x, y)
    where x = z ^. unyy ^. yi
          y = maybe (error "out of bounds") _yi $ z ^. unyy ^. yc 
  shift YE = (& unyy %~ fmap (shift YR))
  shift YW = (& unyy %~ fmap (shift YL))
  shift YN = (& unyy %~ shift YR)
  shift YS = (& unyy %~ shift YL)
  {--neighborhood (YY (Y l c _)) = ns ++ ew
    where ns  = neighborhood c
          ewc = if (S.length l <= 2)
                   then F.toList l
                   else map (S.index l) [0, S.length l - 1]
          ew  = concatMap neighborhood' ewc
          neighborhood' z = (z ^. zc) : neighborhood z --}
  fromMap _ [] = error "Zipper must have length greater than zero."
  fromMap a m  = YY $ Y (S.fromList cs) (Just $ head cs) 0
    where cs        = fmap (fromMap a) $ (fmap.fmap) (& _1 %~ snd) g 
          g         = groupBy (\a b -> (fst $ fst a) == (fst $ fst b)) m
          l         = length m












adjustYTo (i,j) = adjustYCol j . adjustYRow i
adjustYCol j y = y & (unyy . yl . mapped ) %~ (adjustYin j) & (unyy . yc . mapped ) %~ (adjustYin j)
adjustYRow i = YY . adjustYin i . _unyy
adjustYin a yy@(Y arr y _) = yy { _yc = arr ?! a
                                , _yi = a }

(?!) :: S.Seq a -> Int -> Maybe a
(?!) xs i
  | i < 0 = Nothing
  | length xs <= i = Nothing
  | True = Just (xs `S.index` i)


--------bad----code--------
type FCell = (Bool,JSString)

mutateV :: V a -> IO (M a)
mutateV v@(V l c i) = do
  m <- V.thaw l
  return $ M m c i

freezeM :: M a -> IO (V a)
freezeM m@(M l c i) = do
  v <- V.freeze l
  return $ V v c i

adjustMTo (i,j) m = adjustMVCol j =<< adjustMRow i m

adjustMVCol :: Int -> MY a -> IO ()
adjustMVCol j m = V.modify ( _ml $ _unmy m ) (adjustYin j) (_mi $ _unmy m)

adjustMCCol j m = m & ( unmy . mc . mapped ) %~  adjustYin j

adjustMRow :: Int -> MY a -> IO (MY a)
adjustMRow i my = do
  r <- adjustMin i $ _unmy my
  return $ MY r

adjustMin :: Int -> M a -> IO (M a)
adjustMin a m@(M v y _) = do
  c <- v `V.read` a
  return $ m { _mc = Just $ c
             , _mi = a }

shiftMDown m@(M _ _ i)
  | i <= 0 = return $ m {_mc = Nothing, _mi = 0}
  | True = adjustMin (_mi m - 1) m
    
shiftMUp m@(M v _ i) = let l = V.length v in
  if i < l - 1 then adjustMin (_mi m + 1) m 
    else return $ m {_mc = Nothing, _mi = l}

updateArray :: JSString -> M (Y FCell) -> IO (M (Y FCell))
updateArray c m@(M v (Just y) i) = do
  let newRow = yRowSeek'' c y
  V.write v i newRow
  return m { _mc = Just newRow }

yRowSeek'' :: JSString -> (Y FCell) -> (Y FCell)
yRowSeek'' c y@(Y row Nothing colIndex) = y
yRowSeek'' c y@(Y row (Just (s,v)) colIndex)
  | s      = y
  | c /= v = y { _yl = S.adjust (\(a,b) -> (True,b)) colIndex row
               , _yc = Just (True,v) }
  | True   =
    let (l, r) = S.splitAt colIndex row;  
        seqRunL u (S.viewl -> S.EmptyL)  = S.empty
        seqRunL u (S.viewl -> (x :< xs)) = if c == snd x
                                           then u x <| seqRunL u xs
                                           else x <| xs
        seqRunR u (S.viewr -> S.EmptyR)  = S.empty
        seqRunR u (S.viewr -> (xs :> x)) = if c == snd x
                                           then seqRunR u xs |> u x
                                           else xs |> x
        updateRow = seqRunR (\(a,b) -> (True,b)) l <> seqRunL (\(a,b) -> (True,b)) r 
    in y { _yl = updateRow
         , _yc = Just (True,v) }

yySeek'' :: JSString -> MY FCell -> IO (MY FCell)
yySeek'' c !my@(MY m@(M array Nothing rowIndex)) = return my
yySeek'' c !my@(MY m@(M array (Just row) rowIndex)) = do
  case _yc row of
      Nothing -> return my
      Just (True,_) -> return my
      Just (False,_) -> do
          mm <- updateArray c m
          rowUp <- shiftMUp mm
          rowDown <- shiftMDown mm
          branch mm rowUp
          branch mm rowDown     
    where filterSeen = S.findIndicesL fst
          filterMatching = S.findIndicesL ((/= c) . snd)
          toRow = maybe (Y S.empty Nothing (-1)) id . _mc
          seeked = filterSeen . _yl . toRow
          toCheck =  filterMatching . _yl . toRow
          branch old ud = foldl' (>>=) (return $ MY ud) $ (flip fmap) (subSeqs (+1) (seeked old `intersect` (toCheck ud \\ seeked ud))) $ \case 
            [] -> return 
            i:_ -> yySeek'' c . adjustMCCol i

-------------------------

yRowSeek :: JSString -> (Index Y) -> (Y JSString) -> M.Map (Index YY) ()
yRowSeek c i y@(Y row Nothing colIndex) = M.empty
yRowSeek c i y@(Y row (Just v) colIndex)
  | c /= v   = M.empty
  | True     = let (l, r) = S.splitAt colIndex row;
                   rightMatch = maybe (length row) (colIndex+) $ S.findIndexL (/= c) r;
                   leftMatch = maybe 0 (+1) $ S.findIndexR (/= c) l; 
               in M.fromList $ fmap (\c -> ((i,c),())) [leftMatch .. rightMatch -1]

yySeek color stack yy@(YY y@(Y array (Just row) rowIndex)) =
  case _yc row of
      Nothing -> M.empty 
      _ -> M.union newStack $ M.union (branch rowUp toCheckUp) (branch rowDown toCheckDown)      
  where for = flip fmap
        rowSeen = fmap snd $ filter ((==rowIndex).fst) $ M.keys newStack
        matched = yRowSeek color rowIndex row
        newStack = M.union matched stack
        filterMatching = S.findIndicesL (color==)
        toRow =  maybe (Y S.empty Nothing (-1)) id . _yc . _unyy
        rowUp = shift YN yy
        rowDown = shift YS yy
        toCheckUp =  filterMatching $ _yl $ toRow rowUp 
        toCheckDown =  filterMatching $ _yl $ toRow rowDown
        --begin recursion
        branch ud [] = M.empty 
        branch ud chk =  foldl' M.union M.empty $ for (subSeqs (+1) (rowSeen `intersect` chk))
          $ \case 
          [] -> M.empty
          i:_ -> if M.member (index $ _unyy ud, i) newStack then M.empty else yySeek color newStack (adjustYCol i ud)
















----------------------------------------
yRowSeek' :: JSString -> (Y (Bool, JSString)) -> (Y (Bool, JSString))
yRowSeek' c y@(Y row Nothing colIndex) = y
yRowSeek' c y@(Y row (Just (s,v)) colIndex)
  | s        = y
  | c /= v   = y { _yl = S.adjust (\(a,b) -> (True,b)) colIndex row
                 , _yc = Just (True,v) }
  | True     = let (l, r) = S.splitAt colIndex row;
                   updateRow = seqRunR (\(a,b) -> (True,b)) (False,c) l <> seqRunL (\(a,b) -> (True,b)) (False,c) r 
               in y { _yl = updateRow
                    , _yc = Just (True,v) }

--disconnected runs in the array are partitioned with subSeqs

subSeqs :: (Eq a, Num a) => (a -> a) -> [a] -> [[a]]
subSeqs f x = case x \\ t of 
    [] -> [t]
    d -> t:subSeqs f d
  where
    t = takeRun x
    takeRun [] = []
    takeRun (x:[]) = x:[] 
    takeRun (x:xs) = if f x == head xs then x : takeRun xs else x : []
    
type ForestS a = [TreeS a]
data TreeS a = NodeS {
        rootLabel :: !a,         -- ^ label value
        subForest :: ForestS a   -- ^ zero or more child trees
     }  deriving (Eq, Read, Show)

instance Foldable TreeS where
    foldMap f (NodeS x ts) = f x `mappend` foldMap (foldMap f) ts                    

yyBuild color !yy@(YY y@(Y array (Just row) rowIndex)) =
  case _yc row of
      Nothing -> NodeS Nothing [] 
      Just (True,_) -> NodeS Nothing [] 
      Just (False,_) -> NodeS (Just nuyy) (branch rowUp toCheckUp ++ branch rowDown toCheckDown)        
  where filterSeen = S.findIndicesL fst
        filterMatching = S.findIndicesL ((color==).snd)
        toRow =  maybe (Y S.empty Nothing (-1)) id . _yc . _unyy
        updateArray = update (yRowSeek' color row) rowIndex
        nuyy = yy { _unyy = updateArray y }
        rowUp = shift YN nuyy
        rowDown = shift YS nuyy
        seeks = _yl $ toRow nuyy
        toCheckUp =  filterMatching $ _yl $ toRow rowUp 
        toCheckDown =  filterMatching $ _yl $ toRow rowDown
        --begin recursion
        branch ud [] = [NodeS Nothing []]
        branch ud chk =  (flip fmap) (subSeqs (+1) (filterSeen seeks `intersect` chk))
          $ \case 
          [] -> NodeS Nothing []
          i:_ -> yyBuild color $ adjustYCol i ud { _unyy = updateArray $ _unyy ud }

seqRunL :: Eq a => (a -> a) -> a -> S.Seq a -> S.Seq a
seqRunL u c x@(S.viewl -> S.EmptyL)  = S.empty
seqRunL u c (S.viewl -> (x :< xs)) = if c == x then u x <| seqRunL u c xs else x <| xs

seqRunR :: Eq a => (a -> a) -> a -> S.Seq a -> S.Seq a
seqRunR u c (S.viewr -> S.EmptyR)  = S.empty
seqRunR u c (S.viewr -> (xs :> x)) = if c == x then seqRunR u c xs |> u x else xs |> x

merge' :: Eq t =>  Maybe (YY (Bool, t)) -> Maybe (YY (Bool, t)) -> Maybe (YY (Bool, t))
merge' y1 Nothing = y1
merge' y1 (Just y2) = let mergeArrays (Y a1 _ _) (Y a2 _ _) = Y (S.zipWith (\(i1,i2) (j1,_) -> (or [i1, j1] , i2)) a1 a2) Nothing (-1)
           in  ( & (unyy . yl) %~ ( S.zipWith mergeArrays $ (y2 ^. unyy ^. yl) ) ) <$> y1
