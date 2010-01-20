{-# LANGUAGE FlexibleContexts, FlexibleInstances, TypeFamilies #-}

module Data.NumKell.Memo
  ( FMemoIdx(..), fFromList, fMemo
  ) where

import Control.Applicative ((<$>), (<*>))
import Data.Array (Ix, (!), listArray)
import Data.HList (HCons(..), HJust(..), HNil(..))

import Data.NumKell.Funk (Funk(..))
import Data.NumKell.HListUtil (HCatMaybesC(..))

data FMemoIdxFuncs i = FMemoIdxFuncs
  { fMemoArrIdx :: HCatMaybes i -> FMemoArrIdx i
  , fMemoArrBounds :: i -> (FMemoArrIdx i, FMemoArrIdx i)
  , fAllIdxs :: i -> [HCatMaybes i]
  }

type family FFromListType i a
type instance FFromListType HNil a = a
type instance FFromListType (HCons (HJust a) as) b
  = [FFromListType as b]

class FMemoIdx i where
  type FMemoArrIdx i
  fMemoIdxFuncs :: FMemoIdxFuncs i
  fFromListArgs :: FFromListType i e -> (i, [e])

fMemo :: (FMemoIdx i, Ix (FMemoArrIdx i)) => Funk i e -> Funk i e
fMemo funk =
  Funk
  { fSize = fSize funk
  , fIndex = (arr !) . fMemoArrIdx tbl
  }
  where
    tbl = fMemoIdxFuncs
    arr
      = listArray (fMemoArrBounds tbl (fSize funk))
      . map (fIndex funk) . fAllIdxs tbl . fSize $ funk

fFromList :: (FMemoIdx i, Ix (FMemoArrIdx i))
  => FFromListType i e -> Funk i e
fFromList lst =
  Funk
  { fSize = sz
  , fIndex = (arr !) . fMemoArrIdx tbl
  }
  where
    tbl = fMemoIdxFuncs
    (sz, elems) = fFromListArgs lst
    arr = listArray (fMemoArrBounds tbl sz) elems

instance FMemoIdx HNil where
  type FMemoArrIdx HNil = ()
  fMemoIdxFuncs = FMemoIdxFuncs (const ()) (const ((), ())) (const [HNil])
  fFromListArgs x = (HNil, [x])

instance (FMemoIdx as, Integral a)
  => FMemoIdx (HCons (HJust a) as) where
  type FMemoArrIdx (HCons (HJust a) as)
    = (Int, FMemoArrIdx as)
  fMemoIdxFuncs =
    FMemoIdxFuncs ix bnds allIdx
    where
      tbl = fMemoIdxFuncs
      ix (HCons x xs) = (fromIntegral x, fMemoArrIdx tbl xs)
      bnds (HCons (HJust x) xs) =
        ((0, inStart), (fromIntegral x - 1, inEnd))
        where
          (inStart, inEnd) = fMemoArrBounds tbl xs
      allIdx (HCons (HJust x) xs) =
        HCons <$> [0 .. fromIntegral x - 1] <*> fAllIdxs tbl xs
  fFromListArgs lst =
    ( HCons
      ((HJust . fromIntegral . length) lst)
      ((fst . head) elemArgs)
    , elemArgs >>= snd)
    where
      elemArgs = map fFromListArgs lst
