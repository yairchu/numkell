{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}

module Data.NumKell.NewAxes
  ( FNewAxesRes, FNewAxes
  ) where

import Data.HList (HCons(..), HFalse(..), HNil(..), HTrue(..))

import Data.NumKell.Funk (HLMaybes, Funk(..))

type family FNewAxesRes i d
type instance FNewAxesRes HNil HNil = HNil
type instance FNewAxesRes xs (HCons HTrue ys)
  = HCons Int (FNewAxesRes xs ys)
type instance FNewAxesRes (HCons x xs) (HCons HFalse ys)
  = HCons x (FNewAxesRes xs ys)

data FNewAxesFuncs i d =
  FNewAxesFuncs
  { newAxesSize :: HLMaybes i -> d -> HLMaybes (FNewAxesRes i d)
  , newAxesIdx :: d -> FNewAxesRes i d -> i
  }

class FNewAxes i d where
  newAxesFuncs :: FNewAxesFuncs i d

newAxes :: FNewAxes i d => Funk i e -> d -> Funk (FNewAxesRes i d) e
newAxes funk axes =
  Funk
  { fSize = newAxesSize tbl (fSize funk) axes
  , fIndex = fIndex funk . newAxesIdx tbl axes
  }
  where
    tbl = newAxesFuncs

instance FNewAxes HNil HNil where
  newAxesFuncs =
    FNewAxesFuncs
    ((const . const) HNil)
    ((const . const) HNil)

instance FNewAxes xs ys => FNewAxes xs (HCons HTrue ys) where
  newAxesFuncs =
    FNewAxesFuncs sz idx
    where
      sz xs (HCons _ ys) = HCons Nothing (newAxesSize tbl xs ys)
      idx (HCons _ xs) (HCons _ ys) = newAxesIdx tbl xs ys
      tbl = newAxesFuncs

instance FNewAxes xs ys
  => FNewAxes (HCons x xs) (HCons HFalse ys) where
  newAxesFuncs =
    FNewAxesFuncs sz idx
    where
      sz (HCons x xs) (HCons _ ys) =
        HCons x (newAxesSize tbl xs ys)
      idx (HCons _ xs) (HCons y ys) =
        HCons y (newAxesIdx tbl xs ys)
      tbl = newAxesFuncs

