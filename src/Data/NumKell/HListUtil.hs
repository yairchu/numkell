{-# LANGUAGE FlexibleInstances, TypeFamilies #-}

module Data.NumKell.HListUtil
  ( HCatMaybesC(..)
  ) where

import Data.HList (HCons(..), HJust(..), HNil(..), HNothing(..))

-- | HCatMaybes transforms an HList
-- (HJust a :*: HNothing :*: HJust b :*: ...)
-- to an HList
-- (a :*: b :*: ...)
class HCatMaybesC i where
  type HCatMaybes i
  hCatMaybes :: i -> HCatMaybes i

instance HCatMaybesC HNil where
  type HCatMaybes HNil = HNil
  hCatMaybes = id

instance HCatMaybesC as => HCatMaybesC (HCons HNothing as) where
  type HCatMaybes (HCons HNothing as) = HCatMaybes as
  hCatMaybes (HCons _ xs) = hCatMaybes xs

instance HCatMaybesC as => HCatMaybesC (HCons (HJust a) as) where
  type HCatMaybes (HCons (HJust a) as) = HCons a (HCatMaybes as)
  hCatMaybes (HCons (HJust x) xs) = HCons x (hCatMaybes xs)


