{-# LANGUAGE ScopedTypeVariables, TypeFamilies #-}

module Data.NumKell.Funk
  ( HLMaybes, Funk(..)
  , (!)
  ) where

import Control.Applicative (Applicative(..), liftA2)
import Data.Generics.Aliases (orElse)
import Data.HList (HCons(..), HNil(..), hTail)

-- | HLMaybes transforms an HList
-- (a :*: b :*: ...)
-- to a
-- (Maybe a :*: Maybe b :*: ...)
type family HLMaybes i
type instance HLMaybes HNil = HNil
type instance HLMaybes (HCons a as) = HCons (Maybe a) (HLMaybes as)

data Funk i e = Funk
  { fSize :: HLMaybes i
  , fIndex :: i -> e -- named after []'s genericIndex
  }

(!) :: Funk i e -> i -> e
(!) = fIndex

instance Functor (Funk i) where
  fmap f a = a { fIndex = fmap f (fIndex a) }

class FunkIdx i where
  -- dummy (i ->)
  funkMergeSizes :: i -> HLMaybes i -> HLMaybes i -> HLMaybes i
  -- dummy (i ->)
  funkBcastSize :: i -> HLMaybes i

instance FunkIdx HNil where
  funkMergeSizes _ _ _ = HNil
  funkBcastSize _ = HNil

instance (Eq a, FunkIdx as)
  => FunkIdx (HCons a as) where
  funkMergeSizes dummy (HCons x xs) (HCons y ys)
    | Just False == liftA2 (==) x y = error "Funk shapes mismatch"
    | otherwise = HCons (orElse x y) (funkMergeSizes (hTail dummy) xs ys)
  funkBcastSize = HCons Nothing . funkBcastSize . hTail

instance FunkIdx i => Applicative (Funk i) where
  pure val =
    Funk
    { fSize = funkBcastSize (undefined :: i)
    , fIndex = const val
    }
  x <*> y =
    Funk
    { fSize = funkMergeSizes (undefined :: i) (fSize x) (fSize y)
    , fIndex = f
    }
    where
      f idx = fIndex x idx $ fIndex y idx

