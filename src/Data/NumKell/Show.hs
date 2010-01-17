{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.NumKell.Show () where

import Data.HList (HCons(..), HJust(..), HNil(..))
import Data.List (transpose)
import Data.Typeable (Typeable(..))

import Data.NumKell.Funk (Funk(..), (!))

data TableRow i = HeadA | HeadB | Vals i | Hole

instance (Typeable ia, Typeable ib, Integral ia, Integral ib, Show e)
  => Show (Funk (HCons (HJust ia) (HCons (HJust ib) HNil)) e) where
  show arr =
    unlines . map (unwords . zipWith padCell colSizes) $ table
    where
      colSizes = map (maximum . map length) . transpose $ table
      padCell toSize str = replicate (toSize - length str) ' ' ++ str
      HCons (HJust sa) (HCons (HJust sb) HNil) = fSize arr
      edgeMin :: Integral i => i
      edgeMin = 5
      axisIdxs :: Integral i => i -> [TableRow i]
      axisIdxs x
        | x <= edgeMin*2 = map Vals [0 .. x-1]
        | otherwise =
            map Vals [0 .. edgeMin-1]
            ++ [Hole]
            ++ map Vals [x-edgeMin .. x-1]
      table =
        [[cell x y
        | x <- HeadA : HeadB : axisIdxs sb]
        | y <- HeadA : HeadB : axisIdxs sa]
      shortType :: Typeable t => t -> String
      shortType = reverse . takeWhile (/= '.') . reverse . show . typeOf
      cell HeadA HeadB = shortType sa
      cell HeadB HeadA = shortType sb
      cell HeadA HeadA = ""
      cell HeadB HeadB = "+"
      cell HeadB _ = "|"
      cell _ HeadB = "-"
      cell Hole _ = ".."
      cell _ Hole = ".."
      cell HeadA (Vals i) = show (fromIntegral i :: Int)
      cell (Vals i) HeadA = show (fromIntegral i :: Int)
      cell (Vals i) (Vals j) = show (arr ! HCons j (HCons i HNil))

