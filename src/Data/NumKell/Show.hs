{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- Different instances for different dims arrays
-- using method described in HList's slideshow

module Data.NumKell.Show () where

import Data.HList
  ( HCons(..), HHead(..), HLength, HNil(..)
  , HSucc, HZero
  )
import Data.List (transpose)
import Data.Typeable (Typeable(..))

import Data.NumKell.Funk (Funk(..), FunkFuncC(..))

class ShowFunk dim funk where
  -- dummy (dim ->)
  showFunk :: dim -> funk -> String

instance (HLength i dim, ShowFunk dim (Funk i e)) => Show (Funk i e) where
  show = showFunk (undefined :: dim)

fmtTable :: [[String]] -> String
fmtTable table =
  unlines . map (unwords . zipWith padCell colSizes) $ procTable
  where
    procTable = map (map proc) table
    maxLen = 8
    proc str
      | length str <= maxLen = str
      | elem 'e' str = take (maxLen - length postE) preE ++ postE
      | otherwise = take (maxLen-2) str ++ ".."
      where
        (preE, postE) = break (== 'e') str
    colSizes = map (maximum . map length) . transpose $ procTable
    padCell toSize str = replicate (toSize - length str) ' ' ++ str

data TableRow i = HeadA | HeadB | Vals i | Hole

axisIdxs :: Integral i => i -> [TableRow i]
axisIdxs x
  | x <= edgeMin*2 = map Vals [0 .. x-1]
  | otherwise =
      map Vals [0 .. edgeMin-1]
      ++ [Hole]
      ++ map Vals [x-edgeMin .. x-1]
  where
    edgeMin :: Integral i => i
    edgeMin = 5

shortType :: Typeable t => t -> String
shortType = reverse . takeWhile (/= '.') . reverse . show . typeOf

-- show for 0D array
instance Show e => ShowFunk HZero (Funk HNil e) where
  showFunk _ arr =
    "(Funk HNil " ++ show (fVal arr) ++ ")"

-- show for 1D arrays
instance (Typeable ia, Integral ia, Show e)
  => ShowFunk (HSucc HZero) (Funk (HCons ia HNil) e) where
  showFunk _ arr =
    fmtTable [ [ cell x y
    | x <- HeadA : axisIdxs s ]
    | y <- [HeadA, Vals ()] ]
    where
      s = hHead $ funkSize arr
      cell HeadA HeadA = shortType s ++ ":"
      cell (Vals i) HeadA = show (fromIntegral i :: Int)
      cell (Vals i) (Vals ()) = show (fVal arr i)
      cell Hole _ = ".."
      cell _ _ = "Value:"

-- show for 2D arrays
instance (Typeable ia, Typeable ib, Integral ia, Integral ib, Show e)
  => ShowFunk (HSucc (HSucc HZero))
     (Funk (HCons ia (HCons ib HNil)) e) where
  showFunk _ arr =
    fmtTable [ [ cell x y
    | x <- HeadA : HeadB : axisIdxs sb ]
    | y <- HeadA : HeadB : axisIdxs sa ]
    where
      HCons sa (HCons sb HNil) = funkSize arr
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
      cell (Vals i) (Vals j) = show (fVal arr j i)

-- 3D and upwards we just show size/type
instance (Show i, Typeable e)
  => ShowFunk (HSucc (HSucc (HSucc c))) (Funk i e) where
  showFunk _ arr
    = "A Funk of " ++ shortType (undefined :: e)
    ++ "s of size " ++ show (funkSize arr)

