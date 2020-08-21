-- Author WRN 2020
module Datatypes
    ( HoffMap,
      Tree(Leaf,Branch),
      merge
    ) where

import Data.Binary
import Data.Map

import GHC.Generics (Generic)

type HoffMap = Map Char [Bool]

-- branches retain a single char from a leaf (for sorting)

data Tree = Branch Int Char Tree Tree
          | Leaf Int Char
          deriving (Generic, Eq, Show)
instance Binary Tree
instance Ord Tree where
  compare t1 t2 = compare (toComp t1) (toComp t2)

toComp :: Tree -> (Int,Char)
toComp t = case t of
  Branch i c _ _ -> (i,c)
  Leaf   i c     -> (i,c)

merge :: Tree -> Tree -> Tree
merge t1 t2 = Branch (i1+i2) c1 t1 t2 where
  (i1, c1) = toComp t1
  (i2, _)  = toComp t2
