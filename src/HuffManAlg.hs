-- Author WRN 2020
module HuffManAlg
    ( hof_encode,
      hof_decode
    ) where


import Data.List as L
import Data.Map as M
import Datatypes

-- encode
hof_encode :: String -> (Tree, [Bool])
hof_encode s = (hoffTree, genString)
  where
    hoffTree = toHoffTree s
    hoffMap = genCodeMap hoffTree
    genString = concat $ L.map (\k -> findWithDefault undefined k hoffMap) s

-- generates char -> code mapping (from huffman tree)
genCodeMap :: Tree -> HoffMap
genCodeMap t = fromList $ worker [] t where
  worker s (Branch _ _ t1 t2) = (worker (s++[False]) t1) ++ (worker (s++[True]) t2)
  worker s (Leaf _ c) = [(c, s)]

toHoffTree :: String -> Tree
toHoffTree s =
  let
    occMap = toOccMap s
    treeList = L.map (\(c, i) -> Leaf i c) $ toList occMap
    treeMap  = fromList $ zip treeList $ repeat 0
  in mapToHoffTree treeMap

-- generates char -> occurances mapping
toOccMap :: String -> Map Char Int
toOccMap s = fromListWith (+) $ zip s $ repeat 1

-- intermediate Tree map to huffman tree (uses maps as prioQue)
mapToHoffTree :: Map Tree Int -> Tree
mapToHoffTree m =
  case size m of
    0 -> undefined
    1 -> let (k,_) = findMin m in k
    _ -> let
      ((k1,_), m1) = deleteFindMin m
      ((k2,_), m2) = deleteFindMin m1
      m3 = M.insert (merge k1 k2) 0 m2
      in mapToHoffTree m3


--decode
hof_decode :: Tree -> [Bool] -> String
hof_decode t s =
  worker s t
  where
    worker s (Leaf _ c) = c : worker s t
    worker [] x = if x == t then [] else undefined
    worker (c:s) (Branch _ _ t1 t2) = case c of
      False -> worker s t1
      True -> worker s t2
