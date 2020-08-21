{-# LANGUAGE DeriveGeneric #-}

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BS
-- import Data.Binary.Get
import Data.Binary
-- import Data.Bits
import GHC.Generics (Generic)

import Data.Char

import Data.Map as M
import Data.List as L
import System.IO

import System.Environment

data Tree = Branch Int Char Tree Tree
          | Leaf Int Char
          deriving (Generic, Eq, Show)
instance Binary Tree


main = do
  getArgs >>= parse_1

parse_1 (f:a1:a2:[]) = case f of
  "encrypt" -> f_enc a1 a2
  "decrypt" -> f_dec a1 a2
  _ -> putStrLn "Invalid first argument"
parse_1 _            = putStrLn "Invalid number of args"


f_enc f1 f2 = do
  file1 <- openFile f1 ReadMode
  bString <- B.hGetContents file1
  let (t,arr) = hof_encode $ BS.unpack bString
  let strToWrite = toFile t arr
  file2 <- openFile f2 WriteMode
  B.hPut file2 strToWrite
  hClose file2

f_dec f1 f2 = do
  file1 <- openFile f1 ReadMode
  ts_s <- B.hGet file1 8
  let t_size = decode ts_s
  print t_size
  t_s <- B.hGet file1 t_size
  let t = (decode t_s) :: Tree
  bs_s <- B.hGet file1 8
  let bs_size = decode bs_s
  print bs_size
  bs <- B.hGetContents file1
  let str = hof_decode t $ L.take bs_size $ bitDecoder $ BS.unpack bs
  file2 <- openFile f2 WriteMode
  B.hPut file2 $ BS.pack  str
  hClose file2


toFile :: Tree -> [Bool] -> B.ByteString
toFile t bs = B.concat [size_enc_t, enc_t, size_bs, enc_bs]
  where
    enc_t = encode t
    size_enc_t = encode $ B.length enc_t
    size_bs = encode $ length bs
    enc_bs = BS.pack $ bitEncoder bs



type HoffMap = Map Char [Bool]


toComp t = case t of
  Branch i c _ _ -> (i,c)
  Leaf   i c     -> (i,c)

instance Ord Tree where
  compare t1 t2 = compare (toComp t1) (toComp t2)


-- generates char -> occurances mapping
toOccMap :: String -> Map Char Int
toOccMap s = fromListWith (+) $ zip s $ repeat 1

-- intermediate Tree map to huffman tree
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

merge :: Tree -> Tree -> Tree
merge t1 t2 = Branch (i1+i2) c1 t1 t2 where
  (i1, c1) = toComp t1
  (i2, _)  = toComp t2

toHoffTree :: String -> Tree
toHoffTree s =
  let
    occMap = toOccMap s
    treeList = L.map (\(c, i) -> Leaf i c) $ toList occMap
    treeMap  = fromList $ zip treeList $ repeat 0
  in mapToHoffTree treeMap


-- generates char -> code mapping (from huffman tree)
genCodeMap :: Tree -> HoffMap
genCodeMap t = fromList $ worker [] t where
  worker s (Branch _ _ t1 t2) = (worker (s++[False]) t1) ++ (worker (s++[True]) t2)
  worker s (Leaf _ c) = [(c, s)]

-- useful functions from here on
hof_encode :: String -> (Tree, [Bool])
hof_encode s = (hoffTree, genString)
  where
    hoffTree = toHoffTree s
    hoffMap = genCodeMap hoffTree
    genString = concat $ L.map (\k -> findWithDefault undefined k hoffMap) s

hof_decode :: Tree -> [Bool] -> String
hof_decode t s =
  worker s t
  where
    worker s (Leaf _ c) = c : worker s t
    worker [] x = if x == t then [] else undefined
    worker (c:s) (Branch _ _ t1 t2) = case c of
      False -> worker s t1
      True -> worker s t2




-- big endian i guess
bitEncoder :: [Bool] -> String
bitEncoder bs = L.map enc_bit $ splitIn8 bs

splitIn8 :: [Bool] -> [[Bool]]
splitIn8 [] = []
splitIn8 b = L.take 8 b : (splitIn8 $ L.drop 8 b)


enc_bit :: [Bool] -> Char
enc_bit i_bs = chr $ worker i_bs 0 8
  where
    worker (b:bs) acc i = let r = if b then 1 else 0 in worker bs (acc*2 + r) (i-1)
    worker [] acc i = case i of
      0 -> acc
      _ -> worker [] (acc*2) (i-1)


bitDecoder :: String -> [Bool]
bitDecoder s = concat $ L.map dec_bit s


dec_bit :: Char -> [Bool]
dec_bit c =
  reverse $ L.take 8 $ worker (ord c) ++ repeat False
  where
    worker c = case c of
      0 -> []
      _ -> (mod c 2 == 1) : (worker (div c 2))






-- plch
