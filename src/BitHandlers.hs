-- Author WRN 2020
module BitHandlers
    ( bitEncoder,
      bitDecoder
    ) where

-- import Data.List

-- encode and decode bool arrays to bit array strings

import Data.List as L
import Data.Char

-- encode
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

-- decoder
-- length string - boolList
bitDecoder :: Int -> String -> [Bool]
bitDecoder i s = L.take i $ concat $ L.map dec_bit s

dec_bit :: Char -> [Bool]
dec_bit c =
  reverse $ L.take 8 $ worker (ord c) ++ repeat False
  where
    worker c = case c of
      0 -> []
      _ -> (mod c 2 == 1) : (worker (div c 2))
