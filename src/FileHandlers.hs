-- Author WRN 2020
module FileHandlers
    ( f_enc,
      f_dec
    ) where

import BitHandlers
import Datatypes
import HuffManAlg

import System.IO
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.ByteString.Lazy as B


import Data.Binary


-- encode
toFile :: Tree -> [Bool] -> B.ByteString
toFile t bs = B.concat [size_enc_t, enc_t, size_bs, enc_bs]
  where
    enc_t = encode t
    size_enc_t = encode $ B.length enc_t
    size_bs = encode $ length bs
    enc_bs = BS.pack $ bitEncoder bs

f_enc f1 f2 = do
  file1 <- openFile f1 ReadMode
  bString <- B.hGetContents file1
  let (t,arr) = hof_encode $ BS.unpack bString
  let strToWrite = toFile t arr
  file2 <- openFile f2 WriteMode
  B.hPut file2 strToWrite
  hClose file2


-- decode
f_dec f1 f2 = do
  file1 <- openFile f1 ReadMode
  ts_s <- B.hGet file1 8
  let t_size = decode ts_s
  t_s <- B.hGet file1 t_size
  let t = (decode t_s) :: Tree
  bs_s <- B.hGet file1 8
  let bs_size = decode bs_s
  bs <- B.hGetContents file1
  let str = hof_decode t $ bitDecoder bs_size $ BS.unpack bs
  file2 <- openFile f2 WriteMode
  B.hPut file2 $ BS.pack  str
  hClose file2
