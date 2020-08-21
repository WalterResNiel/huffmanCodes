-- Author WRN 2020
module Main where

-- {-# LANGUAGE DeriveGeneric #-}

import FileHandlers
import System.Environment

main :: IO ()
main = do
  getArgs >>= parse_1

parse_1 :: [String] -> IO ()
parse_1 (f:a1:a2:[]) = case f of
  "encrypt" -> f_enc a1 a2
  "decrypt" -> f_dec a1 a2
  _ -> putStrLn "Invalid first argument"
parse_1 _            = putStrLn "Invalid number of args"
