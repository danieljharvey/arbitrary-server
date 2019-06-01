module Main where

import qualified Lib as Lib

main :: IO ()
main = do
  putStrLn "Starting server..."
  Lib.main
