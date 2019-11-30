module Main where

import Language.Quartz
import System.Environment

main :: IO ()
main = do
  args   <- getArgs
  body   <- readFile $ head args
  result <- runModule body
  putStrLn $ ">>> " ++ show result
