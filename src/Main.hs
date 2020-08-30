module Main where

import Register
import Eval
import Helper
import System.IO
import Parser
import qualified Data.HashMap.Strict as M

simpleAssembler :: [String] -> M.HashMap String Value
simpleAssembler commandset =
    makeMap xs value
    where xs = findRegistersOfInterest (map readExpr commandset)
          value = extractFromRegistersOfInterest xs (snd $ runEval (evalTape $ map readExpr commandset) register)

main :: IO ()
main = do
  putStrLn "hello world"
  filepath <- getLine
  commandset <- lines <$> readFile filepath
  print $ simpleAssembler commandset
