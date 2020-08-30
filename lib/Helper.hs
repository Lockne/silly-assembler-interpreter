module Helper where

import Register
import qualified Data.HashMap.Strict as M
import Data.Maybe

findRegistersOfInterest :: [Instruction] -> [[Char]]
findRegistersOfInterest ys = filter (/= "") $ go ys
    where go [] = []
          go (x:xs) =  [f x] ++ go xs
          f (Mov a _) = [a]
          f (MovReg a _) = [a]
          f _         = []

extractFromRegistersOfInterest :: [String] -> Registers -> [Value]
extractFromRegistersOfInterest [] reg = []
extractFromRegistersOfInterest (x:xs) reg = fromMaybe 0 (M.lookup (head x) reg) : extractFromRegistersOfInterest xs reg

makeMap :: [String] -> [Value] -> M.HashMap String Value
makeMap xs value = M.fromList $ zip xs value
