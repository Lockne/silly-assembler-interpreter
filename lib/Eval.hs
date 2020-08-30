module Eval where

import Register
import qualified Data.HashMap.Strict as M
import Data.Maybe

newtype Eval s a =
  Eval
    { runEval :: s -> (a, s)
    }

instance Functor (Eval s) where
  fmap f (Eval g) = Eval $ \x -> (f $ fst $ g x, snd $ g x)

instance Applicative (Eval s) where
  pure a = Eval $ \x -> (a, x)
  (Eval f) <*> (Eval g) = Eval $ \s -> (fst (f s) $ fst (g s), snd $ g s)

instance Monad (Eval s) where
  return = pure
  m >>= k =
    Eval $ \s ->
      let (a, s') = runEval m s
       in runEval (k a) s

eval :: Instruction -> Eval Registers ()
eval (Mov k v) = Eval $ \reg -> ((), M.insert k v reg)
eval (Inc k) =
  Eval $ \reg -> ((), M.insert k (fromMaybe 0 (M.lookup k reg) + 1) reg)
eval (Dec k) =
  Eval $ \reg -> ((), M.insert k (fromMaybe 0 (M.lookup k reg) - 1) reg)
eval (MovReg k c) =
  Eval $ \reg -> ((), M.insert k (fromMaybe 0 (M.lookup c reg)) reg)

evalTape :: [Instruction] -> Eval Registers ()
evalTape xs = do
  let reg = M.fromList (zip ['a' .. 'z'] [0,0 ..])
  let loop k ys
        | k == length xs = return ()
      loop k ys
        | k < length xs = do
          case xs !! k of
            Jnz a v -> do
              let ((), newReg) = runEval (evalTape ys) reg
              case M.lookup a newReg == Just 0 of
                False -> do
                  eval (xs !! ((k + fromInteger v) `mod` length xs))
                  loop
                    ((k + fromInteger v) `mod` length xs + 1)
                    (ys ++ [xs !! ((k + fromInteger v) `mod` length xs)])
                True -> do
                  loop (k + 1) ys
            _ -> do
              eval (xs !! k)
              loop (k + 1) (ys ++ [xs !! k])
  loop 0 []
