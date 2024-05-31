module Main where

import LC

main :: IO ()
main = do
  let term = App (Lam "x" (Var "x")) (Var "y")
  print $ tnf 10 term