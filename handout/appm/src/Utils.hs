module Utils where

-- Any auxiliary code to be shared by Parser, Solver, or tests
-- should be placed here.

import Defs

instance Ord Version where
  (<=) (V[]) _ = False
  (<=) (V(_:_)) (V []) = True
  (<=) (V[VN v1int v1str])  (V[VN v2int v2str]) =
    if checkVersion v1int v2int v1str v2str then True else False
  (<=) (V(VN _ _:xs))  (V(VN _ _:ys)) = V(xs) <= V(ys)

checkVersion :: Int -> Int -> String -> String -> Bool
checkVersion a b c d = a <= b && (c <= d || length(c) <= length(d))

merge :: Constrs -> Constrs -> Maybe Constrs
merge [] [] = Just []
merge c1 [] = Just c1
merge [] c2 = Just c2
merge (c1) (c2) = Just (c1 ++ c2)
