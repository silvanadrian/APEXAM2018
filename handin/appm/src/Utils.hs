module Utils where

-- Any auxiliary code to be shared by Parser, Solver, or tests
-- should be placed here.

import Defs

instance Ord Version where
    (<=) (V []) (V []) = True
    (<=) (V ((VN _ _):_)) (V []) = False
    (<=) (V []) (V ((VN _ _):_)) = True
    (<=) (V ((VN v1int v1str) : vnmbr1)) (V ((VN v2int v2str) : vnmbr2))
        | v1int < v2int = True
        | v1int > v2int = False
        | length(v1str) < length(v2str) = True
        | length(v1str) > length(v2str) = False
        | v1str < v2str = True
        | v1str > v2str = False
        | otherwise = (V vnmbr1) <= (V vnmbr2)

merge :: Constrs -> Constrs -> Maybe Constrs
merge [] [] = Just []
merge c1 [] = Just c1
merge [] c2 = Just c2
merge (const:c1) (c2) = case constInC2 const c2 [] of
                            Just x -> merge c1 (x)
                            Nothing -> Nothing

-- Check if Constraint from c1 is in the Constraint list C2
constInC2 :: (PName, PConstr) -> Constrs -> Constrs -> Maybe Constrs
constInC2 const [] x = Just (x ++ [const])
constInC2 const (c2const:c2tail) x =
                case fst const == fst c2const of
                    True -> case mergeConst (snd const) (snd c2const) of
                                Nothing -> Nothing
                                Just mconst -> Just (x ++ [(fst const, mconst)] ++ c2tail)
                    False -> constInC2 const c2tail (x ++ [c2const])

-- Compare the 2 Constraints with
mergeConst :: PConstr -> PConstr -> Maybe PConstr
mergeConst (b1,c1v1,c1v2) (b2,c2v1,c2v2)
        | c1v2 <= c2v1 = Nothing
        | c2v2 <= c1v1 = Nothing
        | b1 == True && b2 == True = Just (b1, (largest c1v1 c2v1), (smallest c1v2 c2v2))
        | b1 == False && b2 == False = Just (b1, (largest c1v1 c2v1), (smallest c1v2 c2v2))
        | b1 == True && b2 == False = Just (b1, (largest c1v1 c2v1), (smallest c1v2 c2v2))
        | b1 == False && b2 == True = Just (b2, (largest c1v1 c2v1), (smallest c1v2 c2v2))
mergeConst _ _ = Nothing

-- Return the smaller of 2 Versions
smallest :: Version -> Version -> Version
smallest v1 v2 =
    case v1 <= v2 of
        True -> v1
        False -> v2

-- Returns the bigger of 2 Versions
largest :: Version -> Version -> Version
largest v1 v2 =
    case v1 >= v2 of
        True -> v1
        False -> v2
