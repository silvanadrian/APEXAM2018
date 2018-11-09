module SolverImpl where

-- Put your solver implementation in this file.
-- Do not change the types of the following exported functions

import Defs
import Utils




normalize :: Database -> Either String Database
normalize = undefined
-- No More time for that
-- normalize (DB []) = Right (DB [])
-- normalize (DB [(Pkg n1 ver1 desc1 deps1),(Pkg n2 ver2 desc2 deps2)]) = Left "Hello"

solve :: Database -> Constrs -> Sol -> [Sol]
solve = undefined

install :: Database -> PName -> Maybe Sol
install = undefined
