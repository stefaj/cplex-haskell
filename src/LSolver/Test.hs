{-# LANGUAGE ScopedTypeVariables #-}
import LSolver.Bindings
import LSolver.Backend.Cplex

data Vars = X
          | Y
  deriving (Ord, Eq, Show)

main = do
  let obj = Maximize [3,2]
  let cons :: Constraints Vars = Sparse [ [ 1 :# X ] :< 3, [ 1 :# Y] :< 2 ]
  let prob = (obj, cons, [], (0,1))
  print cons
  ans <- solLP prob []
  print ans
