{-# LANGUAGE ScopedTypeVariables #-}
import LSolver.Bindings
import LSolver.Backend.Cplex

data Vars = X
          | Y
  deriving (Ord, Eq, Show)

main = do
  let obj = Maximize [ 3 :# X, 1 :# Y]
  let cons = Constraints [ 
                                            [1 :# X, 1 :# Y] :< 3,
                                            [1 :# Y] :> 1
                                         ]
  let prob = LP (obj, cons, [])
  print cons
  ans <- solLP prob []
  print ans
