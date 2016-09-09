import Data.List
import LSolver.Bindings
import LSolver.Backend.Cplex

objF :: Optimization
objF = Minimize [2, 4, 5, 4, 3]

st :: Constraints Int
st = Sparse [
                [1:#1, 1:#2, 1:#3] := 3,
                [1:#4, 1:#5] := 3,
                [1:#1, 1:#3, 1:#4] :< 2,
                [1:#1, 1:#5] :< 2,
                [1:#5] :< 2,
                [1:#2] :< 6,
                [1:#2, 1:#3] :< 6,
                [1:#4] :< 6
            ]

varRange = (1,5)
bnds = standardBounds varRange

main = do
    sol' varRange objF st bnds
    putStrLn "test"
