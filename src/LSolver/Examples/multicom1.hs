import Data.List
import LSolver.Bindings
import LSolver.Backend.Cplex


objF :: Optimization
objF = Minimize [2, 2]

st :: Constraints Int
st = Sparse [
                [1:#1, 1:#2] := 3
            ]

bnds = [(1, Just 0, Just 2),
        (2,Just 0, Just 2)]

main = do
    sol' (1,2) objF st bnds
    putStrLn "test"
