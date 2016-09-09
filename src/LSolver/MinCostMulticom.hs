module Cplexa.MinCostMulticom(generateLinearProblem, getAllPaths, buildAdjacency) where

import qualified Data.Vector as V
import qualified Data.Map as M
import Data.Vector((!),(//))
import Control.Monad
import LSolver.Bindings
import LSolver.Backend.Cplex

generateLinearProblem :: [(Int, Int)] -> [Double] -> [Double] -> [(Int, Int)] -> [Double]
            -> LinearProblem Int
generateLinearProblem edges costs capacity commodities demand =
    let
        n = length costs
        varRange = (0,n-1)
        adj = buildAdjacency edges
        (pathsAll, pathsCom) = genPaths adj commodities
        edgeCosts = genEdgeCosts edges costs
        edgeCapacity = genEdgeCapacity edges capacity
        objFunc = genObjectiveFunction pathsAll edgeCosts
        constraints = genConstraints pathsAll pathsCom edges edgeCapacity demand
    in (objFunc, constraints, standardBounds varRange, varRange)

    --
    -- edges = [(0,1),(1,4),(4,3),(0,2),(2,4),(2,3),(1,2)]
    -- costs = [1,1,1,2,2,2,2] :: [Double]
    -- capacity = [2,2,2,6,6,6,6] :: [Double]
    -- commodities = [(0,4), (1,3)] :: [(Int, Int)]
    -- demand = [3,3] :: [Double]

loadFromFile fileName :: String -> IO LinearProblem Int
loadFromFile fileName = do
    (n:contents) <- readFile fileName
    [e, cost, cap, k, d] = contents
    return generateLinearProblem (read e) (read cost) (read cap) (read k) (read d)

genObjectiveFunction pathsAll edgeCosts = Minimize $ V.toList $ (\p -> sum $
                    (\a -> edgeCosts M.! a) <$> p) <$> pathsAll
genEdgeCosts edges costs = M.fromList $ zip edges costs
genEdgeCapacity edges capacity = M.fromList $ zip edges capacity
genPaths adj commodities =
    let pathsCom = V.fromList $ map V.fromList $ map (\(s,t) -> getAllPaths s t adj) commodities
        pathsAll = V.foldr (V.++) V.empty pathsCom
    in (pathsAll, pathsCom)

genConstraints pathsAll pathsCom edges edgeCapacity demand =
    Sparse $ (genConstraints1 pathsCom pathsAll) ++ (genConstraints2 pathsAll edges)
    where
        genConstraints1 pathsCom pathsAll = zipWith (:=) (createOneArrs pathsCom pathsAll) demand

        genConstraints2 pathsAll edges = let pathsEdges = genPathEdges pathsAll edges in
                 map (\a ->
                    (map (\p -> 1:#p) $
                  pathsEdges M.! a) :< (edgeCapacity M.! a)   ) edges
-- helper, generates ones and zeroes vector to indicate active path
createOneArrs pathsCom pathsAll = map (\xs -> map (\i -> 1:#i) xs) $ createOneArrs' (V.toList pathsCom) 0 (length pathsAll)
createOneArrs' [] _ _ = []
createOneArrs' (p:ps) i n = let pn = V.length p in
    [i..i+pn-1]:createOneArrs' ps (pn+i) n


-- Hilfe functions
genPathEdges paths edges = foldr (\p m -> genPathEdges' p edges m) M.empty [0..V.length paths-1]
    where
        genPathEdges' pid [] m = m
        genPathEdges' pid (e:edges) m
            | e `elem` (paths ! pid) = genPathEdges' pid edges $ M.insertWith (\new old -> if old /= new then old ++ new else new) e [pid] m
            | otherwise = genPathEdges' pid edges m


getBounds :: Ord a => [(a,a)] -> (a,a)
getBounds edges =   let lst = [fst,snd] <*> edges
                    in (minimum lst, maximum lst)


getAllPaths s e adj =   let paths = filter (\xs -> e == last xs) $ filter (\(x:xs) -> x == s) $ getAllPaths' s e adj []
                        in (\p -> tail $ zip (0:p) p) <$> paths
getAllPaths' s e _ paths
    | s == e = map (\a -> reverse $ e:a) paths
getAllPaths' s e adj paths =
                        let startedPaths = liftM (s:) ([]:paths)
                            ps = filter (/= startedPaths) $ (\n -> getAllPaths' n e adj startedPaths) <$> neighbors
                        in concat $ ps
    where neighbors = adj ! s


--buildAdjacency :: [(Int, Int)] -> Array (Int, Int) Int
buildAdjacency edges = let n = length edges
    in buildAdjacency' edges $ V.replicate n []
buildAdjacency' [] arr = arr
buildAdjacency' ((i,j):cs) arr =
    let cur = (arr ! i)
        added = if j `elem` cur then cur else j  : cur
    in buildAdjacency' cs (arr // [(i,added)])
