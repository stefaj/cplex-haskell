{-# LANGUAGE ScopedTypeVariables #-}
import LSolver.Bindings
import LSolver.Backend.Cplex
import qualified Data.Map as M
import Data.Monoid
import Prelude hiding (sum)

data Vars = U Edge -- Capacity
          | F Path -- Flow
  deriving (Ord, Eq)


type Edge = (Int, Int)
newtype Path = Path [Edge]
  deriving (Eq, Ord, Show)

instance Show Vars where
  show (U x) = "U_" ++ show x ++ " "
  show (F x) = "F_" ++ show x ++ " "


costs = [20,5,5,5,20,20]

edges = [(0,1), (1,2), (2,5), (0,3), (3,4), (4,5)]

ec = M.fromList $ zip edges costs

caps =  [3,3,3,3,3,3]

commodities = [(0,5)]

demand = M.fromList $ zip commodities [2]
p1 = Path [(0,1), (1,2), (2,5)]
p2 = Path [(0,3), (3,4), (4,5)]
pk = M.fromList $ zip commodities [[p1,p2]]

pa = M.fromList $ zip edges [[p1],[p1],[p1],[p2],[p2],[p2]]

main = do
  let obj = Minimize $ sum edges $ \a -> (ec M.! a) :# U a  -- Minimize ca ua 
  let flowc = Constraints $ forall commodities $ \k -> 
                  (concat $ sum (pk M.! k) $ \p -> [1 :# F p]  ) := (demand M.! k)
  let flowcapc = Constraints $ forall edges $ \a ->
                  (concat $ sum (pa M.! a) $ \p -> [1 :# F p, (-1) :# (U a)]) :< 0
  let types = forall edges $ \a -> (U a, TInteger)
  let bounds = [(F p1, Just 0, Nothing), (F p2, Just 0, Nothing)]
  print obj
  print flowc
  print flowcapc
  let prob = MILP obj (flowc <+> flowcapc) bounds types
  print prob
  ans <- solMIP prob [] defaultCallBacks
  putStrLn "\n"
  print ans


constraint set func = map func set
forall set func = map func set
sum set func = map func set


-- constraint coms $ \k -> (  sum (pk !! k) $ \p -> F p  ) := d !! k
