{-# LANGUAGE FlexibleInstances #-}

module LSolver.Bindings(Variable(..), Bound(..), Constraints(..), Optimization(..),
            Bounds(..), Type(..), MixedIntegerProblem(..), LinearProblem(..), MIPSolution(..), LPSolution(..)) where

import Data.List (intercalate)
import qualified Data.Vector as V
import qualified Data.Map as M
import Data.Monoid

data Variable a = Double :# a

data Bound x =  x :< Double
             |  x :> Double
             |  x := Double
             deriving Show

newtype Constraints a = Constraints [ Bound [Variable a] ]

instance Monoid a => Monoid (Constraints a) where
  (Constraints xs) `mappend` (Constraints ys) = Constraints $ xs <> ys
  mempty = Constraints []


data Optimization a = Maximize [Variable a]
                    | Minimize [Variable a]

data Type = TContinuous | TInteger | TBinary

instance (Show a) => Show (Variable a) where
    show (d :# v)
      | d == (-1) = "-" ++ (show v)
      | d == 1 = (show v)
      | otherwise = (show d) ++ "x" ++ (show v)

instance Show a => Show (Optimization a) where
  show (Minimize xs) = "Minimize\n\t" ++ (intercalate "+" $ map show xs)
  show (Maximize xs) = "Maximize\n\t" ++ (intercalate "+" $ map show xs)

showVars xs = intercalate " + " $ map show $ zipWith (:#) xs [0..]

instance (Show a) => Show (Constraints a) where
    show (Constraints bounds) = "\nSubject to\n" ++ (unlines $  map (\a -> "\t" ++ a) $ 
                            map getVarSigns bounds)

printVars xs = intercalate " + " $ map show xs
getVarSigns (x :< v) = (printVars x) ++ " <= " ++ (show v)
getVarSigns (x :> v) = (printVars x) ++ " >= " ++ (show v)
getVarSigns (x := v) = (printVars x) ++ " == " ++ (show v)

instance Show Type where
  show TContinuous = "Continous"
  show TInteger = "Integer"
  show TBinary = "Binary"


type Bounds = [Bound Int]

newtype LinearProblem a = LP ( Optimization a, Constraints a, [(a, Maybe Double, Maybe Double)] )

newtype MixedIntegerProblem a = MILP ( Optimization a, Constraints a, [(a, Maybe Double, Maybe Double)],
                                    [(a,Type)] )

data MIPSolution a = MIPSolution { mipOptimalSol :: Bool, mipObjVal :: Double, mipVars :: M.Map a Double } deriving (Show)

data LPSolution a = LPSolution { lpOptimalSol :: Bool, lpObjVal :: Double, lpVars :: M.Map a Double, lpDualVars :: V.Vector Double} deriving (Show)

