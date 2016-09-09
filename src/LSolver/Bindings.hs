{-# LANGUAGE FlexibleInstances #-}

module LSolver.Bindings(Variable(..), Bound(..), Constraints(..), Optimization(..),
            Bounds(..), Type(..), MixedIntegerProblem, LinearProblem, MIPSolution(..), LPSolution(..)) where

import Data.Ix as I
import Data.List (intercalate)
import qualified Data.Vector as V

data Variable a =  Double :# a

data Bound x =  x :< Double
             |  x :> Double
             |  x := Double
             deriving Show

data Constraints a = Sparse  [ Bound [Variable a] ]

data Optimization = Maximize [Double]
                  | Minimize [Double] 

data Type = TContinuous | TInteger | TBinary

instance (Show a) => Show (Variable a) where
    show (d :# v)
      | d == (-1) = "-x" ++ (show v)
      | d == 1 = "x" ++ (show v)
      | otherwise = (show d) ++ "x" ++ (show v)
instance Show Optimization where
  show (Minimize xs) = "Minimize\n\t" ++ (showVars xs)
  show (Maximize xs) = "Maximize\n\t" ++ (showVars xs)

showVars xs = intercalate " + " $ map show $ zipWith (:#) xs [0..]

instance (Show a) => Show (Constraints a) where
    show (Sparse bounds) = "\nSubject to\n" ++ (unlines $  map (\a -> "\t" ++ a) $ 
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

type LinearProblem a = (Optimization, Constraints a, [(a, Maybe Double, Maybe Double)], (a,a))

type MixedIntegerProblem a = (Optimization, Constraints a, [(a, Maybe Double, Maybe Double)],
                                [(a,Type)], (a,a))

data MIPSolution = MIPSolution { mipOptimalSol :: Bool, mipObjVal :: Double, mipVars :: V.Vector Double} deriving (Show)

data LPSolution = LPSolution { lpOptimalSol :: Bool, lpObjVal :: Double, lpVars :: V.Vector Double, lpDualVars :: V.Vector Double} deriving (Show)
