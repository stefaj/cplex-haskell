{-# LANGUAGE FlexibleInstances #-}

module Data.LP(Variable(..)
                       ,Bound(..)
                       ,Constraints(..)
                       ,Optimization(..)
                       ,(<+>)
                       ,Bounds(..)
                       ,Type(..)
                       ,MixedIntegerProblem(..)
                       ,LinearProblem(..)
                       ,MIPSolution(..)
                       ,LPSolution(..)
                       ,simplifyConstraints
                       ,removeEmptyConstraints
                       ) where

import Data.List (intercalate)
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as M
import Data.Hashable
import Data.Monoid

type Map k v = M.HashMap k v

data Variable a = Double :# a

data Bound x =  x :< Double
             |  x :> Double
             |  x := Double
             deriving Show

newtype Constraints a = Constraints [ Bound [Variable a] ]

simplifyVars :: (Eq a, Hashable a) => [Variable a] -> [Variable a]
simplifyVars vars = map (\(v,c) -> c :# v) $ M.toList $ 
                      foldr (\(c :# v) m -> M.insertWith (+) v c m) M.empty vars            
simplifyBounds (xs :< b) = (simplifyVars xs) :< b
simplifyBounds (xs := b) = (simplifyVars xs) := b
simplifyBounds (xs :> b) = (simplifyVars xs) :> b

simplifyConstraints :: (Eq a, Hashable a) => Constraints a -> Constraints a
simplifyConstraints (Constraints cs) = Constraints $ map simplifyBounds cs 

removeEmptyConstraints :: (Eq a, Hashable a) => Constraints a -> Constraints a
removeEmptyConstraints (Constraints cs) = Constraints $ filter isNonEmpty cs
  where
    isNonEmpty ([] :< b) = False
    isNonEmpty ([] := b) = False
    isNonEmpty ([] :> b) = False
    isNonEmpty _ = True

instance Monoid a => Monoid (Constraints a) where
  (Constraints xs) `mappend` (Constraints ys) = Constraints $ xs <> ys
  mempty = Constraints []

Constraints v1 <+> Constraints v2 = Constraints $ v1 ++ v2

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

data LinearProblem a = LP (Optimization a) (Constraints a) [(a, Maybe Double, Maybe Double)]
    deriving Show

data MixedIntegerProblem a = MILP (Optimization a) (Constraints a) [(a, Maybe Double, Maybe Double)]
                                    [(a,Type)] 
     deriving Show

data MIPSolution a = MIPSolution { mipOptimalSol :: Bool, mipObjVal :: Double, mipVars :: Map a Double } deriving (Show)

data LPSolution a = LPSolution { lpOptimalSol :: Bool, lpObjVal :: Double, lpVars :: Map a Double, lpDualVars :: V.Vector Double} deriving (Show)

