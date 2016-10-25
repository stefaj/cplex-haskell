{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.LP( -- Variable(..)
                       Constraint(..)
                       ,Algebra(..)
                       -- ,(*)
                       ,sum
                       ,forall
                       -- ,I.Constraints(..)
                       ,I.Optimization(..)
                       ,I.Bounds(..)
                       ,I.Type(..)
                       ,MixedIntegerProblem(..)
                       ,LinearProblem(..)
                       ,I.MIPSolution(..)
                       ,I.LPSolution(..)
                       -- ,I.simplifyConstraints
                       -- ,I.removeEmptyConstraints
                       ,buildConstraints
                       ) where

import Data.Monoid
import Data.List (intercalate)
import qualified Prelude as P
import qualified Data.HashMap.Strict as M
import Prelude hiding ((*), sum)
import qualified Data.Internal as I
import Data.Hashable

data Algebra x = Constant Double
               | Double :* x
               | LinearCombination [Algebra x] 

data Constraints x = Constraints [Constraint x]

instance Monoid a => Monoid (Constraints a) where
  (Constraints xs) `mappend` (Constraints ys) = Constraints $ xs <> ys
  mempty = Constraints []

instance Show x => Show (Algebra x) where
  show (Constant d) = show d
  show (d :* x) = show d <> show x
  show (LinearCombination xs) = intercalate " + " $ map show xs


(*) :: Double -> x -> Algebra x
a * b = a :* b

instance Num (Algebra x) where
  fromInteger i = Constant $ fromIntegral i
  a1 + (LinearCombination xs) = LinearCombination (a1:xs)
  (LinearCombination xs) + a2 = LinearCombination (xs++[a2])
  (Constant a) + (Constant b) = Constant (a+b)
  a + b = LinearCombination [a,b]
  negate (Constant d) = Constant $ negate d
  negate (d :* v) = (negate d) :* v
  negate (LinearCombination xs) = LinearCombination $ map negate xs

infixr 1 :<
data Constraint x = Algebra x :< Algebra x
                  | Algebra x := Algebra x
                  | Algebra x :> Algebra x
    deriving (Show)



sum xs f = P.sum $ flip map f xs

forall = flip map

simplify :: (Eq a, Hashable a) => Algebra a -> Algebra a
simplify a@(Constant d) = a
simplify a@(0 :* x) = Constant 0
simplify a@(d :* x) = a
simplify i@(LinearCombination xs) = const + (LinearCombination $ map (\(v,c) -> c :* v) $ M.toList $ 
                      foldr (\(v,c) m -> M.insertWith (+) v c m) M.empty $ getVars i)
  where const = Constant $ getConstant i

getConstant (Constant c) = c
getConstant (d :* v) = 0
getConstant (LinearCombination xs) = P.sum $ map getConstant xs

getVars :: Algebra x -> [(x,Double)]
getVars (d :* v) = [(v,d)]
getVars (LinearCombination xs) = map aux $ filter (\u -> case u of
                                                                  d :* v -> True
                                                                  _ -> False) xs
  where aux (d :* v) = (v,d)                                                                        

buildConstraint :: forall x. (Hashable x, Eq x) => Constraint x -> I.Bound [I.Variable x] 
buildConstraint constr = case constr of 
            (_ :< _ ) -> lhs I.:< rhs
            (_ := _ ) -> lhs I.:= rhs
            (_ :> _ ) -> lhs I.:> rhs
  where
    v = simplify (ol - or)
    vars :: [(x,Double)] = getVars v
    lhs :: [I.Variable x] = map (\(v,d) -> d I.:# v) vars
    rhs = getConstant v
    ol = case constr of
            (a :< _) -> a
            (a := _) -> a
            (a :> _) -> a
    or = case constr of
            (_ :< b) -> b
            (_ := b) -> b
            (_ :> b) -> b

buildConstraints :: (Eq x, Hashable x) => Constraints x -> I.Constraints x
buildConstraints (Constraints constrs) = I.Constraints $ map buildConstraint constrs

data LinearProblem a = LP (I.Optimization a) (Constraints a) [(a, Maybe Double, Maybe Double)]
    -- deriving Show

data MixedIntegerProblem a = MILP (I.Optimization a) (Constraints a) [(a, Maybe Double, Maybe Double)]
                                    [(a,I.Type)] 
    -- deriving Show
