-- {-# LANGUAGE NoImplicitPrelude #-}
-- import qualified Prelude as P

import Data.Monoid
import Data.List (intercalate)
import qualified Prelude as P
import Prelude hiding ((*), sum)

data Variable = X
              | Y
  deriving Show

data Algebra x = Constant Double
               | Double :* x
               | LinearCombination [Algebra x] 

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

infixr 1 :<
data Constraint x = Algebra x :< Algebra x
                  | Algebra x := Algebra x
                  | Algebra x :> Algebra x
    deriving (Show)



sum xs f = P.sum . flip map

forall = flip map



Constraints v1 <+> Constraints v2 = Constraints $ v1 ++ v2
