{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Week05.Calc
  ( eval
  , evalStr
  , Expr(..)
  , MinMax(..)
  , Mod7(..)
  , compile
  , HasVars(..)
  , VarExprT(..)
  ) where

import Week05.ExprT
import Week05.Parser
import qualified Week05.StackVM as SVM
import qualified Data.Map as M

--------------------------------------------------- Exercise 1

eval :: ExprT -> Integer
eval (Lit i) = i
eval (Add a b) = eval a + eval b
eval (Mul a b) = eval a * eval b

--------------------------------------------------- Exercise 2

evalStr :: String -> Maybe Integer
evalStr str = fmap eval (parseExp Lit Add Mul str)

--------------------------------------------------- Exercise 3

-- This is our type class :)
class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

-- now write an instance for our ExprT type
instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul

--------------------------------------------------- Exercise 4
-- Write instances for Integer, Bool, MinMax, and Mod7

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit = (/= 0)
  add = (||)
  mul = (&&)

instance Expr MinMax where
  lit = MinMax
  add (MinMax a) (MinMax b) = lit $ max a b
  mul (MinMax a) (MinMax b) = lit $ min a b

instance Expr Mod7 where
  lit = Mod7 . (`mod` 7)
  add (Mod7 a) (Mod7 b) = lit $ a + b
  mul (Mod7 a) (Mod7 b) = lit $ a * b

--------------------------------------------------- Exercise 5

instance Expr SVM.Program where
  lit = (:[]) . SVM.PushI
  add = error "Week05.Calc#mul not implemented for Program"
  mul = error "Week05.Calc#mul not implemented for Program"

compile :: String -> SVM.Program
compile = error "Week05.Calc#compile not implemented"

--------------------------------------------------- Exercise 6

class HasVars a where
  var :: String -> a

data VarExprT = LitWithVar Integer
  | AddWithVar VarExprT VarExprT
  | MulWithVar VarExprT VarExprT
  | Var String
  deriving (Show, Eq)

instance Expr VarExprT where
  lit = error "Week05.Calc#lit not implemented for VarExprT"
  add = error "Week05.Calc#add not implemented for VarExprT"
  mul = error "Week05.Calc#mul not implemented for VarExprT"

instance HasVars VarExprT where
  var = error "Week05.Calc#var not implemented for VarExprT"

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var = error "Week05.Calc#var not implemented for (M.Map String Integer -> Maybe Integer)"

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit = error "Week05.Calc#lit not implemented for (M.Map String Integer -> Maybe Integer)"
  add = error "Week05.Calc#add not implemented for (M.Map String Integer -> Maybe Integer)"
  mul = error "Week05.Calc#mul not implemented for (M.Map String Integer -> Maybe Integer)"
