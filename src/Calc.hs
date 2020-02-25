{-# LANGUAGE FlexibleInstances #-}
module Calc where

import Control.Monad (liftM2)
import qualified Data.Map as M

import qualified ExprT as ET
import Parser
import qualified StackVM as VM

-- Homework 5
-- exercise 1
eval :: ET.ExprT -> Integer
eval (ET.Lit x) = x
eval (ET.Add x y) = (eval x) + (eval y)
eval (ET.Mul x y) = (eval x) * (eval y)


-- exercise 2
evalStr :: String -> Maybe Integer
evalStr str = do
    ast <- parseExp ET.Lit ET.Add ET.Mul str
    return $ eval ast


-- exercise 3
class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr ET.ExprT where
    lit = ET.Lit
    add = ET.Add
    mul = ET.Mul


-- exercise 4
instance Expr Integer where
    lit = id
    add = (+)
    mul = (*)


instance Expr Bool where
    lit = (> 0)
    add = (||)
    mul = (&&)


newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr MinMax where
    lit = MinMax
    add (MinMax a) (MinMax b) = lit $ max a b
    mul (MinMax a) (MinMax b) = lit $ min a b


instance Expr Mod7 where
    lit = Mod7 . flip mod 7
    add (Mod7 a) (Mod7 b) = lit $ a + b
    mul (Mod7 a) (Mod7 b) = lit $ a * b

-- exercise 5
instance Expr VM.Program where
    lit i = [VM.PushI i]
    add a b = concat [a, b, [VM.Add]]
    mul a b = concat [a, b, [VM.Mul]]


compile :: String -> Maybe VM.Program
compile = parseExp lit add mul

-- exercise 6
class HasVars a where
    var :: String -> a

data VarExprT = Lit Integer
        | Add VarExprT VarExprT
        | Mul VarExprT VarExprT
        | Var String
    deriving (Show, Eq)

instance Expr VarExprT where
    lit = Lit
    add = Add
    mul = Mul

instance HasVars VarExprT where
    var = Var

instance Expr (M.Map String Integer -> Maybe Integer) where
    lit = const . Just
    add a b = \env -> (liftM2 (+)) (a env) (b env)
    mul a b = \env -> (liftM2 (*)) (a env) (b env)

instance HasVars (M.Map String Integer -> Maybe Integer) where
    var = M.lookup

withVars :: [(String, Integer)]
            -> (M.Map String Integer -> Maybe Integer)
            -> Maybe Integer
withVars vals expr = expr $ M.fromList vals
