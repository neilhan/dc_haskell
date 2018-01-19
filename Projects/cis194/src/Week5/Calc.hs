{-# LANGUAGE TypeSynonymInstances #-}

module Week5.Calc where

import Week5.ExprT
import Week5.Parser
import qualified Week5.StackVM as Stack

-- exercise 1, eval
eval :: ExprT -> Integer
eval (Lit i) = i
eval (Add a b) = (eval a) + (eval b)
eval (Mul a b) = (eval a) * (eval b)


-- exercise 2, parse string --------------------------
evalStr :: String -> Maybe Integer
evalStr str = do
    exp <- parseExp Lit Add Mul str
    return $ eval exp

-- exercise 3, define type class --------------------------
class Expr a where
  lit :: Integer -> a
  mul :: a -> a -> a
  add :: a -> a -> a

instance Expr ExprT where
  lit = Lit
  mul = Mul
  add = Add

-- (mul (lit 1) (lit 2))::ExprT works as well
reify :: ExprT -> ExprT
reify = id

-- exercise 4, Expr instances --------------------------
newtype MinMax = MinMax Integer deriving (Eq, Show, Ord)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Integer where
  lit = id
  mul = (*)
  add = (+)

instance Expr Bool where
  lit = (>0)
  add = (||)
  mul = (&&)

instance Expr MinMax where
  lit = MinMax
  add = max
  mul = min

instance Expr Mod7 where
  lit a = Mod7 (mod a 7)
  add (Mod7 a) (Mod7 b) = lit (a + b)
  mul (Mod7 a) (Mod7 b) = lit (a * b)

-- testing code
testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7

-- exercise 5 stack --------------------------

compile :: String -> Maybe Program
