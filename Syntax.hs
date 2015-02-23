module Syntax where

type Var = String

data Lambda = Var :=> Exp
 deriving (Show,Eq,Ord)

data Exp = Ref Var
         | Lam Lambda
         | Exp :@ Exp
 deriving (Show,Eq,Ord)



