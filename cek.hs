import Syntax
import Reachability


infixr 9 ==>
infixr 9 // 

(==>) :: a -> b -> (a,b)
(==>) x y = (x,y)


(//) :: Eq a => (a -> b) -> [(a,b)] -> (a -> b)
(//) f [(x,y)] = \ x' ->
                 if (x == x') 
                 then y
                 else f(x')


type Σ    = (Exp,Env,Kont)
data D    = Clo (Lambda, Env)
type Env  = Var -> D
data Kont = Mt
          | Ar (Exp,Env,Kont)
          | Fn (Lambda,Env,Kont)


inject :: Exp -> Σ
inject (e) = (e, ρ0, Mt)
 where ρ0 :: Env
       ρ0 = \ x -> error $ "no binding for " ++ x
 

isFinal :: Σ -> Bool
isFinal (Lam _, ρ, Mt) = True
isFinal _              = False

evaluate :: Exp -> Exp
evaluate e = case terminal step isFinal (inject(e)) of 
 (e', _, _) -> e'

step :: Σ -> Σ
step (Ref x, ρ, κ)                    = (Lam lam,ρ',κ) where Clo (lam, ρ') = ρ(x)
step (f :@ e, ρ, κ)                   = (f, ρ,  Ar(e, ρ, κ))
step (Lam lam, ρ, Ar(e, ρ', κ))       = (e, ρ', Fn(lam, ρ, κ))
step (Lam lam, ρ, Fn(x :=> e, ρ', κ)) = (e, ρ' // [x ==> Clo (lam, ρ)], κ)


main :: IO ()
main = do 
  return ()
