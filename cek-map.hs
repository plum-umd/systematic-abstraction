import Data.Map
import Syntax
import Reachability
import MapUpdate


type Σ    = (Exp,Env,Kont)
data D    = Clo (Lambda, Env)
type Env  = Var :-> D
data Kont = Mt
          | Ar (Exp,Env,Kont)
          | Fn (Lambda,Env,Kont)


ρ0 :: Env
ρ0 = Data.Map.empty

inject :: Exp -> Σ
inject (e) = (e, ρ0, Mt)

isFinal :: Σ -> Bool
isFinal (Lam _, ρ, Mt) = True
isFinal _              = False

evaluate' :: Exp -> Exp
evaluate' e = case terminal step isFinal (inject(e)) of 
 (e', _, _) -> e'

evaluate :: Exp -> [Σ]
evaluate e = collect step isFinal (inject(e)) 

step :: Σ -> Σ
step (Ref x, ρ, κ)                    = (Lam lam,ρ',κ) where Clo (lam, ρ') = ρ!x
step (f :@ e, ρ, κ)                   = (f, ρ,  Ar(e, ρ, κ))
step (Lam lam, ρ, Ar(e, ρ', κ))       = (e, ρ', Fn(lam, ρ, κ))
step (Lam lam, ρ, Fn(x :=> e, ρ', κ)) = (e, ρ' // [x ==> Clo (lam, ρ)], κ)



main :: IO ()
main = do 
  return ()
