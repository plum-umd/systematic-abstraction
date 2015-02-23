
 -- CESKp: A CESK* machine in which continuations contain pointers
 --        to the subsequent continuations.

import Data.Map
import Syntax
import Reachability
import MapUpdate



type Σ = (Exp,Env,Store,Kont)
data Storable = Clo (Lambda, Env) | Cont Kont
type Env = Var :-> Addr
type Store = Addr :-> Storable
data Kont = Mt | Ar (Exp,Env,Addr) | Fn (Lambda,Env,Addr)
type Addr = Int


inject :: Exp -> Σ
inject (e) = (e, ρ0, σ0, κ0)
 where ρ0 = Data.Map.empty
       σ0 = Data.Map.empty 
       κ0 = Mt

isFinal :: Σ -> Bool
isFinal (Lam _, _, σ, Mt) = True
isFinal _ = False

evaluate :: Exp -> Exp
evaluate e = case terminal step isFinal (inject(e)) of 
 (e', _, _, _) -> e'


step :: Σ -> Σ
step (Ref x, ρ, σ, κ) = (Lam lam, ρ', σ, κ)  
 where Clo(lam, ρ') = σ!(ρ!x)

step (f :@ e, ρ, σ, κ) = (f, ρ, σ', κ')
 where a' = alloc(σ)
       σ' = σ // [a' ==> Cont κ]
       κ' = Ar(e, ρ, a')

step (Lam lam, ρ, σ, Ar(e, ρ', a')) = (e, ρ', σ, Fn(lam, ρ, a')) 

step (Lam lam, ρ, σ, Fn(x :=> e, ρ', a)) =
     (e, ρ' // [x ==> a'], σ // [a' ==> Clo(lam, ρ)], κ) 
 where Cont κ = σ!a
       a' = alloc(σ)
  

alloc :: Store -> Addr
alloc(σ) = (foldl max 0 $! keys σ) + 1


main :: IO ()
main = do 
  return ()
