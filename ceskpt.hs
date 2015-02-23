
 -- CESKpt: A time-stamped CESK* machine in which
 --         continuations contain pointers
 --         to the subsequent continuations.

import Data.Map hiding (foldl)
import Syntax
import Reachability
import MapUpdate



type Σ = (Exp,Env,Store,Kont,Time)
data Storable = Clo (Lambda, Env) | Cont Kont
type Env = Var :-> Addr
type Store = Addr :-> Storable
data Kont = Mt | Ar (Exp,Env,Addr) | Fn (Lambda,Env,Addr)
type Addr = Int
type Time = Int


inject :: Exp -> Σ
inject (e) = (e, ρ0, σ0, κ0, t0)
 where ρ0 = Data.Map.empty
       σ0 = Data.Map.empty 
       κ0 = Mt
       t0 = 0

isFinal :: Σ -> Bool
isFinal (Lam _, _, σ, Mt, _) = True
isFinal _ = False

evaluate :: Exp -> Exp
evaluate e = case terminal step isFinal (inject(e)) of 
 (e', _, _, _, _) -> e'


step :: Σ -> Σ
step ς@(Ref x, ρ, σ, κ, t) = (Lam lam, ρ', σ, κ, t')  
 where Clo(lam, ρ') = σ!(ρ!x)
       t' = tick(ς)

step ς@(f :@ e, ρ, σ, κ, t) = (f, ρ, σ', κ', t')
 where a' = alloc(ς)
       σ' = σ // [a' ==> Cont κ]
       κ' = Ar(e, ρ, a')
       t' = tick(ς)

step ς@(Lam lam, ρ, σ, Ar(e, ρ', a'), t) 
     = (e, ρ', σ, Fn(lam, ρ, a'), t') 
 where t' = tick(ς) 

step ς@(Lam lam, ρ, σ, Fn(x :=> e, ρ', a), t) 
     = (e, ρ' // [x ==> a'], σ // [a' ==> Clo(lam, ρ)], κ, t') 
 where Cont κ = σ!a
       a' = alloc(ς)
       t' = tick(ς)
  

alloc :: Σ -> Addr
alloc (_,_,σ,_,_) = (foldl max 0 $! keys σ) + 1

tick :: Σ -> Time
tick (_,_,_,_,t) = t + 1


main :: IO ()
main = do 
  return ()
