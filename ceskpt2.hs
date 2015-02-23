
 -- CESKpT2: A time-stamped CESK* machine in which
 --          continuations contain pointers
 --          to the subsequent continuations.
 --          Addresses are built from times.

import Data.Map
import Syntax
import Reachability
import MapUpdate



type Σ = (Exp,Env,Store,Kont,Time)
data Storable = Clo (Lambda, Env) | Cont Kont
type Env = Var :-> Addr
type Store = Addr :-> Storable
data Kont = Mt | Ar (Exp,Env,Addr) | Fn (Lambda,Env,Addr)
type Time = Int
data Addr = KAddr (Exp, Time)
          | BAddr (Var, Time)
 deriving (Eq,Ord)



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
 where a' = allocKont(f :@ e, t')
       σ' = σ // [a' ==> Cont κ]
       κ' = Ar(e, ρ, a')
       t' = tick(ς)

step ς@(Lam lam, ρ, σ, Ar(e, ρ', a'), t) 
     = (e, ρ', σ, Fn(lam, ρ, a'), t') 
 where t' = tick(ς) 

step ς@(Lam lam, ρ, σ, Fn(x :=> e, ρ', a), t) 
     = (e, ρ' // [x ==> a'], σ // [a' ==> Clo(lam, ρ)], κ, t') 
 where Cont κ = σ!a
       a' = allocBind(x, t')
       t' = tick(ς)
  

allocBind :: (Var,Time) -> Addr
allocBind (v,t) = BAddr (v,t)

allocKont :: (Exp,Time) -> Addr
allocKont (e,t) = KAddr (e,t)

tick :: Σ -> Time
tick (_,_,_,_,t) = t + 1


main :: IO ()
main = do 
  return ()
