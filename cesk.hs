import Data.Map
import Syntax
import Reachability
import MapUpdate


type Σ = (Exp,Env,Store,Kont)
type Env = Var :-> Addr
data Storable = Clo (Lambda, Env)
type Store = Addr :-> Storable
data Kont = Mt | Ar (Exp,Env,Kont) | Fn (Lambda,Env,Kont)
type Addr = Int


inject :: Exp -> Σ 
inject (e) = (e, ρ0, σ0, Mt) 
 where ρ0 = Data.Map.empty
       σ0 = Data.Map.empty


isFinal :: Σ -> Bool
isFinal (Lam _, _, _, Mt) = True
isFinal _                 = False

evaluate :: Exp -> Exp
evaluate e = case terminal step isFinal (inject(e)) of 
 (e', _, _, _) -> e'


step :: Σ -> Σ 
step (Ref x, ρ, σ, κ) = (Lam lam, ρ', σ, κ)
  where Clo (lam, ρ') = σ!(ρ!x)
step (f :@ e, ρ, σ, κ) = (f, ρ, σ,  Ar(e, ρ, κ))
step (Lam lam,ρ,σ,Ar(e, ρ', κ)) = (e, ρ', σ, Fn(lam, ρ, κ))
step (Lam lam,ρ,σ,Fn(x :=> e, ρ', κ)) =
 (e, ρ' // [x ==> a'], σ // [a' ==> Clo (lam, ρ)], κ) 
   where a' = alloc(σ)


alloc :: Store -> Addr
alloc(σ) = (foldl max 0 $! keys σ) + 1



main :: IO ()
main = do 
  return ()
