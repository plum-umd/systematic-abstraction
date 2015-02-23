
 -- ACESKpTL: A k-CFA-like abstract machine

import Prelude hiding ((!!))

import Data.Set
import Data.Map
import Syntax
import Reachability
import MapUpdate

type ℙ s = Data.Set.Set s

s x = Data.Set.singleton x

class Lattice a where
 bot :: a
 top :: a
 (⊑) :: a -> a -> Bool
 (⊔) :: a -> a -> a
 (⊓) :: a -> a -> a


instance (Ord s, Eq s) => Lattice (ℙ s) where
 bot = Data.Set.empty
 top = error "no representation of universal set"
 x ⊔ y = x `Data.Set.union` y
 x ⊓ y = x `Data.Set.intersection` y
 x ⊑ y = x `Data.Set.isSubsetOf` y

instance (Ord k, Lattice v) => Lattice (k :-> v) where
 bot = Data.Map.empty
 top = error "no representation of top map"
 f ⊑ g = Data.Map.isSubmapOfBy (⊑) f g
 f ⊔ g = Data.Map.unionWith (⊔) f g
 f ⊓ g = Data.Map.intersectionWith (⊓) f g

(⨆) :: (Ord k, Lattice v) => (k :-> v) -> [(k,v)] -> (k :-> v)
f ⨆ [(k,v)] = Data.Map.insertWith (⊔) k v f

(!!) :: (Ord k, Lattice v) => (k :-> v) -> k -> v
f !! k = Data.Map.findWithDefault bot k f


k = 1


type Σ = (Exp,Env,Store,Kont,Time)
data Storable = Clo(Lambda, Env) | Cont Kont
 deriving (Eq,Ord)
type Env = Var :-> Addr
type Store = Addr :-> ℙ(Storable)
data Kont = Mt | Ar(Exp,Env,Addr) | Fn(Lambda,Env,Addr)
 deriving (Eq,Ord)
type Time = [Exp]
data Addr = KAddr (Exp, Time)
          | BAddr (Var, Time)
 deriving (Eq,Ord)


inject :: Exp -> Σ
inject (e) = (e, ρ0, σ0, κ0, t0)
 where ρ0 = Data.Map.empty
       σ0 = Data.Map.empty 
       κ0 = Mt
       t0 = []


step :: Σ -> [Σ]
step ς@(Ref x, ρ, σ, κ, t) = [ (Lam lam, ρ', σ, κ, t') 
  | Clo(lam, ρ') <- Data.Set.toList $! σ!!(ρ!x) ]
  where t' = tick(ς)

step ς@(f :@ e, ρ, σ, κ, t) = [ (f, ρ, σ', κ', t') ] 
 where a' = allocKont(f :@ e, t')
       σ' = σ ⨆ [a' ==> s(Cont κ)] 
       κ' = Ar(e, ρ, a')
       t' = tick(ς)

step ς@(Lam lam, ρ, σ, Ar(e, ρ', a'), t) 
     = [ (e, ρ', σ, Fn(lam, ρ, a'), t') ]
 where t' = tick(ς) 

step ς@(Lam lam, ρ, σ, Fn(x :=> e, ρ', a), t) 
     = [ (e, ρ' // [x ==> a'], σ ⨆ [a' ==> s(Clo(lam, ρ))], κ, t') 
       | Cont κ <- Data.Set.toList $! σ!!a ]
 where t' = tick(ς) 
       a' = allocBind(x, t') 
  

allocBind :: (Var,Time) -> Addr
allocBind (v,t) = BAddr (v,t)

allocKont :: (Exp,Time) -> Addr
allocKont (e,t) = KAddr (e,t)

tick :: Σ -> Time
tick (e,_,_,_,t) = take k (e : t)



explore :: (Ord a) => (a -> [a]) -> a -> ℙ(a)
explore f ς0 = search f Data.Set.empty [ς0]

(∈) :: Ord a => a -> ℙ(a) -> Bool
(∈) = Data.Set.member

search :: (Ord a) => (a -> [a]) -> ℙ(a) -> [a] -> ℙ(a)
search f seen [] = seen
search f seen (hd:tl)
 | hd ∈ seen = search f seen tl
 | otherwise = search f (Data.Set.insert hd seen) (f(hd) ++ tl)

aval :: Exp -> ℙ(Σ)
aval(e) = explore step (inject(e))




main :: IO ()
main = do 
  return ()
