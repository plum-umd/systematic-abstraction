
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



type Σ = (Exp,Store,Kont)
data Storable = Clo Lambda | Cont Kont
 deriving (Eq,Ord)
type Store = Addr :-> ℙ(Storable)
data Kont = Mt | Ar(Exp,Addr) | Fn(Lambda,Addr)
 deriving (Eq,Ord)
data Addr = KAddr Exp | BAddr Var
 deriving (Eq,Ord)


inject :: Exp -> Σ
inject (e) = (e, σ0, κ0)
 where σ0 = Data.Map.empty 
       κ0 = Mt


step :: Σ -> [Σ]
step (Ref x, σ, κ) =
     [ (Lam lam, σ, κ) 
     | Clo(lam) <- Data.Set.toList $! σ!!(BAddr x) ]

step (f :@ e, σ, κ) = [ (f, σ', Ar(e,a')) ] 
 where σ' = σ ⨆ [a' ==> s(Cont κ)] 
       a' = KAddr (f :@ e)

step (Lam lam, σ, Ar(e, a')) = [ (e, σ, Fn(lam, a')) ]

step (Lam lam, σ, Fn(x :=> e, a)) 
   = [ (e, σ ⨆ [BAddr x ==> s(Clo(lam))], κ) 
     | Cont κ <- Data.Set.toList $! σ!!a ]
  


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
