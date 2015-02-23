import Data.Map hiding (foldl)
import Syntax
import Reachability
import MapUpdate



type Σ = (Exp,Env,Store,Addr)
data Storable = Clo (Lambda, Env) | Cont Kont
type Env = Var :-> Addr
type Store = Addr :-> Storable
data Kont = Mt | Ar (Exp,Env,Addr) | Fn (Lambda,Env,Addr)
type Addr = Int



isAr :: Storable -> Bool
isAr (Cont (Ar _)) = True
isAr (_) = False

isFn :: Storable -> Bool
isFn (Cont(Fn _)) = True
isFn (_) = False

isMt :: Storable -> Bool
isMt (Cont(Mt)) = True
isMt (_) = False



inject :: Exp -> Σ
inject (e) = (e, ρ0, σ0, ahalt)
 where ρ0 = Data.Map.empty
       σ0 = Data.Map.empty // [ahalt ==> Cont(Mt)]
       ahalt = 0

isFinal :: Σ -> Bool
isFinal (Lam _, _, σ, a) = isMt(σ!a)

evaluate :: Exp -> Exp
evaluate e = case terminal step isFinal (inject(e)) of 
 (e', _, _, _) -> e'




step :: Σ -> Σ
step (Ref x, ρ, σ, a) = (Lam lam, ρ', σ, a)  
 where Clo(lam, ρ') = σ!(ρ!x)

step (f :@ e, ρ, σ, a) = (f, ρ, σ', a')
 where a' = alloc(σ)
       σ' = σ // [a' ==> Cont(Ar(e, ρ, a))]

step (Lam lam, ρ, σ, a) | isAr(σ!a) = (e, ρ', σ', a'') 
 where Cont(Ar(e, ρ', a')) = σ!a
       a'' = alloc(σ)
       σ' = σ // [a'' ==> Cont(Fn(lam, ρ, a'))]

step (Lam lam, ρ, σ, a) | isFn(σ!a) =
     (e, ρ' // [x ==> a'], σ // [a' ==> Clo(lam, ρ)], a'') 
 where Cont(Fn(x :=> e, ρ', a'')) = σ!a
       a' = alloc(σ)
  

alloc :: Store -> Addr
alloc(σ) = (foldl max 0 $! keys σ) + 1


main :: IO ()
main = do 
  return ()
