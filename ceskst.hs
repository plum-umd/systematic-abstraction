import Data.Map
import Syntax
import Reachability
import MapUpdate


type State = (Exp,Env,Store,Addr,Time)
data Val = Clo (Lambda, Env)
         | Cont Kont
type Env = Var :-> Addr
type Store = Addr :-> Val
data Kont = Mt
          | Ar (Exp,Env,Addr)
          | Fn (Lambda,Env,Addr)
type Time = Int
data Addr = KAddr (Exp, Time)
          | BAddr (Var, Time)
          | MtAddr
 deriving (Eq,Ord)


isAr :: Val -> Bool
isAr (Cont (Ar _)) = True
isAr (_) = False

isFn :: Val -> Bool
isFn (Cont(Fn _)) = True
isFn (_) = False

isMt :: Val -> Bool
isMt (Cont(Mt)) = True
isMt (_) = False


inject :: Exp -> State
inject (e) = (e, ρ0, σ0, MtAddr, t0)
 where ρ0 = Data.Map.empty
       σ0 = Data.Map.empty // [MtAddr ==> Cont(Mt)]
       t0 = 0

isFinal :: State -> Bool
isFinal (Lam _, _, _, MtAddr, _) = True
isFinal _ = False

evaluate :: Exp -> Exp
evaluate e = case terminal step isFinal (inject(e)) of 
 (e', _, _, _, _) -> e'



tick :: State -> Time
tick (_,_,_,_,t) = t + 1

allocKont :: (Exp, Time) -> Addr
allocKont (e,t) = KAddr (e, t)

allocBind :: (Var, Time) -> Addr
allocBind (v,t) = BAddr (v, t)


step :: State -> State
step ς@(Ref x, ρ, σ, a, t) = (Lam lam, ρ', σ, a, t')  
 where Clo (lam, ρ') = σ!(ρ!x)
       t' = tick(ς)

step ς@(f :@ e, ρ, σ, a, t) = (f, ρ, σ', a', t')
 where a' = allocKont(f, t')
       σ' = σ // [a' ==> Cont(Ar(e, ρ, a))]
       t' = tick(ς)

step ς@(Lam lam, ρ, σ, a, t) | isAr(σ!a) = (e, ρ', σ', a'', t') 
 where Cont(Ar(e, ρ', a')) = σ!a
       a'' = allocKont(e, t')
       σ' = σ // [a'' ==> Cont(Fn(lam, ρ, a'))]
       t' = tick(ς)

step ς@(Lam lam, ρ, σ, a, t) | isFn(σ!a) =
       (e, ρ' // [x ==> a'], σ // [a' ==> Clo(lam, ρ)], a'', t') 
 where Cont(Fn(x :=> e, ρ', a'')) = σ!a
       a' = allocBind(x, t')
       t' = tick(ς) 
  


main :: IO ()
main = do 
  return ()
