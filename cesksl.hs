import Data.Map
import Syntax

infixr 9 ==>
infixr 9 // 

(==>) :: a -> b -> (a,b)
(==>) x y = (x,y)

(//) :: Ord a => (Map a b) -> [(a,b)] -> (Map a b)
(//) f [(x,y)] = Data.Map.insert x y f


type Time = [Exp]

data Addr = KAddr Exp Time
          | BAddr Var Time
 deriving (Eq,Ord)

data Val = Clo  Lambda Env
         | Cont Kont

type Env = Map Var Addr

type Store = Map Addr Val

data Kont = Mt
          | Ar (Exp,Env,Addr)
          | Fn (Lambda,Env,Addr)

isAr(Cont (Ar _)) = True
isAr(_) = False

isFn(Cont(Fn _)) = True
isFn(_) = False

type State = (Exp,Env,Store,Addr,Time)


tick :: State -> Time
tick (e,_,_,_,t) = e : t

allocKont :: Exp -> Time -> Addr
allocKont e t = KAddr e t

allocBind :: Var -> Time -> Addr
allocBind v t = BAddr v t 


fetch :: Var -> Env -> Store -> Val
fetch x ρ σ = σ!(ρ!x)


step :: State -> State
step ς@(Ref x, ρ, σ, a, t) = (Lam lam, ρ', σ, a, t')  
 where Clo lam ρ' = fetch x ρ σ 
       t' = tick ς 

step ς@(f :@ e, ρ, σ, a, t) = (f, ρ, σ', a', t')
 where a' = allocKont f t'
       σ' = σ // [a' ==> Cont(Ar(e, ρ, a))]
       t' = tick ς 

step ς@(Lam lam, ρ, σ, a, t) | isAr(σ!a) = (e, ρ', σ', a'', t') 
 where Cont(Ar(e, ρ', a')) = σ!a
       a'' = allocKont e t'
       σ' = σ // [a'' ==> Cont(Fn(lam, ρ, a'))]
       t' = tick ς 

step ς@(Lam lam, ρ, σ, a, t) = (e, ρ' // [x ==> a'], σ // [a' ==> Clo lam ρ], a'', t') 
 where Cont(Fn(x :=> e, ρ', a'')) = σ!a
       a' = allocBind x t'
       t' = tick ς 
  


main :: IO ()
main = do 
  return ()
