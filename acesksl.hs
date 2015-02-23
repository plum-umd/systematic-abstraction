-- BUG: Add join!!!

import Data.Map as Map
import Data.Set as Pow
import Syntax


infixr 9 ==>
infixr 9 // 

(==>) :: a -> b -> (a,b)
(==>) x y = (x,y)

(//) :: Ord a => (Map a b) -> [(a,b)] -> (Map a b)
(//) f [(x,y)] = Map.insert x y f


type Time = [Exp]

data Addr = ArAddr Exp Time
          | FnAddr Exp Time
          | MtAddr
          | BAddr Var Time
 deriving (Eq,Ord,Show)

data Val = Clo  Lambda Env
         | Cont Kont
 deriving (Eq,Ord,Show)

type Env = Map Var Addr

type Store = Map Addr (Pow.Set Val)

data Kont = Mt
          | Ar (Exp,Env,Addr)
          | Fn (Lambda,Env,Addr)
 deriving (Eq,Ord,Show)


isArAddr(ArAddr _ _) = True
isArAddr(_) = False

type State = (Exp,Env,Store,Addr,Time)

k = 1

ρ0 :: Env
ρ0 = Map.empty

σ0 :: Store
σ0 = Map.empty // [MtAddr ==> Pow.singleton(Cont(Mt))]

initial :: Exp -> State
initial e = (e, ρ0, σ0, MtAddr, [])


tick :: State -> Time
tick (e,_,_,_,t) = take k (e : t)

allocArKont :: Exp -> Time -> Addr
allocArKont e t = ArAddr e t

allocFnKont :: Exp -> Time -> Addr
allocFnKont e t = FnAddr e t

allocBind :: Var -> Time -> Addr
allocBind v t = BAddr v t 


fetch :: Var -> Env -> Store -> [Val]
fetch x ρ σ = Pow.toList $ σ!(ρ!x)


step :: State -> [State]
step ς@(Ref x, ρ, σ, a, t) = 
  [ (Lam lam, ρ', σ, a, t') | 
    Clo lam ρ' <- fetch x ρ σ ]
   where t' = tick ς 

step ς@(f :@ e, ρ, σ, a, t) = [ (f, ρ, σ', a', t') ]
 where a' = allocArKont f t'
       σ' = σ // [a' ==> Pow.singleton(Cont(Ar(e, ρ, a)))]
       t' = tick ς 

step ς@(Lam lam, ρ, σ, a, t) | isArAddr(a) = 
 [ (e, ρ', σ', a'', t') |
   Cont(Ar(e, ρ', a')) <- Pow.toList $! σ!a,
   let a'' = allocFnKont e t', 
   let σ'  = σ // [a'' ==> Pow.singleton(Cont(Fn(lam, ρ, a')))] ]
 where t' = tick ς 

step ς@(Lam lam, ρ, σ, a, t) =
 [ (e, ρ' // [x ==> a'], σ // [a' ==> Pow.singleton(Clo lam ρ)], a'', t') |
   Cont(Fn(x :=> e, ρ', a'')) <- Pow.toList $! σ!a,
   let a' = allocBind x t' ]
 where t' = tick ς


explore :: (Ord a, Eq a) => (a -> [a]) -> a -> Pow.Set a
explore f a = search f Pow.empty [a]

isin :: Ord a => a -> Pow.Set a -> Bool
isin = Pow.member

search :: (Ord a, Eq a) => (a -> [a]) -> Pow.Set a -> [a] -> Pow.Set a
search f seen [] = seen
search f seen (hd:tl) | hd `isin` seen = search f seen tl
                      | otherwise      = search f (Pow.insert hd seen) (f(hd) ++ tl)

analyze :: Exp -> Pow.Set State
analyze e = explore step (initial(e))

printStates :: [State] -> IO ()
printStates [] = return ()
printStates (hd:tl) =
 do print hd
    putStr "\n"
    printStates tl

main :: IO ()
main = do 

  printStates $! Pow.toList (analyze $! ((Lam ("x" :=> Ref "x")) 
                      :@ (Lam ("z" :=> Ref "z")))
                    :@ (Lam ("a" :=> Ref "a")))


  return ()
