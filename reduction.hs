import Syntax

-- infixr 0 :=>


c :: Exp -> Exp
c e@(Ref _)          = e
c e@(Lam _)          = e
c e@(Lam _ :@ Lam _) = e
 --
c (Lam _ :@ e) = c(e)
c (e :@ _)     = c(e)


s :: Exp -> Exp -> Exp
s e@(Ref _)          val = val
s e@(Lam _)          val = val
s e@(Lam _ :@ Lam _) val = val
 -- 
s (l@(Lam _) :@ e) val = l :@ (s e val)
s (e :@ e') val        = (s e val) :@ e'


sub :: Var -> Exp -> Exp -> Exp
sub v val (Ref v') | v == v'   = val
                   | otherwise = (Ref v')
sub v val (Lam (v' :=> body)) | v == v'   = Lam (v' :=> body)
                              | otherwise = Lam (v' :=> (sub v val body))
sub v val (f :@ e) = (sub v val f) :@ (sub v val e)


reduce :: Exp -> Exp
reduce e@(c -> (Ref v))   = e
reduce e@(c -> (Lam lam)) = e
reduce e@(c -> (Lam (v :=> body) :@ val)) = s e (sub v val body)


eval :: Exp -> Exp
eval (Ref v)   = Ref v
eval (Lam lam) = Lam lam
eval (f :@ e)  = eval(reduce(f :@ e))


main :: IO ()
main = do 

  print (eval $! ((Lam ("x" :=> Ref "x")) 
                      :@ (Lam ("z" :=> Ref "z")))
                    :@ (Lam ("a" :=> Ref "a")))

  return ()
