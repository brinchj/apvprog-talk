showint i = show i

type Name            =  String

#ifndef MORETERM
#define MORETERM
#endif
data Term            =  Var Name
                     |  Con Int
                     |  Add Term Term
                     |  Lam Name Term
                     |  App Term Term
                     MORETERM

data Value           =  Wrong
                     |  Num Int
                     |  Fun (M Value -> M Value)

type Environment     =  [(Name, M Value)]

showval              :: Value -> String
showval Wrong        =  "<wrong>"
showval (Num i)      =  showint i
showval (Fun f)      =  "<function>"

#ifndef MOREINTERP
#define MOREINTERP
#endif
interp               :: Term -> Environment -> M Value
interp (Var x) e     =  Main.lookup x e
interp (Con i) e     =  unitM (Num i)
interp (Add u v) e   =  interp u e `bindM` (\a ->
                        interp v e `bindM` (\b ->
                        add a b))
interp (Lam x v) e   =  unitM (Fun (\a -> interp v ((x,a):e)))
interp (App t u) e   =  interp t e `bindM` (\f ->
                        apply f (interp u e))
MOREINTERP

lookup               :: Name -> Environment -> M Value
lookup x []          =  unitM Wrong
lookup x ((y,n):e)   =  if x==y then n else Main.lookup x e

add                  :: Value -> Value -> M Value
#ifdef ADD
ADD
#else
add (Num i) (Num j)  =  unitM (Num (i+j))
#endif
add a b              =  unitM Wrong

apply                :: Value -> M Value -> M Value
#ifdef APPLY
APPLY
#else
apply (Fun h) m      =  h m
#endif
apply f m            =  unitM Wrong

test                 :: Term -> String
test t               =  showM (interp t [])

term0 = (App (Lam "x" (Add (Var "x") (Var "x")))
             (Add (Con 10) (Con 11)))
