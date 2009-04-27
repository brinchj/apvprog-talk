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
                     |  Fun (Value -> M Value)

type Environment     =  [(Name, Value)]

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
                        interp u e `bindM` (\a ->
                        apply f a))
MOREINTERP

lookup               :: Name -> Environment -> M Value
#ifdef LOOKUP_ERR
LOOKUP_ERR
#else
lookup x []          =  unitM Wrong
#endif
lookup x ((y,b):e)   =  if x==y then unitM b else Main.lookup x e

add                  :: Value -> Value -> M Value
#ifdef ADD
ADD
#else
add (Num i) (Num j)  =  unitM (Num (i+j))
#endif
#ifdef ADD_ERR
ADD_ERR
#else
add a b              =  unitM Wrong
#endif

apply                :: Value -> Value -> M Value
#ifdef APPLY
APPLY
#else
apply (Fun k) a      =  k a
#endif
#ifdef APPLY_ERR
APPLY_ERR
#else
apply f a            =  unitM Wrong
#endif

test                 :: Term -> String
test t               =  showM (interp t [])

term0 = (App (Lam "x" (Add (Var "x") (Var "x")))
             (Add (Con 10) (Con 11)))
