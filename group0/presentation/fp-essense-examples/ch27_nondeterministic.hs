#define MORETERM | Fail | Amb Term Term
#define MOREINTERP interp Fail e = zeroM; interp (Amb u v) e = interp u e `plusM` interp v e

#ifdef BYNAME
#include "Common/ByName.hs"
#else
#include "Common/Interpreter.hs"
#endif

--

type M a     =  [a]

unitM a      =  [a]
m `bindM` k  =  [ b | a <-m, b <- k a ]

zeroM        =  []
l `plusM` m  =  l ++ m

showM m      =  showlist [ showval a | a <- m]

--

showlist l = show l

term1 = (App (Lam "x" (Add (Var "x") (Var "x"))) (Amb (Con 1) (Con 2)))

main :: IO ()
main = print (test term0 ++ " --- " ++ test term1)
