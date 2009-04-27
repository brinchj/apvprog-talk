mylookup x []  = errorM ("unbound variable: " ++ x)
myadd a b      = errorM ("should be numbers: " ++ showval a ++ "," ++ showval b)
myapply f a    = errorM ("should be function: " ++ showval f)
#define LOOKUP_ERR lookup x [] = mylookup x []
#define ADD_ERR add a b = myadd a b
#define APPLY_ERR apply f a = myapply f a
#include "Common/Interpreter.hs"

data E a  =  Success a | Error String

unitE a   =  Success a
errorE s  =  Error s

(Success a) `bindE` k  =  k a
(Error s)   `bindE` k  =  Error s

showE (Success a)  =  "Success: " ++ showval a
showE (Error s)    =  "Error: " ++ s

--

#ifndef SKIP_CH23_TEST

type M  =  E
unitM   =  unitE
errorM  =  errorE
bindM   =  bindE
showM   =  showE

--

term1 = (App (Con 1) (Con 2))
term2 = (Var "x")
term3 = (Add (Con 1) (Lam "x" (Con 3)))

main :: IO ()
divstr = " ----- "
main = print (test term0 ++ divstr ++ test term1 ++ divstr ++ test term2 ++ divstr ++ test term3)

#endif
