#include "Common/Interpreter.hs"

type  M a    =  a
unitM a      =  a
a `bindM` k  =  k a
showM a      =  showval a

main :: IO ()
main = print (test term0)
