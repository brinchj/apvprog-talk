#include "Common/Interpreter.hs"


type M a   =  (a -> Answer) -> Answer

unitM a      = \c -> c a
m `bindM` k  =  \c -> m (\a -> k a c)

--

type Answer = Value
showM m     = showval (m id)

#ifndef SKIPMAIN
main :: IO ()
main = print (test term0)
#endif
