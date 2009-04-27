callccM      :: ((a -> M b) -> M a) -> M a
callccM h  = \c -> let k a = \_ -> c a
                   in  h k c

#define MORETERM | Callcc Name Term
#define MOREINTERP interp (Callcc x v) e = callccM (\k -> interp v ((x, Fun k):e))

#define SKIPMAIN
#include "ch31_cps.hs"

--

term1 = (Add (Con 1) 
             (Callcc "k" (Add (Con 20) 
                              (App (Var "k") 
                                   (Con 4)))))

main :: IO ()
main = print (test term0 ++ " --- " ++ test term1)
