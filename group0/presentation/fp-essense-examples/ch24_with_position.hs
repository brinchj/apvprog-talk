#define MORETERM | At Position Term
#define MOREINTERP interp (At p t) e = resetM p (interp t e)

#define SKIP_CH23_TEST
#include "ch23_error_messages.hs"

type Position = Int
showpos p     = show p
pos0          = 0

type M a     =  Position -> E a

unitM a      =  \p -> unitE a
errorM s     =  \p -> errorE (showpos p ++ ": " ++ s)

m `bindM` k  =  \p -> m p `bindE` (\x -> k x p)

showM m      =  showE (m pos0)

resetM       :: Position -> M x -> M x
resetM q m   =  \p -> m q

--

term1 = (At 5 (App (At 1 (Con 1)) (At 12 (Con 2))))
term2 = (At 100 (At 29 (Var "x")))
term3 = (At 200 (Add (Con 1) (At 22 (Lam "x" (Con 3)))))

main :: IO ()
divstr = " ----- "
main = print (test term0 ++ divstr ++ test term1 ++ divstr ++ test term2 ++ divstr ++ test term3)
