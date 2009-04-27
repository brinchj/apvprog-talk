myapply (Fun k) a     = tickM `bindM` (\() -> k a)
myadd (Num i) (Num j) = tickM `bindM` (\() -> unitM (Num (i+j)))
#define ADD add (Num i) (Num j) = myadd (Num i) (Num j)
#define APPLY apply (Fun k) a = myapply (Fun k) a

#define MORETERM | Count
#define MOREINTERP interp Count e = fetchM `bindM` (\i -> unitM (Num i))

#ifdef BYNAME
#include "Common/ByName.hs"
#else
#include "Common/Interpreter.hs"
#endif

--

type M a   = State -> (a, State)

unitM a      =  \s0 -> (a, s0)
#ifndef BINDM
m `bindM` k  =  \s0 -> let (a, s1) = m s0
                           (b, s2) = k a s1
                       in  (b, s2)
#endif

--

type State = Int
showM m = let (a, s1) = m 0
          in  "Value: " ++ showval a ++ "; " ++ "Count: " ++ showint s1

tickM   :: M ()
tickM   =  \s -> ((), s+1)

fetchM  :: M State
fetchM  =  \s -> (s, s)

--

term1 = (Add (Add (Con 1) (Con 2)) Count)

main :: IO ()
main = print (test term0 ++ " --- " ++ test term1)
