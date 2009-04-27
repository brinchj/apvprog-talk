myinterp (Out u) e  = interp u e `bindM` (\a  ->
                      outM a     `bindM` (\() ->
                      unitM a))
#define MORETERM | Out Term
#define MOREINTERP interp (Out u) e = myinterp (Out u) e

#include "Common/Interpreter.hs"

type M a      =  (String, a)

unitM a       =  ("", a)
m `bindM` k   =  let (r,a) = m; (s,b) = k a in (r++s, b)
showM (s, a)  =  "Output: " ++ s ++ " Value: " ++ showval a

outM          :: Value -> M ()
outM a        =  (showval a ++ "; ", ())

--

term1 = (Add (Out (Con 41)) (Out (Con 1)))

main :: IO ()
main = print (test term0 ++ " --- " ++ test term1)
