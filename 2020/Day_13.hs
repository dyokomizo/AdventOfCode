module Day13 where
  import qualified Data.Matrix as M
  import Data.Matrix ((!))
  import qualified Data.Ord as O
  import qualified Data.List as L
  import Control.Arrow

  data Bus = B Int | X
    deriving (Eq, Ord, Show)

  departures = enumFromThen 0

  earliest (t,bs) = L.minimumBy (O.comparing snd) $ map ((id *** flip (-) t) . (id *** head) . (id *** filter (>=t)) . (id &&& departures)) $ bs

  ids (t,bs) = (t,[i | B i <- bs])

  equations = go 0 ['a','b'..'z']
    where go n (v:vs) (B i:bs) = equation v n i : go (n+1) vs bs  
          go n    vs  (X  :bs) =                  go (n+1) vs bs
          go _  _      []      =                  []
          equation v n i = show i ++ [v] ++ "-" ++ show n ++ "=t"

  solve bs = case M.inverse (matrix bs) of
               Right m -> M.multStd m (values bs)

  matrix = M.fromLists . pad . go 0
    where go n (B i:bs) = row n i : go (n+1) bs  
          go n (X  :bs) =           go  n    bs
          go _      []  =           []
          row n i = zeroes n ++ [toRational i]
          zeroes n = take n $ repeat (0 :: Rational)

  pad xss = map p xss
    where l = maximum $ map length xss
          p xs = take l $ xs ++ repeat (0 :: Rational)

  values = M.fromLists . go 0
    where go n (B i:bs) = [toRational n] : go (n+1) bs  
          go n (X  :bs) =                  go  n    bs
          go _      []  =                  []

  xs = (1006605, [B 19, X, X, X, X, X, X, X, X, X, X, X, X, B 37, X, X, X, X, X, B 883, X, X, X, X, X, X, X, B 23,X, X, X, X, B 13,X, X, X, B 17,X, X, X, X, X, X, X, X, X, X, X, X, X, B 797, X, X, X, X, X, X, X, X, X, B 41, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, B 29])
{-
       ["19a-0=t","37b-13=t","883c-19=t","23d-27=t","13e-32=t","17f-36=t","797g-50=t","41h-60=t","29i-79=t"]
 -}
  ys = (939, [B 7, B 13,X, X, B 59, X, B 31, B 19])
{-
       ["7a-0=t","13b-1=t","59c-4=t","31d-6=t","19e-7=t"]
 -}
