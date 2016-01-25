import Control.Exception
import Data.Function (on)
import Data.List (sortBy)

type Person     = Int
type Table      = Int
type Placement  = [Location]
type Location        = (Person, Table)
type People     = [Person]
type Tables     = [Table]

prandom x = mod  (x * 1103515245 + 12345) 1000000000  
people = shuffle $ concat [[n | m <- [1..6]] | n <- [1..2]]
tables = [1..length people]
placement = (zip people tables) ++ [(1000,n) | n <- [1..length tables]]
--placement = [(1,1),(1,2),(8,3),(9,4), (1000,1),(1000,2),(1000,3),(1000,4)]

merge [] ys = ys
merge (x:xs) ys = x:merge ys xs

shuffle' xs n
        | n > 0  = shuffle' (merge (reverse a) b) (n-1)
        | otherwise = xs
    where (a,b) = splitAt (quot (length xs) 2) xs
shuffle xs = shuffle' xs (quot (length xs) 2)

runn f x n
    | n == 0 = []
    | otherwise = outp:(runn f outp (n-1))
  where outp = f x

bestLoc :: Placement -> Location -> Location
bestLoc' [] person curBest = curBest
bestLoc' (trialLoc:placement) curLoc curBest
    | abs (seeker-candidate) < abs (seeker-(fst curBest)) =
        bestLoc' placement curLoc trialLoc
    | otherwise = bestLoc' placement curLoc  curBest
  where seeker = fst curLoc
        candidate = fst trialLoc
bestLoc placement location = bestLoc' placement location $ head placement

anneal :: Placement -> Placement
anneal' [] acc = reverse acc
anneal' (location@(person,table):placement) acc =
    anneal' placement $ (bestLoc neighbors location):acc
  where neighbors = filter (\(_,candiate)->abs(table-candiate)<=1)
                           (placement++acc)
anneal placement = anneal' placement []

sortp :: Placement -> Placement
sortp = sortBy (compare `on` snd)

filtp:: Placement -> Placement
filtp = filter (\(p,t)->p/=1000)

pre = filtp . sortp

main = do
    print (bestLoc [(2,1),(3,5),(3,6)] (1,5))
    assert (bestLoc [(2,1),(3,5),(3,6)] (1,5) == (2,1))  print "pass"
    print $ pre placement
    mapM_ (\l->print l) (map pre (runn anneal placement 2))
