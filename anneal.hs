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

best :: Placement -> Location -> Location
best' [] person curBest = (fst person, snd curBest)
best' (trialLoc:placement) curLoc curBest
    | abs (seeker-candidate) < abs (seeker-(fst curBest)) = best' placement curLoc trialLoc
    | otherwise = best' placement curLoc  curBest
  where seeker = fst curLoc
        candidate = fst trialLoc
best neighbors placement  = best' neighbors placement (head neighbors)

anneal :: Placement -> Placement
anneal' [] acc = reverse acc
anneal' (location:placement) acc =
    anneal' placement $ bestNeighbor:acc
  where neighbors = filter (\(x,y)->y>=table-1&&y<=table+1) (placement++acc)
        person = fst location
        table = snd location
        bestNeighbor = if person == 1000 then location else best neighbors location
anneal placement = anneal' placement []

sortp :: Placement -> Placement
sortp = sortBy (compare `on` snd)

filtp:: Placement -> Placement
filtp = filter (\(p,t)->p/=1000)

pre = filtp . sortp

main = do
    --print (best [(2,1),(3,5),(3,6)] (1,5))

    print $ pre placement
    print (map pre (runn anneal placement 2))
