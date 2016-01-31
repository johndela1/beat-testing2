import Control.Exception
import Data.Function (on)
import Data.List (sortBy)

type Person     = Float
type Table      = Int
type Placement  = [Location]
type Location   = (Person, Table)
type People     = [Person]
type Tables     = [Table]

prandom x = mod  (x * 1103515245 + 12345) 1000000000  
people = shuffle $ concat [[n | m <- [1..9]] | n <- [1..50]]
tables = [1..10]
placement = (zip people (concat (repeat tables)))  ++ [(1000,n) | n <- tables]

merge [] ys = ys
merge (x:xs) ys = x:merge ys xs

shuffle' xs n
        | n > 0  = shuffle' (merge (reverse a) b) (n-1)
        | otherwise = xs
    where (a,b) = splitAt (quot (length xs) 2) xs
shuffle xs = shuffle' xs (quot (length xs) 2)

runN f x n
    | n == 0 = []
    | otherwise = outp:(runN f outp (n-1))
  where outp = f x

chooseLoc comp [] _ curBest = curBest
chooseLoc comp (trialLoc:placement) curLoc curBest
    | fst trialLoc == 1000 = chooseLoc comp placement curLoc curBest
    | fst curLoc == 1000 = chooseLoc comp placement curLoc curBest
    | comp (abs (seeker-candidate)) (abs (seeker-(fst curBest))) =
        chooseLoc comp placement curLoc trialLoc
    | otherwise = chooseLoc comp placement curLoc  curBest
  where seeker = fst curLoc
        candidate = fst trialLoc

bestLoc :: Placement -> Location -> Location
bestLoc placement location = chooseLoc (<) placement location $ head placement
bestLoc [] location = location

worstLoc :: Placement -> Location -> Location
worstLoc placement location = chooseLoc (>) placement location $ head placement

anneal :: Placement -> Placement
anneal' [] acc _ = reverse acc
anneal' (location@(person,table):placement) acc n =
    anneal' placement choosenLoc (n+1)
  where neighbors = filter (\(_,candiate)->abs(table-candiate)<=1)
                           (placement++acc)
        choosenLoc = if (mod (prandom n) 2 /= 0) then (bestLoc  neighbors location):acc
                              else (worstLoc neighbors location):acc
anneal placement = anneal' placement [] 0

--avgVar :: Placement -> Int
avgVar placement =
    sum (map (\p->abs (p-avg)) people) / people_cnt
    where people     = map (\(p,t) -> p) placement
          people_cnt = fromIntegral (length people)
          avg        = sum people / people_cnt

summary placement = map avgVar (filter (not.null) (sel <*> placement))
    where sel = (map (\table->filter(\(_,t)->table==t)) tables)

sortp :: Placement -> Placement
sortp = sortBy (compare `on` snd)

filtp:: Placement -> Placement
filtp = filter (\(p,t)->p/=1000)

pre =  sortp -- filtp . sortp
main = do
    assert (worstLoc [(2,1),(4,9),(3,6)] (1,5) == (4,9)) print "pass"
    assert (bestLoc [(2,1),(3,5),(3,6)] (1,5) == (2,1))  print "pass"
    assert (length (anneal placement) == length placement) print "pass"

    let p = placement --[(1.0,1),(2.0, 1),(1.0,2),(3.0,2), (9.0,3),(9.0,3),(7.0,4)]
    let runs = runN anneal p 5
    let report [] = do print "done"
    let report (r:rs) = if rs == [] then print "done" else do
           -- print $ sortp r
            --print ((show (summary [r])) ++ ", " ++ (show (sum (summary[r]))))
            print (show (sum (summary[r])))
            report rs
    --print placement
    print $ pre p
    report runs
