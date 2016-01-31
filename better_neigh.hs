import System.Random
import Data.List

people = [10,20,5,20,10,90,90]

improvePerson world n
    | switch    = [c1] ++ first ++ [c2] ++ last
    | otherwise = world
   where node = world!!n
         first = (take (n+1) world)
         last = (drop (n+3) world)
         c1 = world!!(n+1)
         c2 = world!!(n+2)
         d1 = abs (node-c1)
         d2 = abs (node-c2)
         switch = d2 < d1

improve :: [Int] -> Int -> [Int]
improve world n
     | n > length world - 3 = world
     | otherwise = improve (improvePerson world n) (n+1)

randomlist :: Int -> StdGen -> [Int]
randomlist n = take n . unfoldr (Just . random)

score :: [Int] -> [Int]
score [] = []
score (x:xs)
    | xs == []  = []
    | otherwise = [abs (x - head xs)] ++ score(xs)
main = do
    seed <- newStdGen
    let seed = mkStdGen 3
    let people = map (\x->mod x 100) (randomlist 1000 seed)
    let p1 = (improve people  0) 
    print $sum(score people)
    print $sum(score p1)
