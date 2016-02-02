import Data.Ratio
import Data.List
import Data.Function
import System.IO
import Data.Time.Clock.POSIX
import System.Posix.Process
import Control.Concurrent
import Network.Libev
import Foreign.C.Types
import System.Posix.Unistd

toler = 400000 -- tolerance in ms
deltas :: ((Float, Float),Float,[Int]) -> [Int]
deltas ((nBeats, beat_unit),bpm,notes) = foo notes millisPerSubBeat
   where millisPerBeat = 1000*beat_unit/nBeats/bpm*secInMin
         millisPerSubBeat = millisPerBeat / nBeats
         secInMin = 60
         foo [] _ = []
         foo (n:ns) t
            | n==1 = t:foo ns millisPerSubBeat
            | n==0 = foo ns (t+millisPerSubBeat) 

matches :: ([(Int,Int)],[Int],[Int]) -> Int -> ([(Int,Int)],[Int],[Int])
matches (acc,ms,dts) t =
    if (close best t)
        then ((best,t-best):acc,ms,(delete best dts))
        else (acc,t:ms,dts)
    where best = bestMatch t dts
          close t1 t2 = abs (t1-t2) < toler
          bestMatch t = head.sortBy (compare `on`abs.(t-))
analyze dts = foldl  matches  ([],[],dts)

data Song = Int

main = do
    let easy4 = ((4.0,4.0),120.0,[1,1,1,1])
    let easy3 = ((3.0,4.0),60.0,[1,1,1])

    print $ deltas easy4
    print $ deltas easy3
    print (analyze [100,150,200,400] [100,120,190,405])

    let timeInMicros = numerator . toRational . (* 1000000) <$> getPOSIXTime
    let loop n t1 dts = do
            input <- hWaitForInput stdin (quot n 1000)
            t2 <- timeInMicros
            let dt = fromInteger (t2-t1)
            if input
                then do
                    print dt
                    (do getChar; return ())
                    if (n-dt) > 0 then loop (n-dt) t2 (dt:dts) else return dts
                else do
                    print "expired timer"
                    if (n-dt) > 0 then loop (n-dt) (t2-(toInteger n)) dts else return dts
    t <- timeInMicros
    let n = 3000000::Int
    res <- loop n t []
    print (reverse res)
    print "done"
