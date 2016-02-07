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
   where millisPerBeat = 1000000*beat_unit/bpm*secInMin
         millisPerSubBeat = millisPerBeat / nBeats
         secInMin = 60
         foo [] _ = []
         foo (n:ns) t
            | n==1 = (ceiling t):foo ns millisPerSubBeat
            | n==0 = foo ns (t+millisPerSubBeat) 

matches :: ([(Int,Int)],[Int],[Int]) -> Int -> ([(Int,Int)],[Int],[Int])
matches (acc,ms,dts) t =
    if (close best t)
        then ((best,t-best):acc,ms,(delete best dts))
        else (acc,t:ms,dts)
    where best = if dts==[] then 9999999 else bestMatch t dts
          close t1 t2 = abs (t1-t2) < toler
          bestMatch t = head.sortBy (compare `on`abs.(t-))
analyze dts = foldl  matches  ([],[],dts)

data Song = Int

stdinCb :: IoCallback
stdinCb evLoopPtr evIoPtr revents = do
  --putStrLn "stdin ready"
  evIoStop evLoopPtr evIoPtr
  evUnloop evLoopPtr 2 {- 2 = EVUNLOOP_ALL -}

-- Another callback, this time for a timeout.
timeoutCb :: TimerCallback
timeoutCb evLoopPtr evIoPtr revents = do
  --putStrLn "timeout"
  evUnloop evLoopPtr 1 {- 1 = EVUNLOOP_ONE -}
main = do
    let easy4 = ((4.0,4.0),60.0,[1,1])
    let easy3 = ((3.0,4.0),60.0,[1,1,1])

--    print $ deltas easy4
 --   print $ deltas easy3
    --print (analyze [100,150,200,400] [51, 140,190,405,600])
---------------------------------------------------
    let sleep n = do
        stdinWatcher <- mkEvIo
        timeoutWatcher <- mkEvTimer
        stdinCb_ <- mkIoCallback stdinCb
        timeoutCb_ <- mkTimerCallback timeoutCb
        loop <- evDefaultLoop 0
        evIoInit stdinWatcher stdinCb_ {- 0 = STDIN_FILENO -} 0 ev_read
        evIoStart loop stdinWatcher
        evTimerInit timeoutWatcher timeoutCb_ (n/1000000) 0.0
        evTimerStart loop timeoutWatcher
        evLoop loop 0
---------------------------------------------------
    let play [] = do threadDelay 1000000;return ()
    let play (dt:dts) = do
        threadDelay dt
        print dt
        print "beep"
        play dts

    let timeInMicros = numerator . toRational . (* 1000000) <$> getPOSIXTime
    let input_loop n t1 dts = do
            sleep n
            t2 <- timeInMicros
            let dt = fromInteger (t2-t1)
            if (n-dt) > 0
                then do
                    print dt
                    (do getChar; return ())
                    input_loop (n-dt) t2 (ceiling dt:dts)
                else return dts
    let sync x = do
        if x < 1
            then
                return ()
            else do
                print x
                sleep 1000000
                sync (x-1)
                return ()
   -- forkProcess (play (deltas easy4))
    -- sync 2
    print "nanosleepbefore1"
    t1 <- evTime
    nanosleep 500000000
    t2 <- evTime
    print (t2-t1)

    print "usleepbefore1"
    t1 <- evTime
    usleep 500000
    t2 <- evTime
    print (t2-t1)

    print "internal sleepbefore1"
    t1 <- evTime
    sleep 500000
    t2 <- evTime
    print (t2-t1)

    print "internal again! sleepbefore1"
    t1 <- evTime
    sleep 500000
    t2 <- evTime
    print (t2-t1)
    print "begin"
    t <- timeInMicros
    let n = 3000000::Foreign.C.Types.CDouble
    res <- input_loop n t []

    let dts = reverse $ deltas easy4
    let r = reverse res
   -- print d
   -- print r
    --print (analyze [100,150,200,400] [100,120,190,405])

    -- timestamps' :: ([Int],Int) -> Int -> ([Int],Int)
    let timestamps' (acc,t) dt = (t:acc, t+dt)
    let timestamps dts = (fst(foldl timestamps' ([],0) dts))

    print (timestamps dts)
    print (timestamps r)
    print (analyze (timestamps dts)  (timestamps r))
    print "done"
