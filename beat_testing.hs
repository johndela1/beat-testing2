import Data.Ratio
import Data.List
import Data.Function
import System.IO
import Data.Time.Clock.POSIX
import System.Posix.Process
import Network.Libev
import Foreign.C.Types
import System.Posix.Unistd

toler = 250000 -- tolerance in microseconds
deltas :: ((Float, Float),Float,[Int]) -> [Int]
deltas ((nBeats, beatUnit),bpm,notes) = foo notes uSecsPerSubBeat
   where uSecsPerBeat = 1000000*beatUnit/bpm*secsInMin
         uSecsPerSubBeat = uSecsPerBeat / nBeats
         secsInMin = 60
         foo [] _ = []
         foo (n:ns) t
            | n==1 = (ceiling t):foo ns uSecsPerSubBeat
            | n==0 = foo ns (t+uSecsPerSubBeat) 

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

timestamps' :: ([Int],Int) -> Int -> ([Int],Int)
timestamps' (acc,t) dt = (t:acc, t+dt)
timestamps dts = (fst(foldl timestamps' ([],(head dts)) dts))

main = do
    let poll n = do
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

    let play [] = do usleep 1000000;return ()
    let play (dt:dts) = do
        usleep dt
        print dt
        print "beep"
        play dts

    let timeInMicros = numerator . toRational . (* 1000000) <$> getPOSIXTime
    let input_loop dur t1 dts = do
            poll dur
            t2 <- timeInMicros
            let dt = fromInteger (t2-t1)
            if (dur-dt) > 0
                then do
                    (do getChar; return ())
                    input_loop (dur-dt) t2 ((ceiling dt):dts)
                else do
                     return dts

    let sync bpm x y = do
        if x < 1
            then
                return ()
            else do
                print y
                usleep (bpm*1000000 `quot` 60)
                sync bpm (x-1) (y+1)
                return ()
   -- forkProcess (play (deltas easy4))
    let easy4 = ((4,4),60,[1,1]) -- ,1,1,1])
    let easy3 = ((3,4),60,[1,1,1])
    let pName = easy4

    let dur = realToFrac (
         (foldl (+) 0 (deltas pName)) + toler)::Foreign.C.Types.CDouble
    let (_,bpm,_) = pName
    sync (floor bpm) 1 3
    print 4
    t <- timeInMicros
    res <- input_loop dur t []
    let ref_dts = reverse $ deltas pName
    let input_dts = reverse res
    print "deltas"
    print ref_dts
    print input_dts
    print "timestamps"
    print (timestamps ref_dts)
    print (timestamps input_dts)
    print "analysis"
    print (analyze (timestamps ref_dts)  (timestamps input_dts))
    print "done"
