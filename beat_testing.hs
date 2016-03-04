import Data.Ratio
import Data.List
import Data.Function
import System.IO
import Data.Time.Clock.POSIX
import System.Posix.Process
import Network.Libev
import Foreign.C.Types
import System.Posix.Unistd
import System.Process
import qualified Data.ByteString.Lazy as BL
import Data.Binary.Get

toler = 250000 -- tolerance in microseconds
resolution = 1000000 -- tolerance in microseconds
secsInMin = 60

type Meter = (Int, Int)
type Bpm = Int
type Notes = [Int]
type Song = (Meter, Bpm, Notes)

type Delta = Int
type Timestamp = Int

type Err = [(Int, Int)]
type Missed = [Int]
type Extra = [Int]
type Analysis = (Err, Missed, Extra)

deltas :: Song -> [Delta]
deltas ((nBeats, beatUnit),bpm,notes) = reverse $ (\(_,y) -> y)
    (foldl (\(t,dts) n -> if n==0 then (t+period,dts) else (t,t:dts))
           (period,[]) notes)
  where period = beatUnit*resolution*secsInMin `quot` (bpm*nBeats)

matches :: Analysis -> Timestamp -> Analysis
matches (acc,extr,tss) t =
    if (close best t)
        then ((best,t-best):acc,extr,(delete best tss))
        else (acc,t:extr,tss)
  where
    best = bestMatch tss
    close t1 t2 = abs (t1-t2) < toler
    bestMatch = head.sortBy (compare `on`abs.(t-))

timestamps :: [Delta] -> [Timestamp]
timestamps dts = fst $ foldl (\(acc,t) dt -> (t:acc, t+dt)) ([],(head dts)) dts

analyze :: [Delta] -> [Delta] -> Analysis
analyze dts = foldl  matches  ([],[],tss)
  where tss = timestamps dts

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

ms x =  quot x 1000
analysis (a,e,m) = "analysis:  " ++ show (map (\(x,y)->(ms x,ms y)) a)
err (a,e,m) =      "total err: " ++ show (sum (map (\(_,y)->(abs$ms y)) a))
missed (a,e,m) =   "missed:    " ++ show (map (\x->ms x) m)
extra (a,e,m) =    "extra:     " ++ show (map (\x->ms x) e)

report :: Analysis -> String
report raw = unlines (map ($raw) [analysis,err,missed,extra])

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

    let play (dt:dts) bpm = do
        usleep dt
        print "beep"
        if null dts
            then do usleep (bpm*1000000 `quot` bpm);return ()
            else play dts bpm

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
        if x <= 1
            then do
                print y
                return ()
            else do
                print y
                usleep (secsInMin*1000000 `quot` bpm)
                sync bpm (x-1) (y+1)
                return ()
    let timeInMicros = do
            t <- evTime
            return (floor (t * 1e6))
    let audioDts acc avg decay t1 n = do
        input <- BL.getContents
        let v = fromIntegral (runGet getWord32le input)
        if n == 0
            then return acc
            else 
                if v < (213*1e7) && decay == 0
                    then do
                        t2 <- timeInMicros
                        print (t2-t1)
                        audioDts  ((t2-t1):acc) v 15 t2 (n-1)
                    else do
                        audioDts acc v (if decay == 0 then 0 else (decay-1)) t1 n

    let easy4 = ((4,4),120,[1,1,1,1])
    let easy23 = ((6,4),200,notes)
            where notes = concat . take 2 . repeat $ [1,0,0,1,0,0,1,1,0,1,0,0]
    let easy3 = ((3,4),100,[1,1,1])
    let pName = easy4
    let ref_dts = reverse $ deltas pName

    let dur = realToFrac (
         (foldl (+) 0 ref_dts) + toler)::Foreign.C.Types.CDouble
    let ((nBeat,_),bpm,_) = pName

    --(_, Just hout, _, _) <-
     --     createProcess (proc "rec" ["-t raw","-e unsigned-integer","-b 32","-q","-"]){ std_out = CreatePipe }
    sync bpm nBeat 1
    forkProcess (play ref_dts bpm)

    t <- timeInMicros
    res <- input_loop dur t []

    -- res <- (audioDts [] 0 0 (t+250000) 4)  -- the + 250000 is a shim for syncing
    let input_dts = reverse res
    print "deltas"
    print ref_dts
    print input_dts
    print "timestamps"
    print (timestamps ref_dts)
    print (timestamps input_dts)
    putStrLn "----------- summary --------------"
    putStrLn (report (analyze ref_dts (timestamps input_dts)))

{--
fact = 1/2
smooth old new = old * fact + new * (1-fact)
main :: IO ()
main = do

  t1 <- timeInMicros
  print res
  print "hey"
-- rec -c1 -t sox  -e signed-integer -r 48k - | play -c 1 -t sox -e signed-integer  -r 48k -
-- :w |!ghc dsp.hs && rec -t raw -e unsigned-integer -b 32 -q - | ./dsp
--}
