import qualified Data.ByteString.Lazy as BL
import Data.Binary.Get
import Network.Libev

fact = 1/2
smooth old new = old * fact + new * (1-fact)
main :: IO ()
main = do
  let timeInMicros = do
            t <- evTime
            return (floor (t * 1000000))
  let bar avg decay t1 = do
        input <- BL.getContents
        let v = fromIntegral (runGet getWord32le input)
        if v < 2140000000 && decay == 0
            then do
                t2 <- timeInMicros
                print (quot (t2-t1) 1000)
                bar v 15 t2
                return ()
            else do
                bar v (if decay == 0 then 0 else (decay-1)) t1
                return ()

  t1 <- timeInMicros
  bar 0 0 t1
  print "hey"
-- rec -c1 -t sox  -e signed-integer -r 48k - | play -c 1 -t sox -e signed-integer  -r 48k -
-- :w |!ghc dsp.hs && rec -t raw -e unsigned-integer -b 32 -q - | ./dsp
