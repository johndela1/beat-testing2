import qualified Data.ByteString.Lazy as BL
import Data.Binary.Get
import Network.Libev

fact = 1/2
smooth old new = old * fact + new * (1-fact)
main :: IO ()
main = do
  let timeInMicros = do
            t <- evTime
            return (floor (t * 1e6))
  let audioDts acc avg decay t1 n = do
        input <- BL.getContents
        let v = fromIntegral (runGet getWord32le input)
        if n == 0
            then return acc
            else 
                if v < (214*1e7) && decay == 0
                    then do
                        t2 <- timeInMicros
                        audioDts  ((t2-t1):acc) v 15 t2 (n-1)
                    else do
                        audioDts acc v (if decay == 0 then 0 else (decay-1)) t1 n

  t1 <- timeInMicros
  res <- (audioDts [] 0 0 t1 4)
  print res
  print "hey"
-- rec -c1 -t sox  -e signed-integer -r 48k - | play -c 1 -t sox -e signed-integer  -r 48k -
-- :w |!ghc dsp.hs && rec -t raw -e unsigned-integer -b 32 -q - | ./dsp
