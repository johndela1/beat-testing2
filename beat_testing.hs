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
{--
#!/usr/local/bin/csi -s

(use posix)
(use srfi-1)
(use srfi-18)
;; (use sdl-mixer)

(define HZ 500)
(define TOLER 300)
(define BEAT 4)
(define SECS/MIN 60)
;; (open-audio)
;; (define noise (load-sample "boom.vorbis"))

(define (defined? v)
  (call-with-current-continuation
   (lambda (k)
     (with-exception-handler
      (lambda (exn)
	(k #f))
      (lambda () (eval (with-input-from-string v read)))))))

(define (secs->millis t)
  (* t 1000))

(define (millis->secs t)
  (/ t 1000))

(define (bpm->bps bpm)
  (/ bpm SECS/MIN))

(define (ensure-int x)
  (inexact->exact (round x)))
		   
(define (play pattern bpm)
  (define (rest-time)
    (let* ((note-div (car pattern))
	   (notes (cadr pattern))
	   (note-duration (/ (/ BEAT note-div) (bpm->bps bpm))))
      (define (calc-delay rests acc)
	(if (= 0 (car rests))
	    (calc-delay (cdr rests) (+ acc note-duration))
	    (+ acc note-duration)))
      (calc-delay  (reverse notes) 0)))
  
  (let ((deltas (pattern->deltas pattern bpm)))
    (define (loop deltas)
      (if (null? deltas)
	  (thread-sleep! (rest-time))
	  (begin
	    (thread-sleep! (millis->secs (car deltas)))
	    ;; (play-sample noise)
	    (print 'note)
	    (loop (cdr deltas)))))
    (loop deltas)))

(define (analyze pattern input bpm)
  (define (deltas->tss deltas)
    (define (loop deltas acc-time)
      (if (null? deltas)
	  '()
	  (let ((ts (+ acc-time (car deltas))))
	    (cons ts (loop (cdr deltas) ts)))))
    (loop deltas 0))

  (define (close-enough t1 t2)
    (<= (abs (- t1 t2)) TOLER))

  (define (remove i l)
    (filter (lambda (x) (not (eqv? x i))) l))
   
  (define (find-match ts l)
    (cond
     ((null? l) '())
     ((close-enough (car l) ts) (car l))
     (else (find-match ts (cdr l)))))

  (define (loop ref input)
    (if (null? ref)
	input
	(let ((match (find-match (car ref) input))
	      (sample (car ref))
	      (samples (cdr ref)))
	  (if (null? match)
	      (cons (cons sample 'missed)
		    (loop samples input))
	      (cons (cons sample (- match sample))
		    (loop samples
			  (remove match input)))))))
  (let ((results (loop (deltas->tss (pattern->deltas pattern bpm))
		    (deltas->tss input))))
    (define (passed? res)
      (cond
       ((null? res) 't)
       ((and
	 (not (atom? (car res)))
	 (integer? (cdar res)))
	(passed? (cdr res)))
       (else #f)))
    (cons (passed? results) results))) 

(define (record pattern bpm)
  ;; XXX early beat is not punished but rather set to perfect score
  ;; because the early keypress is waiting on the keyboard buffer
  ;; and when the programs reads it gets it at T0 and when
  ;; the song starts at T0 it is seen as a perfect match
  (define (poll-keys)
    (if (null? (file-select '(0) '() 0))
	#f
	(begin
	  (read-byte)
	  #t)))
  (define (seconds/beat bpm)
    (/ SECS/MIN bpm))
  (let* ((note-div (car pattern))
	 (notes (cadr pattern))
	 (cycles/beat (ensure-int
		       (* (seconds/beat bpm) HZ)))
	 (cycles/note (ensure-int
		       (* cycles/beat (/ BEAT note-div))))
	 (sample-count (* (length notes) cycles/note)))
    (define (loop n delta)
      (cond
       ((<= n 0) '())
       (else
	(let ((sample (poll-keys)))
	  (thread-sleep! (/ 1 HZ))
	  (if (= (modulo n cycles/beat) 0)
	      (print 'beat))
	  (if sample
	      (cons (/ delta (/ HZ 1000)) (loop (sub1 n) 0))
	      (loop (sub1 n) (add1 delta)))))))
    (loop sample-count 0)))

(define (loop-pattern pattern n)
  ;; XXX needs to handle 'pattern' not list of notes
  (if (= n 0)
      '()
      (append pattern
	      (loop-pattern pattern (sub1 n)))))

(define (pattern->deltas pattern bpm)
  (let* ((note-div (car pattern))
	 (notes (cadr pattern))
    	 (note-duration (ensure-int (secs->millis
			 (/ (/ BEAT note-div) (bpm->bps bpm))))))
    (define (convert notes acc)
      (cond
       ((null? notes) '())
       ((= 0 (car notes)) (convert (cdr notes) (+ acc note-duration)))
       (else (cons acc (convert (cdr notes) note-duration)))))
    (convert notes 0)))

(define (trial pattern bpm)
  (play pattern bpm)
  (analyze pattern (record pattern bpm) bpm))

(define (report results)
  (define (sum l)
    (apply + l))
  (let ((passed (car results))
	(errors (cdr results)))
    (print 'errors: errors)
    (if passed
	(begin
	  (let ((total-error
		  (sum (map (lambda (x) (abs (cdr x)))
			    (cdr results)))))
	    (print 'passed)
	    (print 'total-error: total-error)
	    (print 'avg-error: (/ total-error (length errors)))))
	(print 'failed))))


(define easy-4 '(4 (1 1 1 1)))
(define easy-8 '(8 (1 1 1 1 1 1 1 1)))
(define honky-tonk '(8 (1 1 0 1 1 0 1 0  1 1 0 1 1 0 0 1)))
(define honky-tonk-2 (loop-pattern honky-tonk 2))
(define syncopate '(8 (1 0 1 0 0 1 0 1)))
(define syncopate-2 (loop-pattern syncopate 2))
(define green-sleeves '(8 (1 0 0 0 1 0  1 0 0 1 1 0  1 0 0 0 1 0  1 0 0 1 1 0)))
(define easy-3 '(4 (1 1 1 1 1 1)))
(define honky-tonk-begin '(8 (1 1 0 1 1 0 1 0)))

(print (analyze '(4 (1 1 1 1)) '(144.0 828.0 926.0 954.0) 60))
(exit)
;; main entry point
(if (= (length (argv)) 5)
    (let ((pattern-name (list-ref (argv) 3))
	  (bpm (string->number (list-ref (argv) 4))))
      (if (defined? pattern-name)
	  (let ((pattern (eval (with-input-from-string pattern-name read))))
	    (report (trial pattern bpm)))
	  (print 'use-valid-pattern-name)))
    (print 'must-specify-pattern-and-bpm))
--}
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
