import Data.Ratio
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
toler = 50 -- tolerance in ms
deltas :: ((Float, Float),Float,[Int]) -> [Float]
deltas ((nBeats, beat_unit),bpm,notes) = foo notes millisPerSubBeat
   where millisPerBeat = 1000*beat_unit/nBeats/bpm*secInMin
         millisPerSubBeat = millisPerBeat / nBeats
         secInMin = 60
         foo [] _ = []
         foo (n:ns) t
            | n==1 = t:foo ns millisPerSubBeat
            | n==0 = foo ns (t+millisPerSubBeat) 

close :: Int -> Int -> Bool
close t1 t2 = abs (t1-t2) < toler
main = do
    let easy4 = ((4.0,4.0),120.0,[1,1,1,1])
    let easy3 = ((3.0,4.0),60.0,[1,1,1])

    print $ deltas easy4
    print $ deltas easy3
    print $ close 100 51
