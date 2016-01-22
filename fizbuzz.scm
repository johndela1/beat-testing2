(define (fizbuzz n)
    (define (devides? x)
        (= 0 (modulo n x)))

    (if  (devides? 3) (display 'fiz))
    (if  (devides? 5) (display 'buz))
    (if  (or (devides? 3) (devides? 5)) (print)))


(define (nums n)
    (if (< n 101)
            (cons n (nums (add1 n)))
        '()))


(map fizbuzz (nums 1))
