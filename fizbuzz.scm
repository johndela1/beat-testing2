(define (fizbuzz n)
    (define (divides? x)
        (= 0 (modulo n x)))

    (if  (divides? 3) (display 'fiz))
    (if  (divides? 5) (display 'buz))
    (if  (or (divides? 3) (devides? 5)) (print)))


(define (nums n)
    (if (< n 101)
            (cons n (nums (add1 n)))
        '()))


(map fizbuzz (nums 1))
