;https://karczmarczuk.users.greyc.fr/Essays/church.html

(define zero
    (lambda (x)
        (identity x)))

(define (add-1 n)
    (lambda (f)
        (lambda (x)
            (f ((n f) x)))))

(print (((add-1 zero) add1) 0))
(exit)
