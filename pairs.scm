(define (cons x y)
    (lambda (p)
        (if (= p 0)
            x
            y)))

(define (car l)
    (l 0))

(define (cdr l)
    (l 1))

(let ((l (cons 1 (cons 2 '()))))
(print  'car: (car l))
(print  'car-of-cdr: (car (cdr l)))
(exit))
