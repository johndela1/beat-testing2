;lynx http://www.catonmat.net/blog/derivation-of-ycombinator
(define (rfact n)
        (if (= n 0)
            1
            (* n (rfact (- n 1)))))

(define (jfact f)
    (lambda (n)
        (if (= n 0)
            1
            (* n ((f f) (- n 1))))))  ; key part is calling f on f

(print ((jfact jfact) 5))
(print
    (((lambda (f) (lambda (x)  (if (< x 2) 1 (* x ((f f) (- x 1)))))) (lambda (f) (lambda (x)  (if (< x 2) 1 (* x ((f f) (- x 1))))))) 5)
)
(print ((jfact jfact) 5))

(define Y
  (lambda (h)
      ((lambda (x) (x x))
           (lambda (g)
                  (h (lambda args (apply (g g) args)))))))
(define (jfact f)
    (lambda (n)
        (if (= n 0)
            1
            (* n (f (- n 1))))))
(define fact (Y jfact))
(print 'hey)
(print (fact 5))
(exit)
