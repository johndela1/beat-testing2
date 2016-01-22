#!/usr/local/bin/csi -s

(define (nand x y)
    (if x
        (if y
            #f
            #t)
        #t))

(define (jnot x)
    (nand x 1))

(define (jand x y) (nand (nand x y) (nand x y)))

(define (jand x y) (let ((z (nand x y)))
     (nand z z)))

(define (jor x y)
    (nand (nand x x) (nand y y)))

(assert (nand #f #f))
(assert (nand #t #f))
(assert (nand #f #t))
(assert (not (nand #t #t)))

(assert (not (jand #f #f)))
(assert (not (jand #f #t)))
(assert (not (jand #t #f)))
(assert (jand #t #t))

(assert (not (jnot #t)) #f)
(assert (jnot #f))

(assert (not (jor #f #f)))
(assert (jor #f #t))
(assert (jor #t #f))
(assert (jor #t #t))

'(print (jand #f #f))
'(print (jand #f #t))
'(print (jand #t #f))
'(print (jand #t #t))
