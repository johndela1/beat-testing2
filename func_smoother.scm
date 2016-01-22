(define (make-smoother factor)
    (lambda (old new)
        (quotient (+ (* old factor) (* (- 100 factor) new)) 100)))

(define smoother (make-smoother 25))
(define (smooth l prev)
    (cond
        ((null? l) '())
        (else (let ((curr (smoother prev (car l))))
            (cons curr (smooth (cdr l) curr))))))

(define (loop prev_period prev_time)
    (file-select '(0) '() 100)
    (read-byte)
    (let* ((t (current-milliseconds))
           (period (smoother prev_period (- t prev_time))))
        (print period)
        (loop period t)))
(loop 0 0)
