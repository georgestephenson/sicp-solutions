(define (make-accumulator sum)
  (lambda (addend)
    (begin (set! sum (+ sum addend))
           sum)))

(define A (make-accumulator 5))

(A 10)
;Value: 15

(A 10)
;Value: 25