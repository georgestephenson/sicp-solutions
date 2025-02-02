(define (fast-expt b n)
  (expt-iter b n 1))

(define (expt-iter b n product)
  (cond ((= n 0) product)
        ((odd? n) (expt-iter b (- n 1) (* product b)))
        (else (expt-iter (square b) (/ n 2) product))))

(define (odd? n)
  (= (remainder n 2) 1))
