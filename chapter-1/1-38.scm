
(define (cont-frac n d k)
  (define (cont-frac-iter n d k result)
    (if (< k 1)
        result
        (cont-frac-iter n d (- k 1) (/ (n k) (+ (d k) result)))))
  (cont-frac-iter n d k 0))

(define (n k) 1.0)

(define (d k)
  (cond ((= (remainder k 3) 1) 1)
        ((= (remainder k 3) 2) (* 2 (+ 1 (quotient k 3))))
        (else 1)))

(+ 2 (cont-frac n d 1000))
;Value: 2.7182818284590455