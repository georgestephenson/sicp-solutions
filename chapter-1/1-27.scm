(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (fermat-all n)
  (fermat-all-iter n (- n 1)))

(define (fermat-all-iter n a)
  (cond ((= a 1) true)
        ((= (expmod a n n) a) (fermat-all-iter n (- a 1)))
        (else false)))

(fermat-all 561)
;Value: #t
(fermat-all 1105)
;Value: #t
(fermat-all 1729)
;Value: #t
(fermat-all 2465)
;Value: #t
(fermat-all 2821)
;Value: #t
(fermat-all 6601)
;Value: #t