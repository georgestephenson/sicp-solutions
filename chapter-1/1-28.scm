(define (mr-expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (mr-square-check (mr-expmod base (/ exp 2) m)
                          m))
        (else
         (remainder (* base (mr-expmod base (- exp 1) m))
                    m))))

(define (mr-square-check x m)
  (if (and (= (remainder (square x) m) 1)
           (not (or (= x 1) 
                    (= x (- m 1)))))
      0
      (remainder (square x) m)))

(define (mr-test n)
  (define (try-it a)
    (= (mr-expmod a (- n 1) n) 1))
  (try-it (+ 1 (random (- n 1)))))

(define (mr-prime?-times n times)
  (cond ((= times 0) true)
        ((mr-test n) (mr-prime?-times n (- times 1)))
        (else false)))

(define (mr-prime? n)
  (mr-prime?-times n 20))

; Carmicheal numbers non-prime
(mr-prime? 561)
;Value: #f
(mr-prime? 1105)
;Value: #f
(mr-prime? 1729)
;Value: #f
(mr-prime? 2465)
;Value: #f
(mr-prime? 2821)
;Value: #f
(mr-prime? 6601)
;Value: #f

; Previously found primes
(mr-prime? 1000003)
;Value: #t
(mr-prime? 1000033)
;Value: #t
(mr-prime? 1000037)
;Value: #t