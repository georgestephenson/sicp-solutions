(define (inc n) (+ n 1))
(define (sum-cubes a b)
  (sum cube a inc b))

; linear recursion
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(sum-cubes 1 10)
;Value: 3025

; linear iteration
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

(sum-cubes 1 10)
;Value: 3025