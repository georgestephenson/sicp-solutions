
; Part A - show that sum and product are special cases of accumulate

; iterative version

(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))

(define (inc n) (+ n 1))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (sum-cubes a b)
  (sum cube a inc b))

(sum-cubes 1 10)
;Value: 3025

(define (product term a next b)
  (accumulate * 1 term a next b))

(define (factorial n)
  (product values 1 inc n))

(factorial 7)
;Value: 5040

; Part B - write as recursive process

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
         (accumulate combiner null-value term (next a) next b))))

(sum-cubes 1 10)
;Value: 3025
(factorial 7)
;Value: 5040