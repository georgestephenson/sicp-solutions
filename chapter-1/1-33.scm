; Prerequisites

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (next n)
  (if (= n 2)
      3
      (+ n 2)))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

; Filtered-accumulate

(define (filtered-accumulate combiner null-value term a next b filter)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) 
          (if (filter a)
              (combiner result (term a))
              result))))
  (iter a null-value))

; Part A

(define (inc n) (+ n 1))

(define (sum-of-squares-of-primes a b)
  (define (filter a)
    (prime? a))
  (filtered-accumulate + 0 square a inc b filter))

(sum-of-squares-of-primes 13 17)
;Value: 458


; Part B

(define (product-of-relative-primes n)
  (define (filter a)
    (= (gcd a n) 1))
  (filtered-accumulate * 1 values 0 inc (- n 1) filter)))

; if n=13, f(13) = 12! = 479001600

(product-of-relative-primes 13)
;Value: 479001600