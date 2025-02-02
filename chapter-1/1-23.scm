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

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes larger-than smaller-than)
  (define n (+ larger-than 1))
  (cond ((>= larger-than (- smaller-than 1)) (newline) (display "search complete"))
        ((even? n) (search-for-primes n smaller-than))
        (else (timed-prime-test n) 
              (search-for-primes n smaller-than))))

(search-for-primes 1000000 1000100)
;1000003 *** 0.
;1000033 *** 0.
;1000037 *** 0.

(search-for-primes 1000000000 1000000100)
;1000000007 *** .02
;1000000009 *** 1.9999999999999997e-2
;1000000021 *** 2.0000000000000004e-2

(search-for-primes 10000000000 10000000100)
;10000000019 *** .04999999999999999
;10000000033 *** .04999999999999999
;10000000061 *** 4.0000000000000036e-2

(search-for-primes 100000000000 100000000100)
;100000000003 *** .15000000000000002
;100000000019 *** .14
;100000000057 *** .14

; 1. the first three primes above 1 billion
(/ (+ .02 1.9999999999999997e-2 2.0000000000000004e-2) 3)
;Value: .02
; 0.020 seconds on average - previously 0.026 seconds

; 2. the first three primes above 10 billion
(/ (+ .04999999999999999 .04999999999999999 4.0000000000000036e-2) 3)
;Value: .04666666666666667
; 0.047 seconds on average - previously 0.073 seconds

; 3. the first three primes above 100 billion
(/ (+ .15000000000000002 .14 .14) 3)
;Value: .14333333333333334
; 0.143 seconds on average - previously 0.21 seconds

;There is a noticeable improvement but it is not twice as fast.

;Divide average of previous times by average of new times to determine ratio
(/ (+ 0.026 0.073 0.21) (+ 0.02 0.047 0.143))
;Value: 1.4714285714285715

;Observed ratio is that the new algorithm is 1.47 times faster than the previous algorithm.
;The main reason is it not twice as fast is that the (next n) procedure with if check on (= n 2) and
;overheads associated with defining a procedure will be slower than (+ n 1) 
;(which we can assume is very fast)