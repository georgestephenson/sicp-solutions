
(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

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

(search-for-primes 1000 1100)
;1009 *** 0.
;1013 *** 0.
;1019 *** 0.

(search-for-primes 10000 10100)
;10007 *** 0.
;10009 *** 0.
;10037 *** 0.

(search-for-primes 100000 100100)
;100003 *** 0.
;100019 *** 0.
;100049 *** 0.

(search-for-primes 1000000 1000100)
;1000003 *** 9.999999999999998e-3
;1000033 *** 0.
;1000037 *** 0.

(search-for-primes 1000000000 1000000100)
;1000000007 *** 3.0000000000000006e-2
;1000000009 *** .01999999999999999
;1000000021 *** .03

(search-for-primes 10000000000 10000000100)
;10000000019 *** .08000000000000002
;10000000033 *** .06999999999999995
;10000000061 *** .07000000000000006

(search-for-primes 100000000000 100000000100)
;100000000003 *** .21000000000000008
;100000000019 *** .20999999999999996
;100000000057 *** .20999999999999996

;To get an observable time difference on modern 2025 hardware,
;I'll compare times taken to test:

; 1. the first three primes above 1 billion
(/ (+ 3.0000000000000006e-2 .01999999999999999 .03) 3)
;Value: .02666666666666666
; 0.026 seconds on average

; 2. the first three primes above 10 billion
(/ (+ .08000000000000002 .06999999999999995 .07000000000000006) 3)
;Value: .07333333333333335
; 0.073 seconds on average

; 3. the first three primes above 100 billion
(/ (+ .21000000000000008 .20999999999999996 .20999999999999996) 3)
;Value: .21
; 0.21 seconds on average

(/ .07333333333333335 .02666666666666666)
;Value: 2.750000000000001
(/ .21 .07333333333333335)
;Value: 2.863636363636363
(sqrt 10)
;Value: 3.1622776601683795

;The primes above 10 billion took 2.75 times longer, and the primes above 100 billion took 2.86 times longer.
;This is getting closer to (sqrt 10) times as long as the numbers increase and compatible with a O(sqrt n) prediction.
;The computation time is proportional to the steps required for the computation, 
;with some minor variations likely due to hardware, operating system or interpreter overheads and optimisations.