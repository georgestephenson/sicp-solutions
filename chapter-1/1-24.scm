(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (fast-prime? n 10000)
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

(search-for-primes 1000000000 1000000100)
;1000000007 *** .36
;1000000009 *** .32999999999999996
;1000000021 *** .32000000000000006

(search-for-primes 1000000000000 1000000000100)
;1000000000039 *** .45999999999999996
;1000000000061 *** .4700000000000002
;1000000000063 *** .46999999999999975

;I have timed primes near a billion and primes near a trillion
;And ran Fermat's time 10,000 times for each prime, to get
;measurable times.

;Given log(1000000000) = 9
;And log(1000000000000) = 12
;I would expect the Fermat test on primes near a billion to run 
;12/9 = 1.33 times faster than primes near a trillion.

(/ (+ .36 .32999999999999996 .32000000000000006) 3)
;Value: .33666666666666667
(/ (+ .45999999999999996 .4700000000000002 .46999999999999975) 3)
;Value: .4666666666666666

(/ .4666666666666666 .33666666666666667)
;Value: 1.386138613861386

;The observed difference is that primes near a billion test
;1.39 times faster than primes near a billion

;Here are a couple of possible reasons larger numbers could be slower than O(log n) suggests
; 1. "expmod" is written as a recursive process, larger numbers will need a larger memory stack (with time overheads)
; 2. "random" may be slower for larger numbers, as well as all integer arithmetic
; 3. Constant factors are hidden by big O notation anyway - there is not a large difference in this case.