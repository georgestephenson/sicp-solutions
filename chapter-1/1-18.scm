(define (double a)
  (+ a a))

(define (halve a)
  (/ a 2))

(define (even? n)
  (= (remainder n 2) 0))

(define (fast-mult a b)
  (fast-mult-iter a b 0))

(define (fast-mult-iter a b sum)
  (cond ((= b 0) sum)
        ((even? b) (fast-mult-iter (double a) (halve b) sum))
        (else (fast-mult-iter a (- b 1) (+ sum a)))))
