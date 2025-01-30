(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(define (average x y)
  (/ (+ x y) 2))

(define (improve guess x)
  (average guess (/ x guess)))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x)
                     x)))

(sqrt-iter 1.0 4)
;Aborting!: maximum recursion depth exceeded

; Maximum recursion depth was exceeded because the recursion of sqrt-iter will continue forever
; Generally the applicative-order interpreter will want to fully evaluate the argument else-clause which will recursively call sqrt-iter forever
; The if expression is a special case that won't evaluate the else-clause unless the predicate is false.