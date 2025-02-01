(define (average x y)
  (/ (+ x y) 2))

(define (improve guess x)
  (average guess (/ x guess)))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x)
                     x)))

;"The good-enough? test used in computing square roots will not be very effective finding the square roots of very small numbers"
;If the square of x is smaller than 0.001 then good-enough? will pass whenever the guess is less than (sqrt 0.001)

(sqrt 0.001)
;Value: .03162277660168379

(sqrt-iter 1.0 0.000000001)
;Value: .03125001065624928

;"Also, in real computers, arithmetic operations are almost always performed with limited precision. This makes our test inadequate for very large numbers"
;If our radicand is many digits long, the precision of the / operator may never give a precise enough guess to pass the good-enough? test

;(sqrt-iter 1.0 356842354891078375)
;Never finishes

;Alternative Strategy:

(define (good-enough? last-guess next-guess)
  (< (abs (- last-guess next-guess)) 0.000001))


(define (sqrt-iter last-guess guess x)
  (if (good-enough? last-guess guess)
          guess
          (sqrt-iter guess
                     (improve guess x)
                     x)))

(sqrt-iter 0.0 1.0 0.000000001)
;Value: 3.162278058889937e-5
;This result is much more accurate

(sqrt-iter 0.0 1.0 356842354891078375)
;Value: 597362833.5367696
;This result finishes and is accurate to 3dp