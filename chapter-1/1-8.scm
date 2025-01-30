(define (improve guess x)
  (/ (+ (/ x (square guess))
        (* 2 guess))
     3))

(define (good-enough? last-guess next-guess)
  (< (abs (- last-guess next-guess)) 0.000001))


(define (cbrt-iter last-guess guess x)
  (if (good-enough? last-guess guess)
          guess
          (cbrt-iter guess
                     (improve guess x)
                     x)))

(define (cbrt x)
  (cbrt-iter 0.0 1.0 x))