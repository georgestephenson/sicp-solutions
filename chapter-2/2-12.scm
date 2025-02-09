
(load "2-07.scm")

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (make-center-percent c p)
  (let ((w (* c p 0.01 )))
    (make-center-width c w)))

(define (percent i)
  (/ (* (width i) 100) (center i)))


; p. 93 "if you buy a resistor labeled "6.8 ohms with 10% tolerance"
;        you can only be sure that the resistor has a resistance
;        between 6.8 - 0.68 = 6.12 and 6.8+0.68 = 7.48 ohms"

(define resistor (make-center-percent 6.8 10))

(print-interval resistor)
;(6.12,7.4799999999999995)

(percent resistor)
;Value: 9.999999999999996