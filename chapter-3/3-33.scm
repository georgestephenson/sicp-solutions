;; Exercise 3.33

(load "resources/section-3-3-5.scm")

; averager procedure definition
(define (averager a b c)
  (let ((x (make-connector))
        (y (make-connector)))
    (adder a b x)
    (constant 0.5 y)
    (multiplier x y c)
    'ok))

; test
(define A (make-connector))
(define B (make-connector))
(define C (make-connector))
(averager A B C)

(probe "Average value" C)

(set-value! A 249 'user)
(set-value! B 751 'user)
;Probe: Average value = 500.