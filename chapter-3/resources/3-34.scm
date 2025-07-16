;; Exercise 3.34

(load "section-3-3-5.scm")

(define (squarer a b)
  (multiplier a a b))

; test
(define A (make-connector))
(define B (make-connector))
(squarer A B)

(probe "Square root" A)
(probe "Square value" B)

(set-value! A 2 'user)
;Probe: Square root = 2
;Probe: Square value = 4

(forget-value! A 'user)
;Probe: Square root = ?
;Probe: Square value = ?

(set-value! B 9 'user)
;Probe: Square value = 9
;No update is given by the probe for the square root