;; Exercise 3.37

(load "resources/section-3-3-5.scm")

(define (c+ x y)
  (let ((z (make-connector)))
    (adder x y z)
    z))

; x-y = z
; x = z+y
; y+z = x
(define (c- x y)
  (let ((z (make-connector)))
    (adder y z x)
    z))

(define (c* x y)
  (let ((z (make-connector)))
    (multiplier x y z)
    z))

; x/y = z
; x = z*y
; y*z = x
(define (c/ x y)
  (let ((z (make-connector)))
    (multiplier y z x)
    z))

(define (cv x)
  (let ((y (make-connector)))
    (constant x y)
    y))

; Test

(define (celsius-fahrenheit-converter x)
  (c+ (c* (c/ (cv 9) (cv 5))
          x)
      (cv 32)))

(define C (make-connector))
(define F (celsius-fahrenheit-converter C))

(probe "Celsius temp" C)
(probe "Fahrenheit temp" F)
(set-value! C 25 'user)
;Probe: Celsius temp = 25
;Probe: Fahrenheit temp = 77
(set-value! F 212 'user)
;Contradiction (77 212)
(forget-value! C 'user)
;Probe: Celsius temp = ?
;Probe: Fahrenheit temp = ?
(set-value! F 212 'user)
;Probe: Fahrenheit temp = 212
;Probe: Celsius temp = 100