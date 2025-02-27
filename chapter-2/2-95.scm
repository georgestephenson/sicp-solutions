(load "2-94.scm")

(define p1 (make-polynomial 'x '((2 1) (1 -2) (0 1))))
(define p2 (make-polynomial 'x '((2 11) (0 7))))
(define p3 (make-polynomial 'x '((1 13) (0 5))))

(define q1 (mul p1 p2))
(define q2 (mul p1 p3))

(greatest-common-divisor q1 q2)
;Value: (polynomial x (2 1458/169) (1 -2916/169) (0 1458/169))

; Tracing gcd-terms gives the following iterations:

; a ((4 11) (3 -22) (2 18) (1 -14) (0 7)) b ((3 13) (2 -21) (1 3) (0 5))
; a ((3 13) (2 -21) (1 3) (0 5)) b ((2 1458/169) (1 -2916/169) (0 1458/169))
; a ((2 1458/169) (1 -2916/169) (0 1458/169)) b ()

; The nature of the long division method means these different coefficients 
; are divided by each other to make new coefficients which will inevitably 
; lead to noninteger coefficients.

; But we can factor out 1458/169 to get:
; (1458/169)(x^2 - 2x + 1)

; which is p1 multiplied by a constant number.