
; Part A - verify car yields x for any objects x and y

(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

(define x (lambda () "my x value"))

x
;Value: #[compound-procedure 12 x]

; I know x has this address in memory (compound-procedure 12)

(car (cons x 0))
;Value: #[compound-procedure 12 x]

; car returned the same address valued object
; (not only an equivalent value, but the same object. for example if we used primitive numbers and returned an equal number, that might not work for every data object)
; so car yields x for any objects x and y

; However, the hint asks me to use the substitution model to verify this.

;(car (cons x y))
;((cons x y) (lambda (p q) p))
;((lambda (m) (m x y)) (lambda (p q) p))
;((lambda (p q) p) x y)
;x

; Part B - define cdr

(define (cdr z)
  (z (lambda (p q) q)))

(cdr (cons x 0))
;Value: 0