

(define zero (lambda (f) (lambda (x) x)))
; returns x ignoring f

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

;(add-1 zero)
;(lambda (f) (lambda (x) (f ((zero f) x))))
;(lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) x)) f) x))))
;(lambda (f) (lambda (x) (f ((lambda (x) x) x))))
;(lambda (f) (lambda (x) (f x)))

(define one (lambda (f) (lambda (x) (f x))))
; applies f to x once

(define two (lambda (f) (lambda (x) (f (f x)))))
; applies f to x twice

(define (add a b)
  (lambda (f) (lambda (x) ((a f) ((b f) x)))))
; apply b's function to x, apply a's function to the result of ((b f) x)

; if the f in add-1 increments x starting with zero, we can represent church numerals as integers
(define (church-to-number n)
  ((n (lambda (x) (+ x 1))) 0))

(church-to-number zero)
;Value: 0
(church-to-number one)
;Value: 1
(church-to-number two)
;Value: 2
(church-to-number (add-1 zero))
;Value: 1
(church-to-number (add zero one))
;Value: 1
(church-to-number (add one two))
;Value: 3