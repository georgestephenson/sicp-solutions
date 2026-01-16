; exercise 3.80

(define (RLC R L C dt)
  (lambda (vC0 iL0)
    (define vC (integral (delay dvC) vC0 dt))
    (define iL (integral (delay diL) iL0 dt))
    (define dvC (scale-stream iL (/ -1 C)))
    (define diL (add-streams (scale-stream vC (/ 1 L))
                             (scale-stream iL (/ (- R) L))))
    (cons vC iL)))

; dependencies

(define (integral delayed-integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (let ((integrand (force delayed-integrand)))
                   (add-streams (scale-stream integrand dt)
                                int))))
  int)

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define (stream-take s n)
  (cond ((or (zero? n) (stream-null? s))
         '())
        (else
         (cons (stream-car s)
               (stream-take (stream-cdr s) (- n 1))))))

; test

(define RLC1 (RLC 1 1 0.2 0.1))

(define result (RLC1 10 0))
(define vC (car result))
(define iL (cdr result))

(stream-take vC 10)
;Value: (10 10 9.5 
; 8.55 7.220000000000001 5.5955 
; 3.77245 1.8519299999999999 -.0651605000000004 
; -1.8831384500000004)

(stream-take iL 10)
;Value: (0 1. 1.9 
; 2.66 3.249 3.6461 
; 3.84104 3.834181 3.6359559 
; 3.2658442599999997)