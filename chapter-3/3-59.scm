; exercise 3.59
; part a

(define (integrate-series power-series)
  (stream-map / power-series integers))

; dependencies

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define ones (cons-stream 1 ones))

(define integers (cons-stream 1 (add-streams ones integers)))

(define (stream-take s n)
  (cond ((or (zero? n) (stream-null? s))
         '())
        (else
         (cons (stream-car s)
               (stream-take (stream-cdr s) (- n 1))))))

; test

(stream-take (integrate-series ones) 10)
;Value: (1 1/2 1/3 1/4 1/5 1/6 1/7 1/8 1/9 1/10)