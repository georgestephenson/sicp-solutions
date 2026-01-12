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

;_________________________________________________________________

; part b

(define cosine-series
  (cons-stream 1 
               (integrate-series 
                 (scale-stream sine-series -1))))

(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))

; dependencies

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

; test

(stream-take cosine-series 10)
;Value: (1 0 -1/2 0 1/24 0 -1/720 0 1/40320 0)

(stream-take sine-series 10)
;Value: (0 1 0 -1/6 0 1/120 0 -1/5040 0 1/362880)

; These are the correct power series coefficients for 
; sine and cosine according to 
; https://mathcs.clarku.edu/~djoyce/trig/compute.html