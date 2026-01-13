; exercise 3.62

(define (div-series s1 s2)
  (mul-series s1 (invert-unit-series s2)))

(define tangent-series 
  (div-series sine-series cosine-series))

; dependencies

(define (invert-unit-series power-series)
  (cons-stream 1
               (mul-series (scale-stream (stream-cdr power-series) -1)
                           (invert-unit-series power-series))))

(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1)
                  (stream-car s2))
               (add-streams (scale-stream (stream-cdr s2) 
                                          (stream-car s1))
                            (mul-series (stream-cdr s1) s2))))

(define (stream-take s n)
  (cond ((or (zero? n) (stream-null? s))
         '())
        (else
         (cons (stream-car s)
               (stream-take (stream-cdr s) (- n 1))))))

(define (integrate-series power-series)
  (stream-map / power-series integers))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define ones (cons-stream 1 ones))

(define integers (cons-stream 1 (add-streams ones integers)))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define cosine-series
  (cons-stream 1 
               (integrate-series 
                 (scale-stream sine-series -1))))

(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))

; test

(stream-take tangent-series 10)
;Value: (0 1 0 1/3 0 2/15 0 17/315 0 62/2835)

; this looks correct according to 
; https://proofwiki.org/wiki/Power_Series_Expansion_for_Tangent_Function