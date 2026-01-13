; exercise 3.61

(define (invert-unit-series power-series)
  (cons-stream 1
               (mul-series (scale-stream (stream-cdr power-series) -1)
                           (invert-unit-series power-series))))

; dependencies

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

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define ones (cons-stream 1 ones))

; test

; 1/(1-x) = 1 + x + x^2 + x^3 ...
; so the inverse of this series is 1-x

(stream-take (invert-unit-series ones) 20)
;Value: (1 -1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)