; exercise 3.60

(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1)
                  (stream-car s2))
               (add-streams (scale-stream (stream-cdr s2) 
                                          (stream-car s1))
                            (mul-series (stream-cdr s1) s2))))

; dependencies

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

; sin^2(x) + cos^2(x) = 1

(define sin2addcos2
  (add-streams (mul-series sine-series sine-series)
               (mul-series cosine-series cosine-series)))

(stream-take sin2addcos2 20)
;Value: (1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)

; the series is equal to a constant of 1