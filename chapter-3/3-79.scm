; exercise 3.79

(define (solve-2nd f dt y0 dy0)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (stream-map f dy y))
  y)

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

(stream-take (solve-2nd (lambda (dy y) 0) 1 1 2) 5)
;Value: (1 3 5 7 9)

(define cos-stream
  (solve-2nd (lambda (dy y) (- y)) 0.01 1 0))

(stream-take cos-stream 10)
;Value: (1 1 .9999 .9997 .99940001 .99900005 .998500149999 .997900349993 .9972006999720001 .9964012599160009)