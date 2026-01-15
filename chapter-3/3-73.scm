; exercise 3.73

(define (RC R C dt)
  (lambda (i v0)
    (add-streams (scale-stream i R)
                 (integral (scale-stream i (/ 1 C)) 
                           v0 
                           dt))))

; dependencies

(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (add-streams (scale-stream integrand dt)
                              int)))
  int)

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define ones (cons-stream 1 ones))

(define (stream-take s n)
  (cond ((or (zero? n) (stream-null? s))
         '())
        (else
         (cons (stream-car s)
               (stream-take (stream-cdr s) (- n 1))))))

; test

(define RC1 (RC 5 1 0.5))
(define current-stream ones)
(define initial-voltage 1)

(stream-take (RC1 current-stream initial-voltage) 10)
;Value: (6 6.5 7. 7.5 8. 8.5 9. 9.5 10. 10.5)