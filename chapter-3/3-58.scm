; exercise 3.58

(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) den radix)))

; This procedure will compute the decimal digits 
; of dividing num by den in the specified radix/base.
; it only works if the result is >= 0 and < 1
; in other words it assumes num < den

(define (stream-take s n)
  (cond ((or (zero? n) (stream-null? s))
         '())
        (else
         (cons (stream-car s)
               (stream-take (stream-cdr s) (- n 1))))))

(stream-take (expand 1 7 10) 20)
;Value: (1 4 2 8 5 7 1 4 2 8 5 7 1 4 2 8 5 7 1 4)
; 0.142857 recurring

(stream-take (expand 3 8 10) 20)
;Value: (3 7 5 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
; 0.375 (further digits are 0)