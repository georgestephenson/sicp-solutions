
(define nil '())

(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s)))
            (append-this (lambda (x) (cons (car s) x))))
        (append rest (map append-this rest)))))

(subsets (list 1 2 3))
;Value: (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))

; rest will be the set of all sets excluding the value (car s)
; (map append-this rest) will be the set of all sets including the value (car s)
; therefore appending them together produces the set of all sets