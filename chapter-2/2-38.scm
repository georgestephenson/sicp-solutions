(define nil '())

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(fold-right / 1 (list 1 2 3))
;Value: 3/2
(fold-left / 1 (list 1 2 3))
;Value: 1/6
(fold-right list nil (list 1 2 3))
;Value: (1 (2 (3 ())))
(fold-left list nil (list 1 2 3))
;Value: (((() 1) 2) 3)

; To guarantee fold-right and fold-left produce the same values,
; op should have the commutative property, like multiplication for example

(fold-right * 1 (list 1 2 3))
;Value: 6
(fold-left * 1 (list 1 2 3))
;Value: 6