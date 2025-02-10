

; nil now replaced in mit-scheme with '()
(define nil '())

(define (square-list items)
  (if (null? items)
      nil
      (cons (square (car items)) (square-list (cdr items)))))

(square-list (list 1 2 3 4))
;Value: (1 4 9 16)

(define (square-list items)
  (map (lambda (x) (square x)) items))

(square-list (list 1 2 3 4))
;Value: (1 4 9 16)