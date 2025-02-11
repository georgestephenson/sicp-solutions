
(define nil '())
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

; define map
(define (map p sequence)
  (accumulate (lambda (x y) (append (list (p x)) y)) nil sequence))

(define (square-list items)
  (map (lambda (x) (square x)) items))
(square-list (list 1 2 3 4))
;Value: (1 4 9 16)


(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(append (list 1 2) (list 3 4))
;Value: (1 2 3 4)


(define (length sequence)
  (accumulate (lambda (x y) (+ y 1)) 0 sequence))

(length (list 1 2 3 4))
;Value: 4