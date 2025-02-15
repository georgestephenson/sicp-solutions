#lang sicp
(#%require sicp-pict)

(define (split t1 t2)
  (lambda (painter n)
  (if (= n 0)
      painter
      (let ((smaller ((split t1 t2) painter (- n 1))))
        (t1 painter (t2 smaller smaller))))))

(define right-split (split beside below))
(define up-split (split below beside))

;tests
(paint (up-split einstein 4))
(paint (right-split einstein 4))