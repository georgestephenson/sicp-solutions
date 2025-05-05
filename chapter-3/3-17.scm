; Correct version of count-pairs

(define (count-pairs x)
  (let ((counted-pairs '()))
    (define (count-pairs-recursive x)
      (if (or (not (pair? x))
              (memq x counted-pairs))
          0
          (begin (set! counted-pairs (cons x counted-pairs))
            (+ (count-pairs-recursive (car x))
               (count-pairs-recursive (cdr x))
               1))))
    (count-pairs-recursive x)))

; Using my examples from Exercise 3.17, the corrected version of count-pairs should now
; correctly identify that they all include three distinct pairs.

(define x (list 'a 'b 'c))

(count-pairs x)
;Value: 3

(define y1 (list 'a 'b))
(define y2 (cons y1 (cdr y1)))

(count-pairs y2)
;Value: 3

(define z1 (list 'a))
(define z2 (cons z1 z1))
(define z3 (cons z2 z2))

(count-pairs z3)
;Value: 3

(define n1 (list 'a))
(define n2 (list 'b))
(define n3 (list 'c))
(set-cdr! n1 n2)
(set-cdr! n2 n3)
(set-cdr! n3 n1)

(count-pairs n1)
;Value: 3