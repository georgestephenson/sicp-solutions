(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else 
    (let ((x1 (car set1)) (x2 (car set2)))
    (cond ((= x1 x2)
           (cons x1
                 (union-set (cdr set1)
                            (cdr set2))))
          ((< x1 x2) (cons x1 (union-set (cdr set1) set2)))
          ((< x2 x1) (cons x2 (union-set set1 (cdr set2)))))))))

(union-set (list 3 4 5) (list 1 2 4 6 9))
;Value: (1 2 3 4 5 6 9)
(union-set (list 0 5 7 8 9) (list 1 7 9 10))
;Value: (0 1 5 7 8 9 10)

; Like intersection-set, number of steps is at most sum of sizes of set1 and set2.
; However intersection-set easily benefitted from tail recursion whereas
; most cases of union-set involve recursive cons-ing.
; (an iterative version could cache the result set as an argument)