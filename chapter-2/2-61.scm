(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((= x (car set)) set)
        ((< x (car set)) (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set))))))

(adjoin-set 8 (list 1 5 11 15))
;Value: (1 5 8 11 15)

; Worst case still O(n), but equal chance we stop at any point in the set iteration.
; So about n/2 steps on average.