; element-of-set? and adjoin-set still work as-is

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (define (iter set1 set2 tried-set)
    (cond ((or (null? set1) (null? set2)) '())
          ((element-of-set? (car set1) tried-set) (iter (cdr set1) set2 tried-set))
          ((element-of-set? (car set1) set2) (cons (car set1) (iter (cdr set1) set2 (cons (car set1) tried-set))))
          (else (iter (cdr set1) set2 (cons (car set1) tried-set)))))
  (iter set1 set2 '()))

(define (union-set set1 set2)
  (define (iter set1 set2 tried-set)
    (cond ((null? set1) set2)
          ((null? set2) set1)
          ((element-of-set? (car set1) tried-set) (iter (cdr set1) set2 tried-set))
          ((element-of-set? (car set1) set2) (iter (cdr set1) set2 (cons (car set1) tried-set)))
          (else (cons (car set1) (iter (cdr set1) set2 (cons (car set1) tried-set))))))
  (iter set1 set2 '()))

(union-set (list 3 4 5 5 4 3) (list 2 5 4 8 2 5 8 4))
;Value: (3 2 5 4 8 2 5 8 4)
(intersection-set (list 3 4 5 5 4 3) (list 2 5 4 8 2 5 8 4))
;Value: (4 5)
(adjoin-set 9 (list 2 5 4 8 2 5 8 4))
;Value: (9 2 5 4 8 2 5 8 4)

; The time complexity of element-of-set? will be O(M) where M is the length of the list, and M >= N the number of elements in the set.
; Adjoin-set will also be O(M).
; My versions of union-set and intersection-set cache's the previously tried set values in set2.
; That speeds up optimistic cases, but the worst case is still O(M^2) for two lists of size M.

; There are other solutions. Duplication is faster if you just do this
(define adjoin-set cons)  ; O(1)
(define union-set append) ; O(M)

; That may be a reason for duplication, but this will lead to M becoming increasingly large for later operations.