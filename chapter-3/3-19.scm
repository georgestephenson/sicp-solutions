; Had to look up this one - the clever idea is Floyd's "tortoise and hare" algorithm.
; -- The hare traverses the list twice as fast as the tortoise.
; -- If there's any cycle, the tortoise eventually "catches up" with the hare
;    after the hare cycles back around behind the tortoise.
; -- Without cycles, it would be impossible for the tortoise to ever catch up.

(define (contains-cycle? list)
  (define (loop tortoise hare)
    (cond
      ((not (pair? tortoise)) #f)
      ((not (pair? hare)) #f)
      ((not (pair? (cdr hare))) #f)
      ((eq? tortoise hare) #t)
      (else
        (loop (cdr tortoise)
              (cdr (cdr hare))))))
  (loop list 
        (if (pair? list) (cdr list) '())))

; Testing procedure

(define x (list 'a 'b 'c))

(contains-cycle? x)
;Value: #f

(define y1 (list 'a 'b))
(define y2 (cons y1 (cdr y1)))

(contains-cycle? y2)
;Value: #f

(define z1 (list 'a))
(define z2 (cons z1 z1))
(define z3 (cons z2 z2))

(contains-cycle? z3)
;Value: #f

(define n1 (list 'a))
(define n2 (list 'b))
(define n3 (list 'c))
(set-cdr! n1 n2)
(set-cdr! n2 n3)
(set-cdr! n3 n1)

(contains-cycle? n1)
;Value: #t