; Procedure to detect cycles
; Note - this seems to work but is obviously not the most efficient 
; as it doesn't remember previously tried nodes

(define (contains-cycle? x)
  (define (cc first current)
    (if (not (pair? current))
        #f
        (let ((next (cdr current)))
          (if (eq? first next)
              #t
              (or (cc first next)
                  (cc next next))))))
  (cc x x))


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