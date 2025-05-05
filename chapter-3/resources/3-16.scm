(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

(define x (list 'a 'b 'c))

(count-pairs x)

(define y1 (list 'a 'b))
(define y2 (cons y1 (cdr y1)))

(count-pairs y2)

(define z1 (list 'a))
(define z2 (cons z1 z1))
(define z3 (cons z2 z2))

(count-pairs z3)

(define n1 (list 'a))
(define n2 (list 'b))
(define n3 (list 'c))
(set-cdr! n1 n2)
(set-cdr! n2 n3)
(set-cdr! n3 n1)

(count-pairs n1)