

(define nil '())

(define (square-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))

(square-tree 
  (list 1
        (list 2 (list 3 4) 5)
        (list 6 7)))
;Value: (1 (4 (9 16) 25) (36 49))

(define (square-tree tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree sub-tree)
             (square sub-tree)))
       tree))

(square-tree 
  (list 1
        (list 2 (list 3 4) 5)
        (list 6 7)))
;Value: (1 (4 (9 16) 25) (36 49))