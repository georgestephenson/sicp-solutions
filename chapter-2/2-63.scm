
; prerequisites

(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set) 
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

(define (make-leaf entry) (make-tree entry '() '()))

(define tree1
  (make-tree 7
             (make-tree 3 
                        (make-leaf 1)
                        (make-leaf 5))
             (make-tree 9
                        '()
                        (make-leaf 11))))

(define tree2
  (make-tree 3
             (make-leaf 1)
             (make-tree 7
                        (make-leaf 5)
                        (make-tree 9
                                   '()
                                   (make-leaf 11)))))

(define tree3
  (make-tree 5
             (make-tree 3 
                        (make-leaf 1)
                        '())
             (make-tree 9
                        (make-leaf 7)
                        (make-leaf 11))))

; part a

(tree->list-1 tree1)
;Value: (1 3 5 7 9 11)
(tree->list-2 tree1)
;Value: (1 3 5 7 9 11)
(tree->list-1 tree2)
;Value: (1 3 5 7 9 11)
(tree->list-2 tree2)
;Value: (1 3 5 7 9 11)
(tree->list-1 tree3)
;Value: (1 3 5 7 9 11)
(tree->list-2 tree3)
;Value: (1 3 5 7 9 11)

; both procedures will return a list that begins with the leftmost leaf node of the tree, and ends with the rightmost.
; so they produce the same ordered result for a given tree.

; part b

; Both operations must visit every node, which suggests order of growth O(n) or greater.

; For tree->list-1, use of append is expensive. We must traverse the left sublist for every n nodes.
; The append operation will be O(log n) on average for a balanced tree, as the left sublist halves in size at each level.
; So we perform O(log n) of work at each of the n nodes, giving O(n log n) overall.

; For tree->list-2, we cache the result with cons, which is cheaper, O(1).
; We iterate this way for every node which gives O(n).