(define (intersection-tree tree1 tree2)
  (list->tree (intersection-set (tree->list-2 tree1) (tree->list-2 tree2))))

(define (union-tree tree1 tree2)
  (list->tree (union-set (tree->list-2 tree1) (tree->list-2 tree2))))

(define tree1 (list->tree (list 1 4 6 7 9 11)))
(define tree2 (list->tree (list 2 4 5 8 9 12)))

(intersection-tree tree1 tree2)
;Value: (4 () (9 () ()))
(union-tree tree1 tree2)
;Value: (6 (2 (1 () ()) (4 () (5 () ()))) (9 (7 () (8 () ())) (11 () (12 () ()))))

; intersection-tree and union-tree are O(n) because the constituent procedures are also O(n) and are operated sequentially.
; A direct tree-based implementation of intersection and union may be more efficient than converting to and from ordered lists in this way.
; The overall time required will be multiplied by some constant x compared to the time taken for intersection-set and union-set.
; But, this approach still results in O(n).