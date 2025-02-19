(load "resources/section-2-3-4.scm")

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge nodes)
  (define (next-tree nodes)
    (make-code-tree (car nodes) (cadr nodes)))
  (define (remaining-nodes nodes)
    (cddr nodes))
  (cond ((= (length nodes) 1) (car nodes))
        ((= (length nodes) 2) (next-tree nodes))
        (else 
          (successive-merge 
            (adjoin-set (next-tree nodes) (remaining-nodes nodes))))))

(generate-huffman-tree '((A 4) (B 2) (C 1) (D 1)))
;Value: ((leaf a 4) ((leaf b 2) ((leaf d 1) (leaf c 1) (d c) 2) (b d c) 4) (a b d c) 8)

(generate-huffman-tree '((A 8) (B 3) (C 1) (D 1) (E 1) (F 1) (G 1) (H 1)))
;Value: ((leaf a 8) ((((leaf h 1) (leaf g 1) (h g) 2) ((leaf f 1) (leaf e 1) (f e) 2) (h g f e) 4) (((leaf d 1) (leaf c 1) (d c) 2) (leaf b 3) (d c b) 5) (h g f e d c b) 9) (a h g f e d c b) 17)