#lang sicp
(#%require sicp-pict)

(paint (below einstein mark-of-zorro))

; Part A - below analogous to beside

(define (below-a painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-bottom
           (transform-painter painter1
                              (make-vect 0.0 0.0)
                              (make-vect 1.0 0.0)
                              split-point))
          (paint-top
           (transform-painter painter2
                              split-point
                              (make-vect 1.0 0.5)
                              (make-vect 0.0 1.0))))
      (lambda (frame)
        (paint-bottom frame)
        (paint-top frame)))))

(paint (below-a einstein mark-of-zorro))

; Part B - below in terms of beside and rotation operations

(define (below-b painter1 painter2)
  (rotate90 (beside (rotate270 painter1) (rotate270 painter2))))

(paint (below-b einstein mark-of-zorro))