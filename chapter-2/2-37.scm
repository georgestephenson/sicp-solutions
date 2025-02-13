(load "2-36.scm")

(define m 
  (list (list 1 2 3 4) 
        (list 4 5 6 6) 
        (list 6 7 8 9)))

(define v (list 1 2 3 4))
(define w (list 9 8 7 6))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(dot-product v w)
;Value: 70

(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product x v)) m))

(matrix-*-vector m v)
;Value: (30 56 80)

(define (transpose mat)
  (accumulate-n cons nil mat))

(transpose m)
;Value: ((1 4 6) (2 5 7) (3 6 8) (4 6 9))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (v) (matrix-*-vector cols v)) m)))

(define n 
  (list (list 6 7 3 1) 
        (list 9 9 8 9) 
        (list 4 3 2 1)))

(matrix-*-matrix m n)