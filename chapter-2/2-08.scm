

; Analogous to the addition of intervals and using arithmetic subtraction,
; The lower-bound and smallest difference is made by subtracting the upper-bound of y from the lower-bound of x
; The upper-bound or biggest difference will be subtraction lower-bound of y from the upper-bound of x

(load "2-07.scm")

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

(print-interval (sub-interval (make-interval 4 6) (make-interval 1 3)))
;(1,5)
(print-interval (sub-interval (make-interval 5 9) (make-interval 7 13)))
;(-8,2)
(print-interval (sub-interval (make-interval 17 88) (make-interval 2 17)))
;(0,86)