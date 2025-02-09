(load "2-07.scm")

; the procedure div-interval will divide by 0 if one of the bounds of y is 0 which is undefined
; logically if an interval spans 0, implicitly some point of the interval is dividing x by 0.
; although we don't compute the division on intermediate points of the interval, 
; the resulting interval is an abstraction that must include this point's result in its range.

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (if (<= (lower-bound y) 0 (upper-bound y))
      (error "cannot divide by an interval spanning zero")
      (mul-interval x 
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))

(print-interval (div-interval (make-interval 5 74) (make-interval 1 8)))
;(.625,74.)
(print-interval (div-interval (make-interval 12 49) (make-interval -4 3)))
;cannot divide by an interval spanning zero