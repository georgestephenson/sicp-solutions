(load "2-12.scm")

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1))) 
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

(define (div-interval x y)
  (if (<= (lower-bound y) 0 (upper-bound y))
      (error "cannot divide by an interval spanning zero")
      (mul-interval x 
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))

(define tr1 (make-interval 4 8))
(define tr2 (make-interval 46 47.5))

(print-interval (par1 tr1 tr2))
;(3.315315315315315,7.6000000000000005)
(percent (par1 tr1 tr2))
;Value: 39.25387916804227

(print-interval (par2 tr1 tr2))
;(3.68,6.846846846846846)
(percent (par2 tr1 tr2))
;Value: 30.08352731754073

(define tr1 (make-interval 92 92.1))
(define tr2 (make-interval 65 65.25))

(print-interval (par1 tr1 tr2))
;(38.00444868128376,38.27722929936306)
(percent (par1 tr1 tr2))
;Value: .357596509804755

(print-interval (par2 tr1 tr2))
;(38.089171974522294,38.19208770257388)
(percent (par2 tr1 tr2))
;Value: .1349161360040907

; There is a considerable difference in the error between par1 and par2
; Smaller percentages vary greatly in proportion to each other.
; The error will be a big issue for computing small tolerances.