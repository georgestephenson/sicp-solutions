(load "2-07.scm")

; 1. all neg then min=x2y2 max=x1y1
; 2. all pos then min=x1y1 max=x2y2
; 3. x1,x2 pos and y1,y2 neg min=x2y1 max=x1y2
; 4. y1,y2 pos and x1,x2 neg min=x1y2 max=x2y1
; 5. x1 neg, x2,y1,y2 pos min=x1y2 max=x2y2
; 6. y1 neg, y2,x1,x2 pos min=x2y1 max=x2y2
; 7. x1,x2,y1 neg, y2 pos min=x1y2 max=x1y1
; 8. y1,y2,x1 neg, x2 pos min=x2y1 max=x1y1
; 9. x1,y1 neg, x2,y2 pos min=(min x1y2 x2y1) max=(max x1y1 x2y2)

; this could do nested conditionals, but leaving like this for clarity

(define (mul-interval x y)
  (let ((x1 (lower-bound x))
        (x2 (upper-bound x))
        (y1 (lower-bound y))
        (y2 (upper-bound y)))
    (define (neg n) (< n 0))
    (define (pos n) (>= n 0))
    (define x1y1 (lambda () (* x1 y1)))
    (define x1y2 (lambda () (* x1 y2)))
    (define x2y1 (lambda () (* x2 y1)))
    (define x2y2 (lambda () (* x2 y2)))
    (cond ((and (neg x1) (neg x2) (neg y1) (neg y2)) (make-interval (x2y2) (x1y1)))
          ((and (pos x1) (pos x2) (pos y1) (pos y2)) (make-interval (x1y1) (x2y2)))
          ((and (pos x1) (pos x2) (neg y1) (neg y2)) (make-interval (x2y1) (x1y2)))
          ((and (neg x1) (neg x2) (pos y1) (pos y2)) (make-interval (x1y2) (x2y1)))
          ((and (neg x1) (pos x2) (pos y1) (pos y2)) (make-interval (x1y2) (x2y2)))
          ((and (pos x1) (neg x2) (pos y1) (pos y2)) (make-interval (x2y1) (x2y2)))
          ((and (neg x1) (neg x2) (neg y1) (pos y2)) (make-interval (x1y2) (x1y1)))
          ((and (neg x1) (pos x2) (neg y1) (neg y2)) (make-interval (x2y1) (x1y1)))
          (else (make-interval (min (x1y2) (x2y1)) (max (x1y1) (x2y2)))))))

;testing all cases

(print-interval (mul-interval (make-interval -34 -2) (make-interval -43 -6)))
;(12,1462)
(print-interval (mul-interval (make-interval 22 65) (make-interval 23 54)))
;(506,3510)
(print-interval (mul-interval (make-interval 3 7) (make-interval -9 -2)))
;(-63,-6)
(print-interval (mul-interval (make-interval -32 -8) (make-interval 4 32)))
;(-1024,-32)
(print-interval (mul-interval (make-interval -17 43) (make-interval 3 21)))
;(-357,903)
(print-interval (mul-interval (make-interval 23 88) (make-interval -3 99)))
;(-264,8712)
(print-interval (mul-interval (make-interval -55 -32) (make-interval -5 34)))
;(-1870,275)
(print-interval (mul-interval (make-interval -20 10) (make-interval -28 -7)))
;(-280,560)
(print-interval (mul-interval (make-interval -6 23) (make-interval -10 39)))
;-234,897)
