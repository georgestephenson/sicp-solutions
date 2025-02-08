
(define (make-segment s e) (cons s e))
(define (start-segment x) (car x))
(define (end-segment x) (cdr x))

(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (make-rectangle top-segment height) (cons top-segment height))
(define (top-segment x) (car x))
(define (height x) (cdr x))

(define (xy-segment-length s xory-point)
  (abs 
    (- (xory-point (start-segment s))
       (xory-point (end-segment s)))))

(define (segment-length s)
  (sqrt (+ (square (xy-segment-length s x-point))
           (square (xy-segment-length s y-point)))))

(define (rectangle-area r)
  (* (segment-length(top-segment r))
     (height r)))

(define (rectangle-perimeter r)
  (+ (* 2 (segment-length(top-segment r)))
     (* 2 (height r))))

(define my-rectangle (make-rectangle
  (make-segment (make-point 4 3) (make-point 87 9))
  72))

(rectangle-area my-rectangle)
;Value: 5991.594111753566
(rectangle-perimeter my-rectangle)
;Value: 310.4331697709324

(define (make-rectangle topleft-point bottomright-point) (cons topleft-point bottomright-point))
(define (topleft-point x) (car x))
(define (bottomright-point x) (cdr x))

; The previous selectors can be represented in terms of the new selectors as an abstraction barrier
(define (top-segment r) 
  (make-segment 
    (topleft-point r)
    (make-point (x-point (bottomright-point r))
                (y-point (topleft-point r)))))
(define (height r) 
  (segment-length 
    (make-segment
      (topleft-point r)
      (make-point (x-point (topleft-point r))
                  (y-point (bottomright-point r))))))

; note - not the same rectangle as previously, would have to compute what the bottomright-point should be
; but we can show rectangle-area and rectangle-perimeter do not need to be reimplemented
(define my-rectangle2 
  (make-rectangle 
    (make-point 4 3) 
    (make-point 87 94)))

(rectangle-area my-rectangle2)
;Value: 7553
(rectangle-perimeter my-rectangle2)
;Value: 348