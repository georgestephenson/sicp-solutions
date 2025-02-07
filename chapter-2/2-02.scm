
(define (make-segment s e) (cons s e))
(define (start-segment x) (car x))
(define (end-segment x) (cdr x))

(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (average x y)
  (/ (+ x y) 2))

(define (midpoint-segment segment)
  (make-point 
    (average (x-point (start-segment segment))
             (x-point (end-segment segment)))
    (average (y-point (start-segment segment))
             (y-point (end-segment segment)))))

(define segment-to-try (make-segment (make-point 37 26) (make-point 89 92)))

(print-point (midpoint-segment segment-to-try))
;(63,59)