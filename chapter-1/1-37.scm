; Part A

(define (cont-frac n d k)
  (cont-frac-recurs n d k 1))

(define (cont-frac-recurs n d k a)
  (if (> a k)
      0
      (/ (n a) (+ (d a) (cont-frac-recurs n d k (+ a 1))))))

(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           1000)
;Value: .6180339887498948

; Programmatically check for four dp
(define (one-over-phi k)
  (cont-frac (lambda (i) 1.0)
             (lambda (i) 1.0)
             k))

(define (f-checker f k target tolerance)
  (let ((result (f k)))
       (if (< (abs (- target (f k))) tolerance)
           result
           (f-checker f (+ k 1) target tolerance))))

(f-checker one-over-phi 1 0.6180 0.00005)
;Value: 10


; Part B

(define (cont-frac n d k)
  (cont-frac-iter n d k 0))

(define (cont-frac-iter n d k result)
  (if (< k 1)
      result
      (cont-frac-iter n d (- k 1) (/ (n k) (+ (d k) result)))))

(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           1000)
;Value: .6180339887498948