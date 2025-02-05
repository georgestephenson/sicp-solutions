


(define (cont-frac n d k x)
  (define (cont-frac-iter n d k result x)
    (if (< k 1)
        result
        (cont-frac-iter n d (- k 1) (/ (n k x) (- (d k) result)) x)))
  (cont-frac-iter n d k 0 x))

(define (n k x) 
  (if (= k 1) 
      x 
      (square x)))

(define (d k)
  (- (* 2 k) 1))

(define (tan-cf x k)
  (cont-frac n d k x))

(define pi 3.14159265359)

; tan(pi/4) = 1
(tan-cf (/ pi 4) 1000)
;Value: 1.0000000000001035
; Equal to 1 to 12dp

; tan(pi/3) = sqrt(3)
(tan-cf (/ pi 3) 1000)
;Value: 1.732050807569153
; Equal to sqrt(3) to 11dp