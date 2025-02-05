
(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (average x y)
  (/ (+ x y) 2))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (- n 1)))))

(define (log2 x)
  (/ (log x) (log 2)))

(define (nth-root x n)
  (fixed-point-of-transform 
    (lambda (y) (/ x (expt y (- n 1)))) 
    (repeated average-damp (log2 n))
    1.0))

; Experiments:
; 8th root with 2 avg damps - doesn't converge
; 8th root with 3 avg damps - converges
; 16th root with 3 avg damps - doesn't converge
; 16th root with 4 avg damps - converges

; This suggests we need log base 2 (n) average damps