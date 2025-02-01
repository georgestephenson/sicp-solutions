(define (cube x) (* x x x))

(define count 0)

(define (p x) 
  (set! count (+ count 1))
  (- (* 3 x) (* 4 (cube x))))

(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)) )))

(sine 12.15)

count
;Value: 5

; a. The procedure p is applied five times when (sine 12.15) is evaluated.

; b. The (sine a) procedure divides the angle by 3 on each iteration k times until |a/(3^k)| <= 0.1

; This can be rewritten 
; |a/0.1| <= 3^k
; log(a) - log(0.1) <= k*log(3)
; 
; Meaning the number of steps k for (sine a) grows with O(log a)
; The space requirements for k steps is also k as k recursive procedures are kept in the stack
; So the space requirements also grows with O(log a)