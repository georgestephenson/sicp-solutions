


(define (iterative-improve good-enough? improve)
  (lambda (guess) 
    (if (good-enough? guess)
        guess
        ((iterative-improve good-enough? improve) (improve guess)))))

(define (average x y)
  (/ (+ x y) 2))

(define (sqrt n)
  (define (good-enough? guess)
    (< (abs (- (square guess) n)) 0.001))
  (define (improve guess)
    (average guess (/ n guess)))
  ((iterative-improve good-enough? improve) n))

(sqrt 64.0)
;Value: 8.000001655289593
(sqrt 144.0)
;Value: 12.000000012408687


(define (fixed-point f first-guess)
  (define tolerance 0.00001)
  (define (close-enough? guess)
    (< (abs (- guess (f guess))) tolerance))
  ((iterative-improve close-enough? f) first-guess))

(fixed-point cos 1.0)
;Value: .7390893414033927