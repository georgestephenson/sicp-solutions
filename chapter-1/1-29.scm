
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (integral f a b n)
  (define h (/ (- b a) n))
  (define (y k) (+ a (* k h)))
  (define (next a) (+ a 2))
  (define (term a) 
    (+ (f (y a)) 
       (* 4 (f (y (+ a 1)))) 
       (f (y (+ a 2)))))
  (/ (* h (sum term 0 next (- n 2))) 
     3))

(integral cube 0 1 100)
;Value: 1/4
(integral cube 0 1 1000)
;Value: 1/4

(integral cube 0.0 1.0 100)
;Value: .2500000000000001
(integral cube 0.0 1.0 1000)
;Value: .2500000000000001