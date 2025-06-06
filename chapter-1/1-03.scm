(define (square x) (* x x))
(define (f a b c)
        (cond ((and (> a b) (> b c)) (+ (square a) (square b)))
              ((and (> b a) (> a c)) (+ (square b) (square a)))
              (else (+ (square c) (square b)))))
