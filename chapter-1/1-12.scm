(define (triangle-element x y)
  (cond ((= x 1) 1)
        ((= x y) 1)
        (else (+ (triangle-element x (- y 1)) 
                 (triangle-element (- x 1) (- y 1))))))

