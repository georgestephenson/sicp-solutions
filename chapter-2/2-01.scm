(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (make-rat n d)
  (if (< d 0)
      (make-rat (- n) (- d))
      (let ((g (gcd (abs n) (abs d))))
        (cons (/ n g) (/ d g)))))
