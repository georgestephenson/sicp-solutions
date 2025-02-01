; recursive process
(define (f n)
  (if (< n 3)
      n
      (+ (f (- n 1))
         (* 2 (f (- n 2)))
         (* 3 (f (- n 3))))))

; iterative process
(define (f n)
  (define (iter n-1 n-2 n-3 term)
    (if (= n term)
        n-1
        (iter (+ n-1 (* 2 n-2) (* 3 n-3))
              n-1
              n-2
              (+ 1 term))))
  (if (< n 3)
      n
      (iter 2 1 0 2)))
