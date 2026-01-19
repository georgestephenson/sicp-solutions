(define x 0)

(let while ()
  (if (< x 10)
      (begin
        (begin
          (newline)
          (display x)
          (set! x (+ x 1)))
        (while))))

(let ((i 10))
  (let loop ()
    (if ((lambda (x) (> x 0)) i)
        (begin
          (begin
            (newline)
            (display i))
          (set! i ((lambda (x) (- x 1)) i))
          (loop)))))