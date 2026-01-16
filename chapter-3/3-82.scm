; exercise 3.82

(define (monte-carlo experiment)
  (define (iter trials-failed trials-passed)
    (if (experiment)
      (let ((passed (+ trials-passed 1)))
        (cons-stream (/ passed (+ trials-failed passed))
                    (iter trials-failed passed)))
      (let ((failed (+ trials-failed 1)))
        (cons-stream (/ trials-passed (+ failed trials-passed))
                    (iter failed trials-passed)))))
  (iter 0 0))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (estimate-integral P x1 x2 y1 y2)
  (define (test)
    (P (random-in-range x1 x2) (random-in-range y1 y2)))
  (stream-map (lambda (x) (* x (* (abs (- x2 x1)) (abs (- y2 y1))))) 
              (monte-carlo test)))

(define (in-unit-circle? x y)
  (<= (+ (square x) (square y)) 1))

(define estimate-pi
  (estimate-integral in-unit-circle? -1.0 1.0 -1.0 1.0))

(stream-ref estimate-pi 100000)
;Value: 3.1412085879141207