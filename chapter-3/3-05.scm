(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (estimate-integral P x1 x2 y1 y2 trials)
  (define (test)
    (P (random-in-range x1 x2) (random-in-range y1 y2)))
  (* (monte-carlo trials test)
     (* (abs (- x2 x1)) (abs (- y2 y1)))))

(define (in-unit-circle? x y)
  (<= (+ (square x) (square y)) 1))

(define (estimate-pi trials)
  (estimate-integral in-unit-circle? -1.0 1.0 -1.0 1.0 trials))

(estimate-pi 100000)
;Value: 3.1412