(define (make-monitored f)
  (let ((counter 0))
    (define (mf x)
      (if (eq? x 'how-many-calls?)
          counter
          (begin (set! counter (+ counter 1))
                 (f x))))
  mf))

(define s (make-monitored sqrt))

(s 100)
;Value: 10

(s 'how-many-calls?)
;Value: 1