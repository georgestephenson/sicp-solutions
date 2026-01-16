; exercise 3.81

(define (rand request last-value)
  (cond 
    ((and (pair? request) (eq? (car request) 'reset))
      (cdr request))
    ((eq? request 'generate) 
      (rand-update last-value))
    (else (error "Unknown request -- RAND" request))))

(define (random-numbers input-stream last-value)
  (let ((this-value (rand (stream-car input-stream) last-value)))
    (cons-stream this-value
                 (random-numbers (stream-cdr input-stream) this-value))))

; dependencies

(define (rand-update x)
  (let ((a 27) (b 26) (m 127))
    (modulo (+ (* a x) b) m)))
  
(define (stream-take s n)
  (cond ((or (zero? n) (stream-null? s))
         '())
        (else
         (cons (stream-car s)
               (stream-take (stream-cdr s) (- n 1))))))

; test

(define random-init 7)

(define input-stream
  (cons-stream (cons 'reset 54)
    (cons-stream 'generate
      (cons-stream 'generate
        (cons-stream 'generate
                     input-stream)))))

(stream-take (random-numbers input-stream random-init) 12)
;Value: (54 87 89 16 54 87 89 16 54 87 89 16)