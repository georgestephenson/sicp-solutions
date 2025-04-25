(define (rand-update x)
  (let ((a 27) (b 26) (m 127))
    (modulo (+ (* a x) b) m)))

(define random-init 7)

(define rand
  (let ((x random-init))
    (lambda (m)
      (cond 
        ((eq? m 'generate)
          (set! x (rand-update x))
          x)
        ((eq? m 'reset)
          (lambda (new-value)
            (set! x new-value)
            x))
        (else (error "Unknown request -- RAND" m)))
      )))

((rand 'reset) 54)
;Value: 54
(rand 'generate)
;Value: 87
(rand 'generate)
;Value: 89
(rand 'generate)
;Value: 16

((rand 'reset) 54)
;Value: 54
(rand 'generate)
;Value: 87
(rand 'generate)
;Value: 89
(rand 'generate)
;Value: 16
