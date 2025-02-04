
(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (newline)
      (display ";next guess ")
      (display next)
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(fixed-point (lambda (x) (/ (log 1000) (log x)))
             2.0)
;next guess 9.965784284662087
;next guess 3.004472209841214
;next guess 6.279195757507157
;next guess 3.759850702401539
;next guess 5.215843784925895
;next guess 4.182207192401397
;next guess 4.8277650983445906
;next guess 4.387593384662677
;next guess 4.671250085763899
;next guess 4.481403616895052
;next guess 4.6053657460929
;next guess 4.5230849678718865
;next guess 4.577114682047341
;next guess 4.541382480151454
;next guess 4.564903245230833
;next guess 4.549372679303342
;next guess 4.559606491913287
;next guess 4.552853875788271
;next guess 4.557305529748263
;next guess 4.554369064436181
;next guess 4.556305311532999
;next guess 4.555028263573554
;next guess 4.555870396702851
;next guess 4.555315001192079
;next guess 4.5556812635433275
;next guess 4.555439715736846
;next guess 4.555599009998291
;next guess 4.555493957531389
;next guess 4.555563237292884
;next guess 4.555517548417651
;next guess 4.555547679306398
;next guess 4.555527808516254
;next guess 4.555540912917957
;next guess 4.555532270803653
;Value: 4.555532270803653

(define (average x y)
  (/ (+ x y) 2))

(fixed-point (lambda (x) (average x (/ (log 1000) (log x))))
             2.0)
;next guess 5.9828921423310435
;next guess 4.922168721308343
;next guess 4.628224318195455
;next guess 4.568346513136242
;next guess 4.5577305909237005
;next guess 4.555909809045131
;next guess 4.555599411610624
;next guess 4.5555465521473675
;next guess 4.555537551999825
;Value: 4.555537551999825

; The average damping requires fewer steps. 
; Without it, the guesses spend some iterations oscillating higher or lower than the correct value of x.
; The average damping converges in fewer iterations due to the reduced oscillations.