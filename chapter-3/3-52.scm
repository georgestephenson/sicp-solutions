; prerequisites
(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))

; code to evaluate

(define sum 0)

sum
;Value: 0

(define (accum x)
  (set! sum (+ x sum))
  sum)

sum
;Value: 0

(define seq (stream-map accum (stream-enumerate-interval 1 20)))

sum
;Value: 1

(define y (stream-filter even? seq))

sum
;Value: 6

(define z (stream-filter (lambda (x) (= (remainder x 5) 0))
                         seq))

sum
;Value: 10

(stream-ref y 7)
;Value: 136

sum
;Value: 136

(display-stream z)
; 10
; 15
; 45
; 55
; 105
; 120
; 190
; 210
; ;Unspecified return value

sum
;Value: 210

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Explanation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; If delay didn't use memo-proc, then accum would be called repeatedly for
; the same numbers in stream-enumerate-interval, for example
; when we define y, the value of sum is 1 + 2 + 3 = 6,
;
; First element 1, already ran accum, returns 1, not even, keep going
; Second element 2, run accum, 1 + 2 = 3, not even, keep going
; Third element 3, run accum, 3 + 3 = 6, even, stop
; it stops at 6 because this is the first even result (1 and 1 + 2 = 3 are not even).
;
; if we didn't remember the previous result, we'd repeat accum on top of sum, which is already 1.
; then when we define y, we'd get 1 + 1 = 2 for the value of sum.
;
; First element 1, run accum, 1 + 1 = 2, even, stop
;
; This gets increasingly difficult to reason about from this point 
; as the side effects of accum continue to grow.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
