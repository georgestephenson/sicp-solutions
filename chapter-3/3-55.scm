; definition
(define (partial-sums S)
  (cons-stream (stream-car S)
               (add-streams (partial-sums S) (stream-cdr S))))

; dependencies
(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-streams ones integers)))

; test
(display-stream 
  (stream-filter (lambda (n) (< n 100)) (partial-sums integers)))
; 1
; 3
; 6
; 10
; 15
; 21
; 28
; 36
; 45
; 55
; 66
; 78
; 91