; exercise definitions

(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define factorials (cons-stream 1 (mul-streams integers factorials)))

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
  (stream-filter (lambda (n) (< n 1000)) factorials))
; 1
; 1
; 2
; 6
; 24
; 120
; 720