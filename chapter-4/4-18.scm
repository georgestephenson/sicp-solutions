; exercise 4.18

(define (integral delayed-integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (let ((integrand (force delayed-integrand)))
                   (add-streams (scale-stream integrand dt)
                                int))))
  int)

(define (add-streams s1 s2)
  (stream-map + s1 s2))
  
(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

; Version A: original version

(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

(stream-ref (solve (lambda (y) y) 1 0.001) 1000)
;Value: 2.716923932235896

; Version B: text version of internal definitions scanned out

(define (solve f y0 dt)
  (let ((y '*unassigned*)
        (dy '*unassigned*))
    (set! y (integral (delay dy) y0 dt))
    (set! dy (stream-map f y))
    y))

(stream-ref (solve (lambda (y) y) 1 0.001) 1000)
;Value: 2.716923932235896

; Version C: Exercise 4.18 version of internal definitions scanned out

(define (solve f y0 dt)
  (let ((y '*unassigned*)
        (dy '*unassigned*))
    (let ((a (integral (delay dy) y0 dt))
          (b (stream-map f y)))
      (set! y a)
      (set! dy b)
      y)))

(stream-ref (solve (lambda (y) y) 1 0.001) 1000)
;Unknown arguments to STREAM-MAP.


; In Version B, the order of the set! operations matters.
; dy can be set (map on first value of y) because an initial 
; value of y is already known (first value of the integral).

; In Version C, the values of a and b are first evaluated.
; The value of y is unassigned when it is evaluated as part of b.
; So stream-map returns an error that there are unknown arguments.s