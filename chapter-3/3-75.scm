; exercise 3.75

; definition of unspecified procedure: sign-change-detector

(define (sign-change-detector next last)
  (cond 
    ((and (> next 0) (< last 0)) 1)
    ((and (< next 0) (> last 0)) -1)
    (else 0)))

; dependencies

(define (stream-take s n)
  (cond ((or (zero? n) (stream-null? s))
         '())
        (else
         (cons (stream-car s)
               (stream-take (stream-cdr s) (- n 1))))))

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define ones (cons-stream 1 ones))

(define integers (cons-stream 1 (add-streams ones integers)))

; mock a noisy start to a sine signal
(define sense-data
  (cons-stream 0.1
    (cons-stream -0.1
      (cons-stream 0.1
        (cons-stream -0.1
          (stream-map sin integers))))))

; sense data

(stream-take sense-data 12)

; Louis Reasoner's version

(define (make-zero-crossings input-stream last-value)
  (let ((avpt (/ (+ (stream-car input-stream) last-value) 2)))
    (cons-stream (sign-change-detector avpt last-value)
                 (make-zero-crossings (stream-cdr input-stream)
                                      avpt))))

(define zero-crossings (make-zero-crossings sense-data 0))

(stream-take zero-crossings 20)
;Value: (0 -1 1 -1 1 0 0 -1 0 0 1 0 0 -1 0 0 0 1 0 0)

; Alyssa's plan was to always average each value of the sense data with the last value.
; But Louis's version averages each value with the previous averaged value (not the actual last value).
; We need an extra argument to remember the last value, but also remember the last average,
; so we can compare the signs of the averaged values and correctly implement a moving average.

; Fixed version

(define (make-zero-crossings input-stream last-value last-avpt)
  (let ((avpt (/ (+ (stream-car input-stream) last-value) 2)))
    (cons-stream (sign-change-detector avpt last-avpt)
                 (make-zero-crossings (stream-cdr input-stream)
                                      (stream-car input-stream)
                                      avpt))))

(define zero-crossings (make-zero-crossings sense-data 0 0))

(stream-take zero-crossings 20)
;Value: (0 0 0 0 0 0 0 -1 0 0 1 0 0 -1 0 0 0 1 0 0)