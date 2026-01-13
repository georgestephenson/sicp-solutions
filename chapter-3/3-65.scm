; exercise 3.65

(define (ln-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (ln-summands (+ n 1)))))

; dependencies

(define (partial-sums S)
  (cons-stream (stream-car S)
               (add-streams (partial-sums S) (stream-cdr S))))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (stream-take s n)
  (cond ((or (zero? n) (stream-null? s))
         '())
        (else
         (cons (stream-car s)
               (stream-take (stream-cdr s) (- n 1))))))

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1))    
        (s2 (stream-ref s 2)))
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

(define (make-tableau transform s)
  (cons-stream s
               (make-tableau transform
                             (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car
              (make-tableau transform s)))

; test

(define ln-stream
  (partial-sums (ln-summands 1)))

(stream-take ln-stream 10)
; 1. 
; .5 
; .8333333333333333
; .5833333333333333 
; .7833333333333332
; .6166666666666666
; .7595238095238095
; .6345238095238095
; .7456349206349207
; .6456349206349207

(stream-take (euler-transform ln-stream) 10)
; .7
; .6904761904761905
; .6944444444444444
; .6924242424242424
; .6935897435897436
; .6928571428571428
; .6933473389355742
; .6930033416875522
; .6932539682539683
; .6930657506744464

(stream-take (accelerated-sequence euler-transform
                                   ln-stream) 
             10)
; 1. 
; .7 
; .6932773109243697 
; .6931488693329254 
; .6931471960735491 
; .6931471806635636 
; .6931471805604039 
; .6931471805599445 
; .6931471805599427 
; .6931471805599454