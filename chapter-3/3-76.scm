; exercise 3.76

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
;Value: (.1 -.1 .1 -.1 
;        .8414709848078965 .9092974268256817 .1411200080598672 -.7568024953079282 
;        -.9589242746631385 -.27941549819892586 .6569865987187891 .9893582466233818)


; Modular (and fixed) version of Louis's program

(define (smooth stream)
  (define (iter s last-value)
    (let ((avpt (/ (+ (stream-car s) last-value) 2)))
      (cons-stream avpt
                   (iter (stream-cdr s) (stream-car s)))))
  (iter (stream-cdr stream) (stream-car stream)))

(define (make-zero-crossings input-stream)
  (let ((smooth-stream (smooth input-stream)))
    (stream-map sign-change-detector 
                (stream-cdr smooth-stream) 
                smooth-stream)))

(define zero-crossings (make-zero-crossings sense-data))

(stream-take zero-crossings 20)
;Value: (0 0 0 0 0 -1 0 0 1 0 0 -1 0 0 0 1 0 0 -1 0)