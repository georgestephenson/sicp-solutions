; exercise 3.74

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

(define sense-data
  (stream-map sin integers))

(define (make-zero-crossings input-stream last-value)
  (cons-stream
   (sign-change-detector (stream-car input-stream) last-value)
   (make-zero-crossings (stream-cdr input-stream)
                        (stream-car input-stream))))

(define zero-crossings (make-zero-crossings sense-data 0))

; sense data

(stream-take sense-data 12)
;Value: (.8414709848078965 .9092974268256817 .1411200080598672 
;        -.7568024953079282 -.9589242746631385 -.27941549819892586 
;        .6569865987187891 .9893582466233818 .4121184852417566 
;        -.5440211108893698 -.9999902065507035 -.5365729180004349)

; original version of zero-crossings

(stream-take zero-crossings 12)
;Value: (0 0 0 -1 0 0 1 0 0 -1 0 0)


; completed version of Eva Lu Ator's program

(define zero-crossings
  (stream-map sign-change-detector 
              sense-data
              (cons-stream 0 sense-data)))

(stream-take zero-crossings 12)
;Value: (0 0 0 -1 0 0 1 0 0 -1 0 0)