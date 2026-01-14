; exercise 3.67

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (interleave
      (stream-map (lambda (x) (list x (stream-car t)))
                (stream-cdr s))
      (pairs (stream-cdr s) (stream-cdr t))))))

; dependencies

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define ones (cons-stream 1 ones))

(define integers (cons-stream 1 (add-streams ones integers)))

(define (stream-take s n)
  (cond ((or (zero? n) (stream-null? s))
         '())
        (else
         (cons (stream-car s)
               (stream-take (stream-cdr s) (- n 1))))))

; test

(stream-take (pairs integers integers) 21)
;Value: ((1 1) (1 2) (2 1) (1 3)  (2 2) (1 4)  (3 1) 
;        (1 5) (2 3) (1 6) (4 1)  (1 7) (3 2)  (1 8) 
;        (5 1) (1 9) (2 4) (1 10) (6 1) (1 11) (3 3))

; although the order is messy, the above output at least
; has all the pairs between (1,1) and (3,3)