; exercise 3.69

(define (triples s t u)
  (cons-stream
   (list (stream-car s) (stream-car t) (stream-car u))
   (interleave
     (interleave
       (stream-map (lambda (x) (list (stream-car s) (stream-car t) x))
                   (stream-cdr u))
       (stream-map (lambda (x) (list (stream-car s) (car x) (cadr x)))
                   (pairs (stream-cdr s) (stream-cdr t))))
     (triples (stream-cdr s) (stream-cdr t) (stream-cdr u)))))

(define (is-pythagorean triple)
  (let ((x (car triple))
        (y (cadr triple))
        (z (caddr triple)))
    (= (+ (square x) (square y)) (square z))))

(define (pythagorean-triples)
 (stream-filter is-pythagorean (triples integers integers integers)))

; dependencies

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

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

(stream-take (triples integers integers integers) 20)
;Value: ((1 1 1) (1 1 2) (2 2 2) (1 2 2) (2 2 3) 
;        (1 1 3) (3 3 3) (1 2 3) (2 3 3) (1 1 4) 
;        (3 3 4) (1 3 3) (2 2 4) (1 1 5) (4 4 4) 
;        (1 2 4) (2 3 4) (1 1 6) (3 4 4) (1 3 4))

(stream-take (pythagorean-triples) 5)
;Value: ((3 4 5) (6 8 10) (5 12 13) (9 12 15) (8 15 17))