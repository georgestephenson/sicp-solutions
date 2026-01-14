; exercise 3.70

(define (merge-weighted s1 s2 weight)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let* ((s1car (stream-car s1))
                (s2car (stream-car s2))
                (s1weight (weight s1car))
                (s2weight (weight s2car)))
           (if (< s1weight s2weight)
               (cons-stream s1car (merge-weighted (stream-cdr s1) s2 weight))
               (cons-stream s2car (merge-weighted s1 (stream-cdr s2) weight)))))))

(define (weighted-pairs s t weight)
  (cons-stream
   (list (stream-car s) (stream-car t))
  (merge-weighted
    (stream-map (lambda (x) (list (stream-car s) x))
              (stream-cdr t))
    (weighted-pairs (stream-cdr s) (stream-cdr t) weight)
    weight)))

; dependencies

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

; part a

(define (sum-pair pair)
  (+ (car pair) (cadr pair)))

(stream-take (weighted-pairs integers integers sum-pair) 12)
;(1 1)
;(1 2)
;(2 2)
;(1 3)
;(2 3)
;(1 4)
;(3 3)
;(2 4)
;(1 5)
;(3 4)
;(2 5)
;(1 6)

; part b

(define (sum235 pair)
  (let ((i (car pair))
        (j (cadr pair)))
    (+ (* 2 i) (* 3 j) (* 5 i j))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (notdiv235 x)
  (not (or (divides? x 2)
           (divides? x 3)
           (divides? x 5))))

(define (notdiv235pair pair)
  (let ((i (car pair))
        (j (cadr pair)))
    (and (notdiv235 i) (notdiv235 j))))

(stream-take 
  (stream-filter notdiv235pair
                 (weighted-pairs integers integers sum235)) 20)
;(4 4) 
;(4 6) 
;(4 7) 
;(4 8) 
;(6 6) 
;(4 9) 
;(4 10) 
;(6 7) 
;(4 11)
;(6 8) 
;(7 7) 
;(4 12)
;(4 13)
;(6 9) 
;(7 8) 
;(4 14) 
;(6 10) 
;(4 15) 
;(7 9) 
;(8 8)