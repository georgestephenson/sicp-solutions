; exercise 3.71

; dependencies

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

; ramanujan numbers

(define (sum-cubes pair)
  (+ (cube (car pair)) (cube (cadr pair))))

(define (detect-matches s f)
  (let ((f0 (f (stream-ref s 0)))
        (f1 (f (stream-ref s 1))))
    (if (= f0 f1)
        (cons-stream f0 (detect-matches (stream-cdr s) f))
        (detect-matches (stream-cdr s) f))))

(stream-take (detect-matches (weighted-pairs integers integers sum-cubes) sum-cubes) 6)
;Value: (1729 4104 13832 20683 32832 39312)