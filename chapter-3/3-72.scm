; exercise 3.72

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

; stream of all numbers that can be written as
; the sum of two squares in three different ways

(define (sum-squares pair)
  (+ (square (car pair)) (square (cadr pair))))

(define (string-sum-square-pair pair)
  (string-append (number->string (car pair)) "^2 + " (number->string (cadr pair)) "^2 = " (number->string (sum-squares pair))))

(define (detect-matches s f)
  (let* ((s0 (stream-ref s 0))
         (s1 (stream-ref s 1))
         (s2 (stream-ref s 2))
         (f0 (f s0))
         (f1 (f s1))
         (f2 (f s2)))
    (if (= f0 f1 f2)
        (cons-stream (string-append (string-sum-square-pair s0) ", " (string-sum-square-pair s1) ", " (string-sum-square-pair s2)) 
                     (detect-matches (stream-cdr s) f))
        (detect-matches (stream-cdr s) f))))

(stream-take (detect-matches (weighted-pairs integers integers sum-squares) sum-squares) 6)
; "10^2 + 15^2 = 325, 6^2 + 17^2 = 325, 1^2 + 18^2 = 325" 
; "13^2 + 16^2 = 425, 8^2 + 19^2 = 425, 5^2 + 20^2 = 425" 
; "17^2 + 19^2 = 650, 11^2 + 23^2 = 650, 5^2 + 25^2 = 650" 
; "14^2 + 23^2 = 725, 10^2 + 25^2 = 725, 7^2 + 26^2 = 725" 
; "19^2 + 22^2 = 845, 13^2 + 26^2 = 845, 2^2 + 29^2 = 845" 
; "15^2 + 25^2 = 850, 11^2 + 27^2 = 850, 3^2 + 29^2 = 850"