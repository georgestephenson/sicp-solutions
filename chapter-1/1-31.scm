
; Part A(i) - writing product procedure

(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))


; Part A(ii) - define factorial in terms of product procedure

(define (inc n) (+ n 1))

(define (factorial n)
  (product values 1 inc n))

(factorial 7)
;Value: 5040

(factorial 11)
;Value: 39916800


; Part A(iii) - use product to compute pi

; using n here to mean the number of iterations
; noticed the formula is repetitions of (k*(k+2))/(k+1)^2
; if we increment k by 2 each iteration

(define (pi-product n)
  (define (pi-term x)
    (/ (* x (+ x 2)) (square (+ x 1))))
  (define (pi-next x)
    (+ x 2))
  (* 4 (product pi-term 2.0 pi-next (* n 2))))

(pi-product 1000)
;Value: 3.1423773650938855

(pi-product 1000000)
;Value: 3.1415934389872975


; Part B - write product as a recursive process

(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(factorial 9)
;Value: 362880

(factorial 13)
;Value: 6227020800