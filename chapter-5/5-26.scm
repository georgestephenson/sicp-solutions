; exercise 5.26

(load "resources/load-eceval.scm")

(define the-global-environment (setup-environment))

(start eceval)

(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product)
              (+ counter 1))))
  (iter 1 1))

;;; EC-Eval input:
(factorial 5)
;(total-pushes = 204 maximum-depth = 10)
;;; EC-Eval value:
;120

;;; EC-Eval input:
(factorial 8)
;(total-pushes = 309 maximum-depth = 10)
;;; EC-Eval value:
;40320

;;; EC-Eval input:
(factorial 13)
;(total-pushes = 484 maximum-depth = 10)
;;; EC-Eval value:
;6227020800

;;; EC-Eval input:
(factorial 21)
;(total-pushes = 764 maximum-depth = 10)
;;; EC-Eval value:
;51090942171709440000

; part a - maximum-depth is always 10

;          5a + b = 204
;               b = 204 - 5a
;          8a + b = 309
; 8a + (204 - 5a) = 309
;              3a = 105
;               a = 35
;               b = 204 - 5*35
;               b = 29

; part b - number of push operations is 35n+29 for n >= 1