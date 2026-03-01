; exercise 5.27

(load "resources/load-eceval.scm")

(define the-global-environment (setup-environment))

(start eceval)

(define (factorial n)
  (if (= n 1)
      1
      (* (factorial (- n 1)) n)))

;;; EC-Eval input:
(factorial 5)
;(total-pushes = 144 maximum-depth = 28)
;;; EC-Eval value:
;120

;;; EC-Eval input:
(factorial 8)
;(total-pushes = 240 maximum-depth = 43)
;;; EC-Eval value:
;40320

;;; EC-Eval input:
(factorial 13)
;(total-pushes = 400 maximum-depth = 68)
;;; EC-Eval value:
;6227020800

;;; EC-Eval input:
(factorial 21)
;(total-pushes = 656 maximum-depth = 108)
;;; EC-Eval value:
;51090942171709440000

; recursive factorial - maximum depth
; 5n + 3

; recursive factorial - total pushes
; 32n - 16

; iterative factorial - maximum depth (from exercise 5.26)
; 10

; iterative factorial - total pushes (from exercise 5.26)
; 35n + 29


;           | maximum depth | number of pushes
; --------------------------+-----------------
; recursive | 5n + 3        | 32n - 16
; factorial |               |
; --------------------------+-----------------
; iterative | 10            | 35n + 29
; factorial |               |