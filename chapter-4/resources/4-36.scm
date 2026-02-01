; exercise 4.36

(load "resources/ch4-ambeval.scm")

(define the-global-environment (setup-environment))
(driver-loop)

(define (require p)
  (if (not p) (amb)))

(define (an-integer-starting-from n)
  (amb n (an-integer-starting-from (+ n 1))))

(define (an-integer-between low high)
  (require (<= low high))
  (amb low (an-integer-between (+ low 1) high)))

(define (a-pythagorean-triple)
  (let ((k (an-integer-starting-from 1)))
    (let ((i (an-integer-between 1 k)))
      (let ((j (an-integer-between i k)))
        (require (= (+ (* i i) (* j j)) (* k k)))
        (list i j k)))))

;;; Amb-Eval input:
(a-pythagorean-triple)
;;; Starting a new problem 
;;; Amb-Eval value:
;(3 4 5)

;;; Amb-Eval input:
try-again
;;; Amb-Eval value:
;(6 8 10)

;;; Amb-Eval input:
try-again
;;; Amb-Eval value:
;(5 12 13)

;;; Amb-Eval input:
try-again
;;; Amb-Eval value:
;(9 12 15)