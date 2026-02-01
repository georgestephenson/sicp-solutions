; exercise 4.35

(load "resources/ch4-ambeval.scm")

(define the-global-environment (setup-environment))
(driver-loop)

(define (require p)
  (if (not p) (amb)))

(define (an-integer-between low high)
  (require (<= low high))
  (amb low (an-integer-between (+ low 1) high)))

(define (a-pythagorean-triple-between low high)
  (let ((i (an-integer-between low high)))
    (let ((j (an-integer-between i high)))
      (let ((k (an-integer-between j high)))
        (require (= (+ (* i i) (* j j)) (* k k)))
        (list i j k)))))

;;; Amb-Eval input:
(a-pythagorean-triple-between 8 25)
;;; Starting a new problem 
;;; Amb-Eval value:
(8 15 17)

;;; Amb-Eval input:
try-again
;;; Amb-Eval value:
(9 12 15)

;;; Amb-Eval input:
try-again
;;; Amb-Eval value:
(12 16 20)

;;; Amb-Eval input:
try-again
;;; Amb-Eval value:
(15 20 25)

;;; Amb-Eval input:
try-again
;;; There are no more values of
(a-pythagorean-triple-between 8 25)
