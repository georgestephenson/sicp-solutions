; exercise 4.54

(load "resources/ch4-ambeval.scm")

; implement require

(define (analyze exp)
  (cond ((self-evaluating? exp) 
         (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((require? exp) (analyze-require exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((let? exp) (analyze (let->combination exp))) ;**
        ((amb? exp) (analyze-amb exp))                ;**
        ((application? exp) (analyze-application exp))
        (else
         (error "Unknown expression type -- ANALYZE" exp))))

(define (require? exp) 
  (tagged-list? exp 'require))

(define (require-predicate exp) 
  (cadr exp))

(define (analyze-require exp)
  (let ((pproc (analyze 
                (require-predicate exp))))
    (lambda (env succeed fail)
      (pproc env
             (lambda (pred-value fail2)
               (if (not pred-value)
                   (fail2)
                   (succeed 'ok fail2)))
             fail))))

; start driver-loop

(define the-global-environment (setup-environment))
(driver-loop)

; test 

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
;(8 15 17)

;;; Amb-Eval input:
try-again
;;; Amb-Eval value:
;(9 12 15)

;;; Amb-Eval input:
try-again
;;; Amb-Eval value:
;(12 16 20)

;;; Amb-Eval input:
try-again
;;; Amb-Eval value:
;(15 20 25)

;;; Amb-Eval input:
try-again
;;; There are no more values of
;(a-pythagorean-triple-between 8 25)