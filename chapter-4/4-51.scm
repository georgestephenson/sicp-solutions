; exercise 4.51

(load "resources/ch4-ambeval.scm")

; implement permanent-set!

(define (analyze exp)
  (cond ((self-evaluating? exp) 
         (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((permanent-assignment? exp) (analyze-permanent-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((let? exp) (analyze (let->combination exp))) ;**
        ((amb? exp) (analyze-amb exp))                ;**
        ((application? exp) (analyze-application exp))
        (else
         (error "Unknown expression type -- ANALYZE" exp))))

(define (permanent-assignment? exp)
  (tagged-list? exp 'permanent-set!))

(define (permanent-assignment-variable exp) (cadr exp))

(define (permanent-assignment-value exp) (caddr exp))

(define (analyze-permanent-assignment exp)
  (let ((var (permanent-assignment-variable exp))
        (vproc (analyze (permanent-assignment-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)
                 (set-variable-value! var val env)
                 (succeed 'ok fail2))
             fail))))

; start driver-loop

(define the-global-environment (setup-environment))
(driver-loop)

; test as per exercise description

(define (require p)
  (if (not p) (amb)))

(define (an-element-of items)
  (require (not (null? items)))
  (amb (car items) (an-element-of (cdr items))))

(define count 0)

;;; Amb-Eval input:
(let ((x (an-element-of '(a b c)))
      (y (an-element-of '(a b c))))
  (permanent-set! count (+ count 1))
  (require (not (eq? x y)))
  (list x y count))
;;; Starting a new problem 
;;; Amb-Eval value:
;(a b 2)

;;; Amb-Eval input:
try-again
;;; Amb-Eval value:
;(a c 3)


(define count 0)

; if we use set!

(let ((x (an-element-of '(a b c)))
      (y (an-element-of '(a b c))))
  (set! count (+ count 1))
  (require (not (eq? x y)))
  (list x y count))
;;; Starting a new problem 
;;; Amb-Eval value:
(a b 1)

;;; Amb-Eval input:
try-again
;;; Amb-Eval value:
(a c 1)