; exercise 4.53

(load "resources/ch4-ambeval.scm")

; implement permanent-set! and if-fail

(define (analyze exp)
  (cond ((self-evaluating? exp) 
         (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((permanent-assignment? exp) (analyze-permanent-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((if-fail? exp) (analyze-if-fail exp))
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

(define (if-fail? exp) (tagged-list? exp 'if-fail))

(define (if-fail-predicate exp) (cadr exp))

(define (if-fail-failure exp) (caddr exp))

(define (analyze-if-fail exp)
  (let ((pproc (analyze (if-fail-predicate exp)))
        (fproc (analyze (if-fail-failure exp))))
    (lambda (env succeed fail)
      (pproc env
             (lambda (val fail2)
               (succeed val fail))
             (lambda ()
               (fproc env succeed fail))))))

; start driver-loop

(define the-global-environment (setup-environment))
(driver-loop)

; test as per exercise description

(define (require p)
  (if (not p) (amb)))

(define (an-element-of items)
  (require (not (null? items)))
  (amb (car items) (an-element-of (cdr items))))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (prime-sum-pair list1 list2)
  (let ((a (an-element-of list1))
        (b (an-element-of list2)))
    (require (prime? (+ a b)))
    (list a b)))

;;; Amb-Eval input:
(let ((pairs '()))
  (if-fail (let ((p (prime-sum-pair '(1 3 5 8) '(20 35 110))))
             (permanent-set! pairs (cons p pairs))
             (amb))
           pairs))
;;; Starting a new problem 
;;; Amb-Eval value:
;((8 35) (3 110) (3 20))

; like a list-returning implementation of prime-sum-pair, goes and gets every possible result for prime-sum-pair
; then once it runs out of results and fails, returns the pairs