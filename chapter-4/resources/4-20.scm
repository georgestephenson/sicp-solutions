; exercise 4.20

; load evaluator code

(load "ch4-mceval.scm")

; leave the driver-loop commented out

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '= =)
        ))

(define the-global-environment (setup-environment))
(define-variable! '*unassigned* '*unassigned* the-global-environment)

; redefine eval

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp) 
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((let? exp) (eval (let->combination exp) env))
        ((letrec? exp) (eval (letrec->letset! exp) env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))

(define (let? exp) (tagged-list? exp 'let))
(define (letrec? exp) (tagged-list? exp 'letrec))

; implement let->combination

(define (let-vars bindings)
  (map car bindings))

(define (let-exps bindings)
  (map cadr bindings))

(define (let->combination exp)
  (let* ((bindings (cadr exp))
         (vars (let-vars bindings))
         (exps (let-exps bindings))
         (body (cddr exp)))
    (cons (cons 'lambda (cons vars body)) exps)))

; implement letrec->letset!

(define (binding->set! binding)
  (let ((v (car binding))
        (e (cadr binding)))
    (list 'set! v e)))

(define (letrec->letset! exp)
  (let* ((bindings (cadr exp))
         (vars (let-vars bindings))
         (body (cddr exp)))
    (cons 'let
          (cons (map (lambda (v) (list v ''*unassigned*)) vars)
                (append (map binding->set! bindings)
                        body)))))

; test

(letrec ((fact
          (lambda (n)
            (if (= n 1)
                1
                (* n (fact (- n 1)))))))
  (fact 10))
;Value: 3628800

(eval '(letrec ((fact
         (lambda (n)
           (if (= n 1)
               1
               (* n (fact (- n 1)))))))
           (fact 10))
      the-global-environment)
;Value: 3628800