; exercise 4.6

; load evaluator code

(load "resources/ch4-mceval.scm")

; leave the driver-loop commented out

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list '+ +)
        (list '- -)
        (list '= =)
        ))

(define the-global-environment (setup-environment))

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
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))

(define (let? exp) (tagged-list? exp 'let))

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

; test

(let ((x (+ 3 4)) (y (+ 1 2))) (+ x y))
;Value: 10

(eval '(let ((x (+ 3 4)) (y (+ 1 2))) (+ x y))
      the-global-environment)
;Value: 10