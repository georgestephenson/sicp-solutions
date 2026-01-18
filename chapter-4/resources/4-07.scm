; exercise 4.7

; load evaluator code

(load "ch4-mceval.scm")

; leave the driver-loop commented out

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list '+ +)
        (list '* *)
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
        ((let*? exp) (eval (let*->nested-lets exp) env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))

(define (let? exp) (tagged-list? exp 'let))
(define (let*? exp) (tagged-list? exp 'let*))

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

; implement let*->nested-lets

(define (let*->nested-lets exp)
  (define (nest bindings body)
    (if (null? bindings) 
        body
        (cons 'let 
              (cons (list (car bindings)) 
                    (list (nest (cdr bindings) body))))))
  (let ((bindings (cadr exp))
        (body (caddr exp)))
    (nest bindings body)))

; test

(let* ((x 3)
       (y (+ x 2))
       (z (+ x y 5)))
  (* x z))
;Value: 39

(eval '(let* ((x 3)
              (y (+ x 2))
              (z (+ x y 5)))
         (* x z))
      the-global-environment)
;Value: 39