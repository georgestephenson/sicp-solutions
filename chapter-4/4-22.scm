; exercise 4.22

; load evaluator code

(load "resources/ch4-analyzingmceval.scm")

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

; redefine analyze

(define (analyze exp)
  (cond ((self-evaluating? exp) 
         (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((let? exp) (analyze (let->combination exp)))
        ((application? exp) (analyze-application exp))
        (else
         (error "Unknown expression type -- ANALYZE" exp))))

(define (let? exp) (tagged-list? exp 'let))

; implement let->combination (same as exercise 4.6)

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