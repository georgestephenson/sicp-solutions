; exercise 4.28
; load evaluator code

(load "resources/ch4-leval.scm")

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list '+ +)
        (list '- -)
        ))

; removing actual-value from eval

; (define (eval exp env)
;   (cond ((self-evaluating? exp) exp)
;         ((variable? exp) (lookup-variable-value exp env))
;         ((quoted? exp) (text-of-quotation exp))
;         ((assignment? exp) (eval-assignment exp env))
;         ((definition? exp) (eval-definition exp env))
;         ((if? exp) (eval-if exp env))
;         ((lambda? exp)
;          (make-procedure (lambda-parameters exp)
;                          (lambda-body exp)
;                          env))
;         ((begin? exp) 
;          (eval-sequence (begin-actions exp) env))
;         ((cond? exp) (eval (cond->if exp) env))
;         ((application? exp)             ; clause from book
;          (apply (eval (operator exp) env)
;                 (operands exp)
;                 env))
;         (else
;          (error "Unknown expression type -- EVAL" exp))))

(define the-global-environment (setup-environment))
(driver-loop)

; example where forcing the value of the operator is required

(define (add3 n) (+ n 3))
(define (minus7 n) (- n 7))

(define (combine f g)
  (lambda (n)
    (f (g n))))


; with the above eval definition uncommented

((combine add3 minus7) 10)
;Unknown procedure type -- APPLY (thunk add3 #0=(((combine minus7 add3 false true ...) (procedure (f g) (...) #0#) (procedure (n) (...) #0#) (procedure (n) (...) #0#) #f ...)))

; with the above eval definition commented out

((combine add3 minus7) 10)
;6