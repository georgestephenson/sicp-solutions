; exercise 4.3

; load evaluator code

(load "resources/ch4-mceval.scm")

; leave the driver-loop commented out

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list '+ +)
        ))

(define the-global-environment (setup-environment))

; use make-table definition from section 2

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)    
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define eval-table (make-table))
(define get (eval-table 'lookup-proc))
(define put (eval-table 'insert-proc!))

; redefine eval in data-directed style

(define (install-eval-package)
  (put 'eval 'self-eval (lambda (exp env) exp))
  (put 'eval 'variable lookup-variable-value)
  (put 'eval 'quote 
    (lambda (exp env) (text-of-quotation exp)))
  (put 'eval 'set! eval-assignment)
  (put 'eval 'define eval-definition)
  (put 'eval 'if eval-if)
  (put 'eval 'lambda 
    (lambda (exp env) 
      (make-procedure (lambda-parameters exp)
                      (lambda-body exp)
                      env)))
  (put 'eval 'begin 
    (lambda (exp env) 
      (eval-sequence (begin-actions exp) env)))
  (put 'eval 'cond 
    (lambda (exp env) (eval (cond->if exp) env)))
  (put 'eval 'application 
    (lambda (exp env)
      (apply (eval (operator exp) env)
             (list-of-values (operands exp) env))))
  'done)

(define (eval exp env)
  (define proc
    (cond ((self-evaluating? exp) 'self-eval)
          ((variable? exp) 'variable)
          ((get 'eval (operator exp)) (operator exp))
          (else 'application)))
  ((get 'eval proc) exp env))


; tests

(install-eval-package)

(eval '(+ 1 2) the-global-environment)
;Value: 3

(eval '(define x 3) the-global-environment)
;Value: ok

(eval 'x the-global-environment)
;Value: 3

(eval '((lambda (x) (+ x 1)) 5) the-global-environment)
;Value: 6

(eval '(begin (define y 3) (+ y 4)) the-global-environment)
;Value: 7

(eval '(cond ((null? '()) 1)
             (else 2))
      the-global-environment)
;Value: 1

(eval '((lambda (x)
          ((lambda (y) (+ x y)) 4))
        3)
      the-global-environment)
;Value: 7