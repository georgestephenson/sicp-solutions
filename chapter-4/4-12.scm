; exercise 4.12

; load evaluator code

(load "resources/ch4-mceval.scm")

; use abstractions

(define (scan frame var on-var-found on-vars-null)
  (let loop ((vars (frame-variables frame))
             (vals (frame-values frame)))
    (cond ((null? vars)
           (on-vars-null))
          ((eq? var (car vars))
           (on-var-found vals))
          (else
           (loop (cdr vars) (cdr vals))))))

(define (variable-action var env on-var-found)
  (define (env-loop env)
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan frame 
                var 
                on-var-found 
                (lambda () (env-loop (enclosing-environment env)))))))
  (env-loop env))

(define (lookup-variable-value var env)
  (variable-action var env car))

(define (set-variable-value! var val env)
  (variable-action var env (lambda (vals) (set-car! vals val))))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (scan frame 
          var
          (lambda (vals) (set-car! vals val)) 
          (lambda () (add-binding-to-frame! var val frame)))))

; test

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list '+ +)
        (list '= =)
        ))

(define the-global-environment (setup-environment))

(eval '(define x 10) 
      the-global-environment)

(eval '(+ x 1)
      the-global-environment)

(eval '(define (proc x)
         (+ x 2)) 
      the-global-environment)

(eval '(proc 0) 
      the-global-environment)