; exercise 4.11

; load evaluator code

(load "resources/ch4-mceval.scm")

; rewrite environment operations

(define (make-frame variables values)
  (if (null? variables)
      '()
      (cons (cons (car variables) (car values))
            (make-frame (cdr variables) (cdr values)))))

(define (frame-variables frame) (map car frame))
(define (frame-values frame) (map cdr frame))

(define (add-binding-to-frame! var val frame)
  (set-cdr! frame
            (cons (cons var val) (cdr frame))))

; these all remain the same:
; extend-environment
; lookup-variable-value
; set-variable-value!
; define-variable! var val env)

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
;Value: 11

(eval '(define (proc x)
         (+ x 2)) 
      the-global-environment)

(eval '(proc 0) 
      the-global-environment)
;Value: 2