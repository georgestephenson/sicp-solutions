; exercise 4.14

; load evaluator code

(load "resources/ch4-mceval.scm")

; Louis's version

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list 'list list)
        (list '+ +)
        (list 'map map)
        ))

(define the-global-environment (setup-environment))

(eval '(define x (list 1 2 3 4 5)) the-global-environment)

(eval '(map (lambda (y) (+ y 1)) x) the-global-environment)
;The object (procedure (y) ((+ y 1)) (((x false true car cdr cons null? list + map) (1 2 3 4 5) #f #t (primitive #[compiled-procedure 12 ("list" #x0) #x7e02 #xe03b4a]) (primitive #[compiled-procedure 13 ("list" #x0) #x7dbd #xe03b05]) (primitive #[compiled-procedure 14 ("list" #x0) #x7d89 #xe03ad1]) (primitive #[compiled-procedure 15 ("list" #x0) #x7d31 #xe03a79]) (primitive #[compiled-procedure 16 ("list" #x0) #x7c48 #xe03990]) (primitive #[arity-dispatched-procedure 17]) (primitive #[arity-dispatched-procedure 18])))) is not applicable.

; Eva's version

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list 'list list)
        (list '+ +)
        (list 'append append)
        ))

(define the-global-environment (setup-environment))

(eval '(define nil '()) the-global-environment)

(eval '(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence))))) 
  the-global-environment)

(eval '(define (map p sequence)
  (accumulate (lambda (x y) (append (list (p x)) y)) nil sequence))
  the-global-environment)

(eval '(define x (list 1 2 3 4 5)) the-global-environment)

(eval '(map (lambda (y) (+ y 1)) x) the-global-environment)
;Value: (2 3 4 5 6)


; Explanation:
; Using the primitive map expects a native lambda or
; procedure as input. So it doesn't understand
; lambdas or procedures produced by the metacircular 
; evaluator which have bespoke data structures.
; But map defined with the metacircular evaluator
; works natively with these structures.