; exercise 4.31

; Discussion: "define" is derived from "lambda", and "lambda" is derived from "procedure",
; so I chose to handle this in the "apply" procedure when handling procedures.
; Which means you can also use (b lazy) or (d lazy-memo) in a lambda too. I think that's fine.
; I haven't been too precious about how I access parameters, or handling errors and edge cases.
;
; I used the existing lazy evaluator as a base (ch4-leval.scm), which has laziness and
; memoization by default, and added conditions to:
;
; 1. Force evaluation if the argument is not "lazy" or "lazy-memo"
; 2. Skip memoization if the argument is "lazy" but not "lazy-memo"

; load evaluator code

(load "resources/ch4-leval.scm")

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list 'list list)
        (list '+ +)
        (list 'map map)
        (list 'newline newline)
        (list 'display display)
        ))

(define (apply procedure arguments env)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure
          procedure
          (list-of-arg-values arguments env))) ; changed
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           (determine-list-of-args (cadr procedure) arguments env) ; changed
           (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type -- APPLY" procedure))))

; ignore the lazy and lazy-memo markers
(define (param-name param)
  (if (symbol? param)
      param
      (car param)))

(define (param-eval-exp param)
  (cond ((symbol? param) actual-value)
        ((eq? (cadr param) 'lazy) delay-it-no-memo)
        ((eq? (cadr param) 'lazy-memo) delay-it)
        (else (error "Can't read procedure parameter -- PARAM-EVAL-EXP" param))))

(define (delay-it-no-memo exp env)
  (list 'thunk-no-memo exp env))

(define (thunk? obj)
  (or (tagged-list? obj 'thunk)
      (tagged-list? obj 'thunk-no-memo)))

(define (thunk-with-memo? obj)
  (tagged-list? obj 'thunk))

(define (force-it obj)
  (cond ((thunk? obj)
         (let ((result (actual-value
                        (thunk-exp obj)
                        (thunk-env obj))))
           (if (thunk-with-memo? obj)
             (begin
               (set-car! obj 'evaluated-thunk)
               (set-car! (cdr obj) result)  ; replace exp with its value
               (set-cdr! (cdr obj) '())))     ; forget unneeded env
           result))
        ((evaluated-thunk? obj)
         (thunk-value obj))
        (else obj)))

(define (procedure-parameters p) 
  (let ((params (cadr p)))
    (map param-name params)))

(define (determine-list-of-args params exps env)
  (if (no-operands? exps)
      '()
      (cons ((param-eval-exp (car params)) (first-operand exps) env)
            (determine-list-of-args (cdr params)
                                    (rest-operands exps)
                                    env))))

; test

(define the-global-environment (setup-environment))
(driver-loop)

(define lazy-memo-calls 0)
(define lazy-no-memo-calls 0)

(define (a)
  (newline)
  (display "a called")
  1)

(define (b)
  (newline)
  (display "b called")
  (set! lazy-no-memo-calls (+ lazy-no-memo-calls 1))
  2)

(define (c)
  (newline)
  (display "c called")
  3)

(define (d)
  (newline)
  (display "d called")
  (set! lazy-memo-calls (+ lazy-memo-calls 1))
  4)

; testing laziness - b and d don't get called

(define (f a (b lazy) c (d lazy-memo))
  (newline)
  (display "f called")
  0)

;;; L-Eval input:
(f (a) (b) (c) (d))
;c called
;a called
;f called
;;; L-Eval value:
;0

; testing forcing b and d to evaluate when needed

(define (g a (b lazy) c (d lazy-memo))
  (newline)
  (display "g called")
  (+ a b c d))

;;; L-Eval input:
(g (a) (b) (c) (d))
;c called
;a called
;g called
;d called
;b called
;;; L-Eval value:
;10

; testing if d is memoized - b gets called twice, d gets called once

(define (h a (b lazy) c (d lazy-memo))
  (newline)
  (display "h called")
  (+ b b d d))  ; force b twice, force d twice

;;; L-Eval input:
(h (a) (b) (c) (d))
;c called
;a called
;h called
;d called
;b called
;b called
;;; L-Eval value:
;12

;;; L-Eval input:
lazy-memo-calls
;;; L-Eval value:
;2

;;; L-Eval input:
lazy-no-memo-calls
;;; L-Eval value:
;3