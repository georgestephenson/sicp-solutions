; exercise 4.29
; load evaluator code

(load "resources/ch4-leval.scm")

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

; force-it without memoization - uncomment to remove memoization
; (define (force-it obj)
;   (if (thunk? obj)
;       (actual-value (thunk-exp obj) (thunk-env obj))
;       obj))

(define the-global-environment (setup-environment))
(driver-loop)

; I would expect most kinds of deeply recursive expressions to run much slower without memoization

((lambda (n)
   ((lambda (fact)
      (fact fact n))
    (lambda (ft k)
      (if (= k 1)
          1
          (* k (ft ft (- k 1)))))))
 1000)
; result: a VERY big number!
; with memoization - ran in less than 1 second
; without memoization - still not completed after many seconds

(define count 0)

(define (id x)
  (set! count (+ count 1))
  x)

(define (square x)
  (* x x))

; running lazy evaluator with memoization

;;; L-Eval input:
(square (id 10))
;;; L-Eval value:
100

;;; L-Eval input:
count
;;; L-Eval value:
1

; running lazy evaluator without memoization

;;; L-Eval input:
(square (id 10))
;;; L-Eval value:
100

;;; L-Eval input:
count
;;; L-Eval value:
2