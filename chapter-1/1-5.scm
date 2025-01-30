(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

(test 0 (p))

; Applicative-order must first evaluation the arguments and then apply.
; Since the argument (p) evaluates to itself, the evaluation of this argument will never complete.
;
; Normal-order evaluation would fully expand the evaluation of all compound expressions into primitives as follows:
; (test 0 (p))
; (if (= 0 0) 0 (p))
; (if #t 0 (p))
; 0
;
; Thus normal-order evaluation will output 0 whereas applicative-order will never complete.