(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

; The "if" compound expression is an operator taking a and b as arguments.
; If b > 0 then the operator + will be applied to a and b evaluating to (+ a b)
; Else if b <= 0 then the operator - will be applied to a and b evaluating to (- a b)
; Subtracting instead of adding b if it's negative is the same as adding its absolute value |b|