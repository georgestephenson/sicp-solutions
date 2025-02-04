
; Using the substitution model to think about this, we have:

; (f f)
; (f 2) because g = f
; (2 2) because g = 2

; I would expect an error that 2 is not an operator so it cannot have an operand.

(define (f g)
  (g 2))

(f f)
;The object 2 is not applicable.

; This is indeed the same error given by (2 2)

(2 2)
;The object 2 is not applicable.