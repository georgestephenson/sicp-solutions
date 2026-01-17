; exercise 4.1

; orginal definition of list-of-values

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

; force evaluate operands from left to right

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ((value (eval (first-operand exps) env)))
        (cons value
              (list-of-values (rest-operands exps) env)))))

; force evaluate operands from right to left

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ((rest-of-values
             (list-of-values (rest-operands exps) env)))
        (cons (eval (first-operand exps) env)
              rest-of-values))))