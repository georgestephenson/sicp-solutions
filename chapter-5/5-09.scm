; exercise 5.9

(load "resources/ch5-regsim.scm")

(define (make-operation-exp exp machine labels operations)
  (let ((op (lookup-prim (operation-exp-op exp) operations))
        (aprocs
         (map (lambda (e)
                (if (label-exp? e)
                    (error "Cannot operate on label expression -- ASSEMBLE" e)
                    (make-primitive-exp e machine labels)))
              (operation-exp-operands exp))))
    (lambda ()
      (apply op (map (lambda (p) (p)) aprocs)))))

(define amb-machine
  (make-machine
   '(a b)
   (list (list 'eq? eq?))
   '(controller
      start
        (goto (label here))
      here
        (assign b (op eq?) (reg a) (label here))
        (goto (label there))
      there)))
;Cannot operate on label expression -- ASSEMBLE (label here)