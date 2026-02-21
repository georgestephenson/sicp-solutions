; exercise 5.7

(load "resources/ch5-regsim.scm")

; part a - recursive exponentiation

(define rec-expt-machine
  (make-machine
   '(n b continue val)
   (list (list '- -) (list '* *) (list '= =))
   '(controller
        (assign continue (label expt-done))
      expt-loop
        (test (op =) (reg n) (const 0))
        (branch (label base-case))
        (save continue)
        (save n)
        (assign n (op -) (reg n) (const 1))
        (assign continue (label after-expt))
        (goto (label expt-loop))
      after-expt
        (restore n)
        (restore continue)
        (assign val (op *) (reg b) (reg val))
        (goto (reg continue))
      base-case
        (assign val (const 1))
        (goto (reg continue))
      expt-done)))

(set-register-contents! rec-expt-machine 'b 3)

(set-register-contents! rec-expt-machine 'n 4)

(start rec-expt-machine)

(get-register-contents rec-expt-machine 'val)
;Value: 81

; part b - iterative exponentiation

(define iter-expt-machine
  (make-machine
   '(product b counter)
   (list (list '- -) (list '* *) (list '= =))
   '(controller
      test-counter
        (test (op =) (reg counter) (const 0))
        (branch (label iter-done))
        (assign product (op *) (reg b) (reg product))
        (assign counter (op -) (reg counter) (const 1))
        (goto (label test-counter))
      iter-done)))

(set-register-contents! iter-expt-machine 'b 3)

(set-register-contents! iter-expt-machine 'counter 4)

(set-register-contents! iter-expt-machine 'product 1)

(start iter-expt-machine)

(get-register-contents iter-expt-machine 'product)
;Value: 81