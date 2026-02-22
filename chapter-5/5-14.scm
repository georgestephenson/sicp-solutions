; exercise 5.14

(load "resources/ch5-regsim.scm")

(define fact-machine
  (make-machine
   '(n val continue)
   (list (list '= =) (list '* *) (list '- -) (list 'read read))
   '(controller
      fact-init
        (assign n (op read))
        (perform (op initialize-stack))
        (assign continue (label fact-done))     ; set up final return address
      fact-loop
        (test (op =) (reg n) (const 1))
        (branch (label base-case))
        ;; Set up for the recursive call by saving n and continue.
        ;; Set up continue so that the computation will continue
        ;; at after-fact when the subroutine returns.
        (save continue)
        (save n)
        (assign n (op -) (reg n) (const 1))
        (assign continue (label after-fact))
        (goto (label fact-loop))
      after-fact
        (restore n)
        (restore continue)
        (assign val (op *) (reg n) (reg val))   ; val now contains n(n-1)!
        (goto (reg continue))                   ; return to caller
      base-case
        (assign val (const 1))                  ; base case: 1!=1
        (goto (reg continue))                   ; return to caller
      fact-done
        (perform (op print-stack-statistics))
        (goto (label fact-init)))))

(start fact-machine)
1
;(total-pushes = 0 maximum-depth = 0)
2
;(total-pushes = 2 maximum-depth = 2)
3
;(total-pushes = 4 maximum-depth = 4)
4
;(total-pushes = 6 maximum-depth = 6)
5
;(total-pushes = 8 maximum-depth = 8)

; total-pushes and maximum-depth are both equal to 2(n-1)