; exercise 5.11b

(load "resources/ch5-regsim.scm")

(define (make-save inst machine stack pc)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      (push stack (cons (stack-inst-reg-name inst) (get-contents reg)))
      (advance-pc pc))))

(define (make-restore inst machine stack pc)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      (let ((item (pop stack)))
        (if (eq? (car item) (stack-inst-reg-name inst))
            (set-contents! reg (cdr item))
            (error "Restored item was saved from a different register -- ASSEMBLE" (car item))))          
      (advance-pc pc))))

; test with Fibonacci machine modified in exercise 5.11a

(define fib-machine
  (make-machine
   '(n continue val)
   (list (list '< <) (list '+ +) (list '- -))
   '(controller
        (assign continue (label fib-done))
      fib-loop
        (test (op <) (reg n) (const 2))
        (branch (label immediate-answer))
        ;; set up to compute Fib(n-1)
        (save continue)
        (assign continue (label afterfib-n-1))
        (save n)                           ; save old value of n
        (assign n (op -) (reg n) (const 1)); clobber n to n-1
        (goto (label fib-loop))            ; perform recursive call
      afterfib-n-1                         ; upon return, val contains Fib(n-1)
        (restore n)
        (restore continue)
        ;; set up to compute Fib(n-2)
        (assign n (op -) (reg n) (const 2))
        (save continue)
        (assign continue (label afterfib-n-2))
        (save val)                         ; save Fib(n-1)
        (goto (label fib-loop))
      afterfib-n-2                         ; upon return, val contains Fib(n-2)
        (restore n)                        ; n contains Fib(n-1), val contains Fib(n-2)
        (restore continue)
        (assign val                        ; Fib(n-2)+Fib(n-1)
           (op +) (reg val) (reg n)) 
        (goto (reg continue))              ; return to caller, answer is in val
      immediate-answer
        (assign val (reg n))               ; base case: Fib(n)=n
        (goto (reg continue))
      fib-done)))

(set-register-contents! fib-machine 'n 3)

(start fib-machine)
;Restored item was saved from a different register -- ASSEMBLE val