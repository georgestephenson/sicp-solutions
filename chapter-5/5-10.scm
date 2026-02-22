; exercise 5.10

(load "resources/ch5-regsim.scm")

; syntax change: make "test" and "branch" a one-liner instruction

(define (make-execution-procedure inst labels machine
                                  pc flag stack ops)
  (cond ((eq? (car inst) 'assign)
         (make-assign inst machine labels ops pc))
        ((eq? (car inst) 'test)
         (make-test inst machine labels ops flag pc))
        ((eq? (car inst) 'goto)
         (make-goto inst machine labels pc))
        ((eq? (car inst) 'save)
         (make-save inst machine stack pc))
        ((eq? (car inst) 'restore)
         (make-restore inst machine stack pc))
        ((eq? (car inst) 'perform)
         (make-perform inst machine labels ops pc))
        (else (error "Unknown instruction type -- ASSEMBLE"
                     inst))))

(define (make-test inst machine labels operations flag pc)
  (let ((condition (test-condition inst)))
    (if (operation-exp? condition)
        (let ((condition-proc
               (make-operation-exp
                condition machine labels operations)))
          (lambda ()
            (if (condition-proc)
                ((make-branch inst machine labels flag pc))
                (advance-pc pc))))
        (error "Bad TEST instruction -- ASSEMBLE" inst))))

(define (test-condition test-instruction)
  (define (iter arg-list)
    (if (eq? (car arg-list) 'branch)
        '()
        (cons (car arg-list)
              (iter (cdr arg-list)))))
  (iter (cdr test-instruction)))

(define (branch-dest branch-instruction)
  (if (eq? (car branch-instruction) 'branch)
      (cadr branch-instruction)
      (branch-dest (cdr branch-instruction))))

(define rec-expt-machine
  (make-machine
   '(n b continue val)
   (list (list '- -) (list '* *) (list '= =))
   '(controller
        (assign continue (label expt-done))
      expt-loop
        (test (op =) (reg n) (const 0) branch (label base-case))
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