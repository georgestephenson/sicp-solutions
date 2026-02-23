; exercise 5.16

; extending solution to exercise 5.15 as it sounds like that's the intention

(load "resources/ch5-regsim.scm")

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '())
        (count 0)
        (trace #f))
    (let ((the-ops
           (list (list 'initialize-stack
                       (lambda () (stack 'initialize)))
                 ;;**next for monitored stack (as in section 5.2.4)
                 ;;  -- comment out if not wanted
                 (list 'print-stack-statistics
                       (lambda () (stack 'print-statistics)))))
          (register-table
           (list (list 'pc pc) (list 'flag flag))))
      (define (allocate-register name)
        (if (assoc name register-table)
            (error "Multiply defined register: " name)
            (set! register-table
                  (cons (list name (make-register name))
                        register-table)))
        'register-allocated)
      (define (lookup-register name)
        (let ((val (assoc name register-table)))
          (if val
              (cadr val)
              (error "Unknown register:" name))))
      (define (display-and-reset-count)
        (newline)
        (display count)
        (set! count 0))
      (define (execute)
        (let ((insts (get-contents pc)))
          (if (null? insts)
              'done
              (begin
                (if trace
                    (begin
                      (newline)
                      (display "trace ")
                      (display count)
                      (display ": ")
                      (display (caar insts))))
                ((instruction-execution-proc (car insts)))
                (set! count (+ count 1))
                (execute)))))
      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (execute))
              ((eq? message 'install-instruction-sequence)
               (lambda (seq) (set! the-instruction-sequence seq)))
              ((eq? message 'allocate-register) allocate-register)
              ((eq? message 'get-register) lookup-register)
              ((eq? message 'install-operations)
               (lambda (ops) (set! the-ops (append the-ops ops))))
              ((eq? message 'stack) stack)
              ((eq? message 'operations) the-ops)
              ((eq? message 'display-and-reset-count) (display-and-reset-count))
              ((eq? message 'trace-on) (set! trace #t))
              ((eq? message 'trace-off) (set! trace #f))
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))

(define (display-and-reset-count machine)
  (machine 'display-and-reset-count))

(define (trace-on! machine)
  (machine 'trace-on))

(define (trace-off! machine)
  (machine 'trace-off))

; test

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

(set-register-contents! rec-expt-machine 'n 4)

(trace-on! rec-expt-machine)

(start rec-expt-machine)
;trace 0: (assign continue (label expt-done))
;trace 1: (test (op =) (reg n) (const 0))
;trace 2: (branch (label base-case))
;trace 3: (save continue)
;trace 4: (save n)
;trace 5: (assign n (op -) (reg n) (const 1))
;trace 6: (assign continue (label after-expt))
;trace 7: (goto (label expt-loop))
;trace 8: (test (op =) (reg n) (const 0))
;trace 9: (branch (label base-case))
;trace 10: (save continue)
;trace 11: (save n)
;trace 12: (assign n (op -) (reg n) (const 1))
;trace 13: (assign continue (label after-expt))
;trace 14: (goto (label expt-loop))
;trace 15: (test (op =) (reg n) (const 0))
;trace 16: (branch (label base-case))
;trace 17: (save continue)
;trace 18: (save n)
;trace 19: (assign n (op -) (reg n) (const 1))
;trace 20: (assign continue (label after-expt))
;trace 21: (goto (label expt-loop))
;trace 22: (test (op =) (reg n) (const 0))
;trace 23: (branch (label base-case))
;trace 24: (save continue)
;trace 25: (save n)
;trace 26: (assign n (op -) (reg n) (const 1))
;trace 27: (assign continue (label after-expt))
;trace 28: (goto (label expt-loop))
;trace 29: (test (op =) (reg n) (const 0))
;trace 30: (branch (label base-case))
;trace 31: (assign val (const 1))
;trace 32: (goto (reg continue))
;trace 33: (restore n)
;trace 34: (restore continue)
;trace 35: (assign val (op *) (reg b) (reg val))
;trace 36: (goto (reg continue))
;trace 37: (restore n)
;trace 38: (restore continue)
;trace 39: (assign val (op *) (reg b) (reg val))
;trace 40: (goto (reg continue))
;trace 41: (restore n)
;trace 42: (restore continue)
;trace 43: (assign val (op *) (reg b) (reg val))
;trace 44: (goto (reg continue))
;trace 45: (restore n)
;trace 46: (restore continue)
;trace 47: (assign val (op *) (reg b) (reg val))
;trace 48: (goto (reg continue))
;Value: done

(get-register-contents rec-expt-machine 'val)
;Value: 81