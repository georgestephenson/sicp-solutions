; exercise 5.15

(load "resources/ch5-regsim.scm")

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '())
        (count 0))
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
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))

(define (display-and-reset-count machine)
  (machine 'display-and-reset-count))

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

(start rec-expt-machine)

(get-register-contents rec-expt-machine 'val)
;Value: 81

(display-and-reset-count rec-expt-machine)
;49

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

(display-and-reset-count iter-expt-machine)
;22