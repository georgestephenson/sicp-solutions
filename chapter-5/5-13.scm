; exercise 5.13

(load "resources/ch5-regsim.scm")

; exclude register-names from make-machine

(define (make-machine ops controller-text)
  (let ((machine (make-new-machine)))
    ((machine 'install-operations) ops)    
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
    machine))

; change definition of lookup-register to conditionally
; perform allocate-register.

; this is probably not the most performant implementation
; however the main point is that the work is done during
; assembly and not during runtime of the machine

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '()))
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
              (begin 
                (allocate-register name)
                (lookup-register name)))))
      (define (execute)
        (let ((insts (get-contents pc)))
          (if (null? insts)
              'done
              (begin
                ((instruction-execution-proc (car insts)))
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
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))

; test

(define fib-machine
  (make-machine
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
        (assign n (reg val))               ; n now contains Fib(n-2)
        (restore val)                      ; val now contains Fib(n-1)
        (restore continue)
        (assign val                        ; Fib(n-1)+Fib(n-2)
                (op +) (reg val) (reg n)) 
        (goto (reg continue))              ; return to caller, answer is in val
      immediate-answer
        (assign val (reg n))               ; base case: Fib(n)=n
        (goto (reg continue))
      fib-done)))

(set-register-contents! fib-machine 'n 7)

(start fib-machine)

(get-register-contents fib-machine 'val)
;Value: 13