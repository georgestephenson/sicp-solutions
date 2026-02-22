; exercise 5.11c

(load "resources/ch5-regsim.scm")

(define (make-machine register-names ops controller-text)
  (let ((machine (make-new-machine register-names)))
    (for-each (lambda (register-name)
                ((machine 'allocate-register) register-name))
              register-names)
    ((machine 'install-operations) ops)    
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
    machine))

(define (make-new-machine register-names)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (map (lambda (name) (list name (make-stack))) 
                    register-names))
        (the-instruction-sequence '()))
    (let ((the-ops
           (list (list 'initialize-stack
                   (lambda ()
                     (for-each (lambda (entry) ((cadr entry) 'initialize))
                               stack)))
                 (list 'print-stack-statistics
                   (lambda ()
                     (for-each (lambda (entry) ((cadr entry) 'print-statistics))
                               stack)))))
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

(define (lookup-stack-reg stack reg)
        (let ((val (assoc reg stack)))
          (if val
              (cadr val)
              (error "Unknown register:" reg))))

(define (pop stack reg)
  ((lookup-stack-reg stack reg) 'pop))

(define (push stack reg value)
  (((lookup-stack-reg stack reg) 'push) value))

(define (make-save inst machine stack pc)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      (push stack (stack-inst-reg-name inst) (get-contents reg))
      (advance-pc pc))))

(define (make-restore inst machine stack pc)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      (set-contents! reg (pop stack (stack-inst-reg-name inst)))    
      (advance-pc pc))))

; test

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
