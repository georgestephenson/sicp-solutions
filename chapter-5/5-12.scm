; exercise 5.12

(load "resources/ch5-regsim.scm")

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '())
        (instructions '())
        (entry-point-registers '())
        (saved-or-restored-registers '())
        (register-sources '()))
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
      (define (add-register-source reg source)
        (let ((val (assoc reg register-sources)))
          (if val
              (if (not (member source (cadr val)))
                  (set-car! (cdr val) (cons source (cadr val))))
              (set! register-sources
                  (cons (list reg (list source))
                        register-sources)))))
      (define (list-register-sources reg)
        (let ((val (assoc reg register-sources)))
          (if val
              (cadr val)
              '())))
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
               (lambda (seq) 
                 (set! the-instruction-sequence seq)
                 (set! instructions 
                   (sort (unique-instructions (map car seq))
                         (lambda (x y) (string<? (symbol->string (car x))
                                                 (symbol->string (car y))))))))
              ((eq? message 'allocate-register) allocate-register)
              ((eq? message 'get-register) lookup-register)
              ((eq? message 'install-operations)
               (lambda (ops) (set! the-ops (append the-ops ops))))
              ((eq? message 'stack) stack)
              ((eq? message 'operations) the-ops)
              ((eq? message 'list-instructions) instructions)
              ((eq? message 'list-entry-point-registers) entry-point-registers)
              ((eq? message 'add-entry-point-register)
               (lambda (reg) (if (not (memq reg entry-point-registers))
                                 (set! entry-point-registers (cons reg entry-point-registers)))))
              ((eq? message 'list-saved-or-restored-registers) saved-or-restored-registers)
              ((eq? message 'add-saved-or-restored-register)
               (lambda (reg) (if (not (memq reg saved-or-restored-registers))
                                 (set! saved-or-restored-registers (cons reg saved-or-restored-registers)))))
              ((eq? message 'list-register-sources) list-register-sources)
              ((eq? message 'add-register-source) add-register-source)
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))

(define (unique-instructions seq)
  (cond ((null? seq) '())
        ((member (car seq) (cdr seq))
         (unique-instructions (cdr seq)))
        (else
         (cons (car seq) (unique-instructions (cdr seq))))))

(define (list-instructions machine)
  (machine 'list-instructions))

(define (list-entry-point-registers machine)
  (machine 'list-entry-point-registers))

(define (list-saved-or-restored-registers machine)
  (machine 'list-saved-or-restored-registers))

(define (list-register-sources machine reg-name)
  ((machine 'list-register-sources) reg-name))

(define (add-entry-point-register machine reg-name)
  ((machine 'add-entry-point-register) reg-name))

(define (add-saved-or-restored-register machine reg-name)
  ((machine 'add-saved-or-restored-register) reg-name))

(define (add-register-source machine reg-name source)
  ((machine 'add-register-source) reg-name source))

(define (make-goto inst machine labels pc)
  (let ((dest (goto-dest inst)))
    (cond ((label-exp? dest)
           (let ((insts
                  (lookup-label labels
                                (label-exp-label dest))))
             (lambda () (set-contents! pc insts))))
          ((register-exp? dest)
           (let* ((reg-name (register-exp-reg dest))
                  (reg (get-register machine reg-name)))
             (add-entry-point-register machine reg-name)
             (lambda ()
               (set-contents! pc (get-contents reg)))))
          (else (error "Bad GOTO instruction -- ASSEMBLE"
                       inst)))))

(define (make-save inst machine stack pc)
  (let* ((reg-name (stack-inst-reg-name inst))
         (reg (get-register machine
                            reg-name)))
    (add-saved-or-restored-register machine reg-name)
    (lambda ()
      (push stack (get-contents reg))
      (advance-pc pc))))

(define (make-restore inst machine stack pc)
  (let* ((reg-name (stack-inst-reg-name inst))
         (reg (get-register machine
                            reg-name)))
    (add-saved-or-restored-register machine reg-name)
    (lambda ()
      (set-contents! reg (pop stack))    
      (advance-pc pc))))

(define (make-assign inst machine labels operations pc)
  (let* ((reg-name (assign-reg-name inst))
         (target (get-register machine reg-name))
        (value-exp (assign-value-exp inst)))
    (let ((value-proc
           (if (operation-exp? value-exp)
               (make-operation-exp
                value-exp machine labels operations)
               (make-primitive-exp
                (car value-exp) machine labels))))
      (add-register-source machine reg-name value-exp)
      (lambda ()                ; execution procedure for assign
        (set-contents! target (value-proc))
        (advance-pc pc)))))

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

(list-instructions fib-machine)
;Value: (
; (assign continue (label fib-done)) 
; (assign continue (label afterfib-n-1)) 
; (assign n (op -) (reg n) (const 1)) 
; (assign n (op -) (reg n) (const 2)) 
; (assign continue (label afterfib-n-2)) 
; (assign n (reg val)) 
; (assign val (op +) (reg val) (reg n)) 
; (assign val (reg n)) 
; (branch (label immediate-answer)) 
; (goto (label fib-loop)) 
; (goto (reg continue)) 
; (restore n) 
; (restore val) 
; (restore continue) 
; (save n) 
; (save continue) 
; (save val) 
; (test (op <) (reg n) (const 2))
;)

(list-entry-point-registers fib-machine)
;Value: (continue)

(list-saved-or-restored-registers fib-machine)
;Value: (val n continue)

(list-register-sources fib-machine 'continue)
;Value: (((label afterfib-n-2)) ((label afterfib-n-1)) ((label fib-done)))

(list-register-sources fib-machine 'val)
;Value: (((reg n)) ((op +) (reg val) (reg n)))

(list-register-sources fib-machine 'n)
;Value: (((reg val)) ((op -) (reg n) (const 2)) ((op -) (reg n) (const 1)))
