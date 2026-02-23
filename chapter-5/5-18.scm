; exercise 5.18

(load "resources/ch5-regsim.scm")

(define (make-label-map labels)
  (let ((table '()))
    (for-each
     (lambda (label-entry)
       (let ((name (car label-entry))
             (insts (cdr label-entry)))
         (let ((existing (assoc insts table)))
           (if existing
               (set-cdr! existing (cons name (cdr existing)))
               (set! table (cons (list insts name) table))))))
     labels)
    table))

(define (lookup-labels-for-inst inst-tail label-map)
  (let ((entry (assoc inst-tail label-map)))
    (if entry (cdr entry) '())))

(define (update-insts! insts labels machine)
  (let ((pc (get-register machine 'pc))
        (flag (get-register machine 'flag))
        (stack (machine 'stack))
        (ops (machine 'operations))
        (label-map (make-label-map labels)))
    (install-label-map! machine label-map)
    (install-label-map-in-registers! machine label-map)
    (for-each
     (lambda (inst)
       (set-instruction-execution-proc!
        inst
        (make-execution-procedure
         (instruction-text inst) labels machine
         pc flag stack ops)))
     insts)))

(define (install-label-map-in-register! register label-map)
  ((register 'install-label-map) label-map))

(define (install-label-map-in-registers! machine label-map)
  (for-each
    (lambda (entry)
      (install-label-map-in-register! (cadr entry) label-map))
    (machine 'register-table)))

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '())
        (count 0)
        (trace #f)
        (label-map '()))
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
      (define (display-next-inst insts)
        (begin
          (for-each (lambda (label)
                      (newline)
                      (display label)
                      (display ":"))
                    (lookup-labels-for-inst insts label-map))
          (newline)
          (display "trace ")
          (display count)
          (display ": ")
          (display (caar insts))))
      (define (execute)
        (let ((insts (get-contents pc)))
          (if (null? insts)
              'done
              (begin
                (if trace
                    (display-next-inst insts))
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
              ((eq? message 'install-label-map)
               (lambda (m) (set! label-map m)))
              ((eq? message 'register-trace-on)
               (lambda (register-name) (trace-on! (lookup-register register-name))))
              ((eq? message 'register-trace-off)
               (lambda (register-name) (trace-off! (lookup-register register-name))))
              ((eq? message 'register-table) register-table)
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))

(define (display-and-reset-count machine)
  (machine 'display-and-reset-count))

(define (trace-on! x)
  (x 'trace-on))

(define (trace-off! x)
  (x 'trace-off))

(define (install-label-map! machine label-map)
  ((machine 'install-label-map) label-map))

(define (make-register name)
  (let ((contents '*unassigned*)
        (trace #f)
        (label-map '()))
    (define (display-value val)
      (let ((labels (lookup-labels-for-inst val label-map)))
        (if (null? labels)
            (display val)
            (for-each display labels))))
    (define (dispatch message)
      (cond ((eq? message 'get) contents)
            ((eq? message 'set)
             (lambda (value)
               (if trace
                   (begin
                     (newline)
                     (display "register ")
                     (display name)
                     (display ": old value (")
                     (display-value contents)
                     (display ") new value (")
                     (display-value value)
                     (display ")")))
               (set! contents value)))
            ((eq? message 'trace-on) (set! trace #t))
            ((eq? message 'trace-off) (set! trace #f))
            ((eq? message 'install-label-map)
             (lambda (m) (set! label-map m)))
            (else
             (error "Unknown request -- REGISTER" message))))
    dispatch))

(define (register-trace-on! machine register-name)
  ((machine 'register-trace-on) register-name))

(define (register-trace-off! machine register-name)
  ((machine 'register-trace-off) register-name))

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

(register-trace-on! rec-expt-machine 'n)
(register-trace-on! rec-expt-machine 'continue)

(start rec-expt-machine)
;register continue: old value (*unassigned*) new value (expt-done)
;register n: old value (4) new value (3)
;register continue: old value (expt-done) new value (after-expt)
;register n: old value (3) new value (2)
;register continue: old value (after-expt) new value (after-expt)
;register n: old value (2) new value (1)
;register continue: old value (after-expt) new value (after-expt)
;register n: old value (1) new value (0)
;register continue: old value (after-expt) new value (after-expt)
;register n: old value (0) new value (1)
;register continue: old value (after-expt) new value (after-expt)
;register n: old value (1) new value (2)
;register continue: old value (after-expt) new value (after-expt)
;register n: old value (2) new value (3)
;register continue: old value (after-expt) new value (after-expt)
;register n: old value (3) new value (4)
;register continue: old value (after-expt) new value (expt-done)
;Value: done

(get-register-contents rec-expt-machine 'val)
;Value: 81