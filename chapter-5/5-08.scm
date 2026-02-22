; exercise 5.8

(load "resources/ch5-regsim.scm")

(define amb-machine
  (make-machine
   '(a)
   '()
   '(controller
      start
        (goto (label here))
      here
        (assign a (const 3))
        (goto (label there))
      here
        (assign a (const 4))
        (goto (label there))
      there)))

(start amb-machine)

(get-register-contents amb-machine 'a)
;Value: 3

(define (add-to-labels next-inst insts labels)
  (let ((new-label (make-label-entry next-inst
                                     insts)))
    (if (memq (car new-label) (map car labels))
        (error "Same label used twice in register-machine code -- ASSEMBLE"
               (car new-label))
        (cons new-label
              labels))))

(define (extract-labels text receive)
  (if (null? text)
      (receive '() '())
      (extract-labels (cdr text)
       (lambda (insts labels)
         (let ((next-inst (car text)))
           (if (symbol? next-inst)
               (receive insts
                        (add-to-labels next-inst insts labels))
               (receive (cons (make-instruction next-inst)
                              insts)
                        labels)))))))

(define amb-machine
  (make-machine
   '(a)
   '()
   '(controller
      start
        (goto (label here))
      here
        (assign a (const 3))
        (goto (label there))
      here
        (assign a (const 4))
        (goto (label there))
      there)))
;Same label used twice in register-machine code -- ASSEMBLE here
