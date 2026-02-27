; exercise 5.22

(load "resources/ch5-regsim.scm")

; (define (append x y)
;   (if (null? x)
;       y
;       (cons (car x) (append (cdr x) y))))

(define append-machine
  (make-machine
   '(x y result continue)
   (list (list 'null? null?) (list 'cons cons)
         (list 'car car) (list 'cdr cdr))
   '(controller
        (assign continue (label append-done))
      append-loop
        (test (op null?) (reg x))
        (branch (label base-case))
        (save continue)
        (save x)
        (assign x (op cdr) (reg x))
        (assign continue (label after-append))
        (goto (label append-loop))
      after-append
        (restore x)
        (restore continue)
        (save x)
        (assign x (op car) (reg x)) ; clobber x to be (car x)
        (assign result (op cons) (reg x) (reg result))
        (restore x)
        (goto (reg continue))
      base-case
        (assign result (reg y))
        (goto (reg continue))
      append-done)))

(define x (list 'a 'b))
(define y (list 'c 'd))

(set-register-contents! append-machine 'x x)
(set-register-contents! append-machine 'y y)

(start append-machine)

(get-register-contents append-machine 'x)
;Value: (a b)
(get-register-contents append-machine 'y)
;Value: (c d)
(get-register-contents append-machine 'result)
;Value: (a b c d)

; (define (append! x y)
;   (set-cdr! (last-pair x) y)
;   x)

; (define (last-pair x)
;   (if (null? (cdr x)) x (last-pair (cdr x))))

(define append!-machine
  (make-machine
   '(x y p)
   (list (list 'null? null?)
         (list 'cdr cdr) (list 'set-cdr! set-cdr!))
   '(controller
        (assign p (reg x))
      last-pair-loop
        (save p)
        (assign p (op cdr) (reg p))
        (test (op null?) (reg p))
        (branch (label last-pair-done))
        (goto (label last-pair-loop))
      last-pair-done
        (restore p)
        (perform (op set-cdr!) (reg p) (reg y)))))

(set-register-contents! append!-machine 'x x)
(set-register-contents! append!-machine 'y y)

(start append!-machine)

(get-register-contents append!-machine 'x)
;Value: (a b c d)
(get-register-contents append!-machine 'y)
;Value: (c d)
(get-register-contents append!-machine 'p)
;Value: (b c d)