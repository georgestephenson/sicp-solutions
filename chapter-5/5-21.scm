; exercise 5.21

(load "resources/ch5-regsim.scm")

(define rec-count-leaves-machine
  (make-machine
   '(tree temp continue val)
   (list (list '+ +) (list 'null? null?) 
         (list 'car car) (list 'cdr cdr)
         (list 'pair? pair?) (list 'not not))
   '(controller
        (assign continue (label count-done))
      count-loop
        (test (op null?) (reg tree))
        (branch (label immediate-answer-0))
        (assign temp (op pair?) (reg tree))
        (test (op not) (reg temp))
        (branch (label immediate-answer-1))
        ;; set up to compute (count-leaves (car tree))
        (save continue)
        (assign continue (label aftercount-car))
        (save tree)                        ; save old value of tree
        (assign tree (op car) (reg tree))  ; clobber tree to (car tree)
        (goto (label count-loop))          ; perform recursive call
      aftercount-car                       ; upon return, val contains (count-leaves (car tree))
        (restore tree)
        (restore continue)
        ;; set up to compute (count-leaves (cdr tree))
        (assign tree (op cdr) (reg tree))
        (save continue)
        (assign continue (label aftercount-cdr))
        (save val)                         ; save (count-leaves (car tree))
        (goto (label count-loop))
      aftercount-cdr                       ; upon return, val contains (count-leaves (cdr tree))
        (assign temp (reg val))
        (restore val)
        (restore continue)
        (assign val                        ; (+ (count-leaves (car tree))
                (op +) (reg val) (reg temp)) ;  (count-leaves (cdr tree)))
        (goto (reg continue))              ; return to caller, answer is in val
      immediate-answer-0
        (assign val (const 0))
        (goto (reg continue))
      immediate-answer-1
        (assign val (const 1))
        (goto (reg continue))
      count-done)))

(define x (cons (list 1 2) (list 3 4)))
(define y (list x x))

(set-register-contents! rec-count-leaves-machine 'tree y)

(start rec-count-leaves-machine)

(get-register-contents rec-count-leaves-machine 'val)
;Value: 8


(define iter-count-leaves-machine
  (make-machine
   '(tree continue temp n)
   (list (list '+ +) (list 'null? null?) 
         (list 'car car) (list 'cdr cdr)
         (list 'pair? pair?) (list 'not not))
   '(controller
        (assign n (const 0))
        (assign continue (label count-done))
      count-loop
        (test (op null?) (reg tree))
        (branch (label immediate-answer-n))
        (assign temp (op pair?) (reg tree))
        (test (op not) (reg temp))
        (branch (label immediate-answer-n1))
        ;; set up to compute (count-iter (car tree) n)
        (save continue)
        (assign continue (label aftercount-car))
        (save tree)                        ; save old value of tree
        (assign tree (op car) (reg tree))  ; clobber tree to (car tree)
        (goto (label count-loop))          ; perform recursive call
      aftercount-car
        (restore tree)
        (restore continue)
        (assign tree (op cdr) (reg tree))
        (goto (label count-loop))
      immediate-answer-n
        (goto (reg continue))
      immediate-answer-n1
        (assign n (op +) (reg n) (const 1))
        (goto (reg continue))
      count-done)))

(set-register-contents! iter-count-leaves-machine 'tree y)

(start iter-count-leaves-machine)

(get-register-contents iter-count-leaves-machine 'n)
;Value: 8