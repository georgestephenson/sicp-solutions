; prerequisites

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

;;;;;;;;;;
; part a ;
;;;;;;;;;;

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (car (cdr mobile)))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (car (cdr branch)))

(define branch1 (make-branch 4 7))
(define branch2 (make-branch 9 10))
(define mobile1 (make-mobile branch1 branch2))
(define branch3 (make-branch 43 mobile1))
(define branch4 (make-branch 31 11))
(define mobile2 (make-mobile branch3 branch4))

(left-branch mobile1)
;Value: (4 7)
(right-branch mobile2)
;Value: (31 11)
(branch-length branch4)
;Value: 31
(branch-structure branch3)
;Value: ((4 7) (9 10))

;;;;;;;;;;
; part b ;
;;;;;;;;;;

(define (branch-weight branch)
  (let ((structure (branch-structure branch)))
      (if (not (pair? structure))
          structure
          (total-weight structure))))

(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile)) 
     (branch-weight (right-branch mobile))))

; mobile 2 weight = ((7 + 10) + 11) = 28

(total-weight mobile2)
;Value: 28


;;;;;;;;;;
; part c ;
;;;;;;;;;;

(define (torque branch)
  (* (branch-length branch) (branch-weight branch)))

(define (balanced-branch? branch)
  (let ((structure (branch-structure branch)))
    (if (not (pair? structure))
        #t
        (balanced? structure))))

(define (balanced? mobile)
  (and (= (torque (left-branch mobile))
          (torque (right-branch mobile)))
       (and (balanced-branch? (left-branch mobile))
            (balanced-branch? (right-branch mobile)))))

(balanced? mobile1)
;Value: #f
(balanced? mobile2)
;Value: #f

; make a balanced submobile
(define branch1 (make-branch 4 80))
(define branch2 (make-branch 40 8))
(define mobile1 (make-mobile branch1 branch2))

; balance base mobile - mobile1 weight is now 88
(define branch3 (make-branch 3 mobile1))
(define branch4 (make-branch 44 6))
(define mobile2 (make-mobile branch3 branch4))

(balanced? mobile1)
;Value: #t
(balanced? mobile2)
;Value: #t

;;;;;;;;;;
; part d ;
;;;;;;;;;;

(define (make-mobile left right)
  (cons left right))

(define (make-branch length structure)
  (cons length structure))

; we just need to change these selectors to only call cdr.
; the (car (cdr x)) construction caters for lists ending with nil (or '())

(define (right-branch mobile)
  (cdr mobile))

(define (branch-structure branch)
  (cdr branch))

(define branch1 (make-branch 4 7))
(define branch2 (make-branch 9 10))
(define mobile1 (make-mobile branch1 branch2))
(define branch3 (make-branch 43 mobile1))
(define branch4 (make-branch 31 11))
(define mobile2 (make-mobile branch3 branch4))

(total-weight mobile2)
;Value: 28