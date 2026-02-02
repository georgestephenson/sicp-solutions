; exercise 4.43

(load "resources/ch4-ambeval.scm")

(define the-global-environment (setup-environment))
(driver-loop)

(define (require p)
  (if (not p) (amb)))

(define (distinct? items)
  (cond ((null? items) true)
        ((null? (cdr items)) true)
        ((member (car items) (cdr items)) false)
        (else (distinct? (cdr items)))))

(define (yacht father)
  (cond ((eq? father 'parker) 'mary)
        ((eq? father 'barnacle) 'gabrielle)
        ((eq? father 'moore) 'lorna)
        ((eq? father 'hall) 'rosalind)
        ((eq? father 'downing) 'melissa)))

(define (fathers)
  (let ((moore-daughter 'mary)
        (barnacle-daughter 'melissa)
        (downing-daughter (amb 'gabrielle 'lorna 'rosalind))
        (hall-daughter (amb 'gabrielle 'lorna 'rosalind))
        (parker-daughter (amb 'gabrielle 'lorna 'rosalind)))
    (require (distinct? (list moore-daughter barnacle-daughter 
                              downing-daughter hall-daughter parker-daughter)))
    (let ((gabrielle-father 
           (cond ((eq? downing-daughter 'gabrielle) 'downing)
                 ((eq? hall-daughter 'gabrielle) 'hall)
                 ((eq? parker-daughter 'gabrielle) 'parker)))
          (lorna-father
           (cond ((eq? downing-daughter 'lorna) 'downing)
                 ((eq? hall-daughter 'lorna) 'hall)
                 ((eq? parker-daughter 'lorna) 'parker))))
      (require (eq? (yacht gabrielle-father) parker-daughter))
      (list 'lorna-father lorna-father))))

;;; Amb-Eval input:
(fathers)
;;; Starting a new problem 
;;; Amb-Eval value:
;(lorna-father downing)

try-again
;;; There are no more values of
;(fathers)


(define (fathers-no-moore-constraint)
  (let ((moore-daughter (amb 'mary 'gabrielle 'lorna 'rosalind))
        (barnacle-daughter 'melissa)
        (downing-daughter (amb 'mary 'gabrielle 'lorna 'rosalind))
        (hall-daughter (amb 'mary 'gabrielle 'lorna 'rosalind))
        (parker-daughter (amb 'mary 'gabrielle 'lorna 'rosalind)))
    (require (distinct? (list moore-daughter barnacle-daughter 
                              downing-daughter hall-daughter parker-daughter)))
    (let ((gabrielle-father 
           (cond ((eq? moore-daughter 'gabrielle) 'moore)
                 ((eq? downing-daughter 'gabrielle) 'downing)
                 ((eq? hall-daughter 'gabrielle) 'hall)
                 ((eq? parker-daughter 'gabrielle) 'parker)))
          (lorna-father
           (cond ((eq? moore-daughter 'lorna) 'moore)
                 ((eq? downing-daughter 'lorna) 'downing)
                 ((eq? hall-daughter 'lorna) 'hall)
                 ((eq? parker-daughter 'lorna) 'parker)))
          (mary-father
           (cond ((eq? moore-daughter 'mary) 'moore)
                 ((eq? downing-daughter 'mary) 'downing)
                 ((eq? hall-daughter 'mary) 'hall)
                 ((eq? parker-daughter 'mary) 'parker))))
      (require (eq? (yacht gabrielle-father) parker-daughter))
      (list 'lorna-father lorna-father 'mary-father mary-father))))

;;; Amb-Eval input:
(fathers-no-moore-constraint)
;;; Starting a new problem 
;;; Amb-Eval value:
;(lorna-father downing mary-father moore)

;;; Amb-Eval input:
try-again
;;; Amb-Eval value:
;(lorna-father parker mary-father downing)

;;; Amb-Eval input:
try-again
;;; Amb-Eval value:
;(lorna-father parker mary-father hall)

;;; Amb-Eval input:
try-again
;;; Amb-Eval value:
;(lorna-father moore mary-father downing)

;;; Amb-Eval input:
try-again
;;; There are no more values of
;(fathers-no-moore-constraint)

; Answer: 4 different solutions if we aren't told
; Mary Ann's last name is Moore