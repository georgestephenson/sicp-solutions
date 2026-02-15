; exercise 4.75

(load "resources/ch4-query.scm")

(define (initialize-data-base rules-and-assertions)
  (define (deal-out r-and-a rules assertions)
    (cond ((null? r-and-a)
           (set! THE-ASSERTIONS (list->stream assertions))
           (set! THE-RULES (list->stream rules))
           'done)
          (else
           (let ((s (query-syntax-process (car r-and-a))))
             (cond ((rule? s)
                    (store-rule-in-index s)
                    (deal-out (cdr r-and-a)
                              (cons s rules)
                              assertions))
                   (else
                    (store-assertion-in-index s)
                    (deal-out (cdr r-and-a)
                              rules
                              (cons s assertions))))))))
  (let ((operation-table (make-table)))
    (set! get (operation-table 'lookup-proc))
    (set! put (operation-table 'insert-proc!)))
  (put 'and 'qeval conjoin)
  (put 'or 'qeval disjoin)
  (put 'not 'qeval negate)
  (put 'lisp-value 'qeval lisp-value)
  (put 'always-true 'qeval always-true)
  (put 'unique 'qeval uniquely-asserted)
  (deal-out rules-and-assertions '() '()))

; definition of uniquely-asserted

(define (uniquely-asserted contents frame-stream)
  (stream-flatmap
    (lambda (frame)
      (let ((result-stream (qeval (car contents)  ; Extract the actual query pattern
                                  (singleton-stream frame))))
        (if (singleton-stream? result-stream)
            result-stream
            the-empty-stream)))
    frame-stream))

(define (singleton-stream? s)
  (and (not (stream-null? s))
       (stream-null? (stream-cdr s))))

(initialize-data-base microshaft-data-base)

(query-driver-loop)

; test

(unique (job ?x (computer wizard)))
;;; Query results:
;(unique (job (bitdiddle ben) (computer wizard)))

;;; Query input:
(unique (job ?x (computer programmer)))
;;; Query results:
; no results

;;; Query input:
(and (job ?x ?j) (unique (job ?anyone ?j)))
;;; Query results:
;(and (job (aull dewitt) (administration secretary)) (unique (job (aull dewitt) (administration secretary))))
;(and (job (cratchet robert) (accounting scrivener)) (unique (job (cratchet robert) (accounting scrivener))))
;(and (job (scrooge eben) (accounting chief accountant)) (unique (job (scrooge eben) (accounting chief accountant))))
;(and (job (warbucks oliver) (administration big wheel)) (unique (job (warbucks oliver) (administration big wheel))))
;(and (job (reasoner louis) (computer programmer trainee)) (unique (job (reasoner louis) (computer programmer trainee))))
;(and (job (tweakit lem e) (computer technician)) (unique (job (tweakit lem e) (computer technician))))
;(and (job (bitdiddle ben) (computer wizard)) (unique (job (bitdiddle ben) (computer wizard))))

; all prople who supervise precisely one person

;;; Query input:
(and (job ?supervisor ?j) (unique (supervisor ?anyone ?supervisor)))
;;; Query results:
;(and (job (scrooge eben) (accounting chief accountant)) (unique (supervisor (cratchet robert) (scrooge eben))))
;(and (job (hacker alyssa p) (computer programmer)) (unique (supervisor (reasoner louis) (hacker alyssa p))))