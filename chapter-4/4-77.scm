; exercise 4.77

(load "resources/ch4-query.scm")

; One way to fix it is to change the order we evaluate nots 
; like in the "and" proc for example.

; Probably not the intended fix... but it does fix the issue described
; in section 4.4.3.

(define (conjoin conjuncts frame-stream)
  (define (conjoin-nots-last conjuncts frame-stream last-term-not)
    (if (empty-conjunction? conjuncts)
        frame-stream
        (let ((first (first-conjunct conjuncts)))
          (if (and (eq? (type first) 'not) (not last-term-not))
              (conjoin-nots-last (append (rest-conjuncts conjuncts) (list first))
                                 frame-stream
                                 #t)
              (conjoin-nots-last (rest-conjuncts conjuncts)
                                 (qeval first frame-stream)
                                 #f)))))
  (conjoin-nots-last conjuncts frame-stream #f))

(initialize-data-base microshaft-data-base)

(query-driver-loop)

;;; Query input:
(and (supervisor ?x ?y)
     (not (job ?x (computer programmer))))
;;; Query results:
;(and (supervisor (aull dewitt) (warbucks oliver)) (not (job (aull dewitt) (computer programmer))))
;(and (supervisor (cratchet robert) (scrooge eben)) (not (job (cratchet robert) (computer programmer))))
;(and (supervisor (scrooge eben) (warbucks oliver)) (not (job (scrooge eben) (computer programmer))))
;(and (supervisor (bitdiddle ben) (warbucks oliver)) (not (job (bitdiddle ben) (computer programmer))))
;(and (supervisor (reasoner louis) (hacker alyssa p)) (not (job (reasoner louis) (computer programmer))))
;(and (supervisor (tweakit lem e) (bitdiddle ben)) (not (job (tweakit lem e) (computer programmer))))

;;; Query input:
(and (not (job ?x (computer programmer)))
     (supervisor ?x ?y))
;;; Query results:
;(and (not (job (aull dewitt) (computer programmer))) (supervisor (aull dewitt) (warbucks oliver)))
;(and (not (job (cratchet robert) (computer programmer))) (supervisor (cratchet robert) (scrooge eben)))
;(and (not (job (scrooge eben) (computer programmer))) (supervisor (scrooge eben) (warbucks oliver)))
;(and (not (job (bitdiddle ben) (computer programmer))) (supervisor (bitdiddle ben) (warbucks oliver)))
;(and (not (job (reasoner louis) (computer programmer))) (supervisor (reasoner louis) (hacker alyssa p)))
;(and (not (job (tweakit lem e) (computer programmer))) (supervisor (tweakit lem e) (bitdiddle ben)))