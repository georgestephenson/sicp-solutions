; exercise 4.56

; setup microshaft database and query driver loop

(load "resources/ch4-query.scm")

(initialize-data-base microshaft-data-base)

(query-driver-loop)

; part a - the names of all people who are supervised by Ben Bitdiddle, together with their addresses

(and (supervisor ?x (Bitdiddle Ben))
     (address ?x ?a))
;;; Query results:
;(and (supervisor (tweakit lem e) (bitdiddle ben)) (address (tweakit lem e) (boston (bay state road) 22)))
;(and (supervisor (fect cy d) (bitdiddle ben)) (address (fect cy d) (cambridge (ames street) 3)))
;(and (supervisor (hacker alyssa p) (bitdiddle ben)) (address (hacker alyssa p) (cambridge (mass ave) 78)))


; part b - all people whose salary is less than Ben Bitdiddle's, together with their salary and Ben Bitdiddle's salary

(and (salary ?person ?person-amount)
     (salary (Bitdiddle Ben) ?ben-amount)
     (lisp-value < ?person-amount ?ben-amount))
;;; Query results:
;(and (salary (aull dewitt) 25000) (salary (bitdiddle ben) 60000) (lisp-value < 25000 60000))
;(and (salary (cratchet robert) 18000) (salary (bitdiddle ben) 60000) (lisp-value < 18000 60000))
;(and (salary (reasoner louis) 30000) (salary (bitdiddle ben) 60000) (lisp-value < 30000 60000))
;(and (salary (tweakit lem e) 25000) (salary (bitdiddle ben) 60000) (lisp-value < 25000 60000))
;(and (salary (fect cy d) 35000) (salary (bitdiddle ben) 60000) (lisp-value < 35000 60000))
;(and (salary (hacker alyssa p) 40000) (salary (bitdiddle ben) 60000) (lisp-value < 40000 60000))


; part c - all people who are supervised by someone who is not in the computer division, together with the supervisor's name and job

(and (supervisor ?person ?supervisor)
     (job ?supervisor ?supervisor-job)
     (not (job ?supervisor (computer . ?supervisor-job-type))))
;;; Query results:
;(and (supervisor (aull dewitt) (warbucks oliver)) (job (warbucks oliver) (administration big wheel)) (not (job (warbucks oliver) (computer . ?supervisor-job-type))))
;(and (supervisor (cratchet robert) (scrooge eben)) (job (scrooge eben) (accounting chief accountant)) (not (job (scrooge eben) (computer . ?supervisor-job-type))))
;(and (supervisor (scrooge eben) (warbucks oliver)) (job (warbucks oliver) (administration big wheel)) (not (job (warbucks oliver) (computer . ?supervisor-job-type))))
;(and (supervisor (bitdiddle ben) (warbucks oliver)) (job (warbucks oliver) (administration big wheel)) (not (job (warbucks oliver) (computer . ?supervisor-job-type))))