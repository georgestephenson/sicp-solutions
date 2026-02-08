; exercise 4.57

; setup microshaft database and query driver loop

(load "resources/ch4-query.scm")

(initialize-data-base microshaft-data-base)

(query-driver-loop)

(assert! 
  (rule (can-replace ?person-1 ?person-2)
        (and (job ?person-1 ?job-1)
             (job ?person-2 ?job-2)
             (not (same ?person-1 ?person-2))
             (or (same ?job-1 ?job-2)
                 (can-do-job ?job-1 ?job-2)))))

; part a - all people who can replace Cy D. Fect

(can-replace ?x (Fect Cy D))
;;; Query results:
;(can-replace (hacker alyssa p) (fect cy d))
;(can-replace (bitdiddle ben) (fect cy d))

; part b - all people who can replace someone who is being paid more than they are, together with the two salaries

(and (can-replace ?person-1 ?person-2)
     (salary ?person-1 ?salary-1)
     (salary ?person-2 ?salary-2)
     (lisp-value < ?salary-1 ?salary-2))
;;; Query results:
;(and (can-replace (fect cy d) (hacker alyssa p)) (salary (fect cy d) 35000) (salary (hacker alyssa p) 40000) (lisp-value < 35000 40000))
;(and (can-replace (aull dewitt) (warbucks oliver)) (salary (aull dewitt) 25000) (salary (warbucks oliver) 150000) (lisp-value < 25000 150000))