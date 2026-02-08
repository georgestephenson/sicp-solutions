; exercise 4.58

; setup microshaft database and query driver loop

(load "resources/ch4-query.scm")

(initialize-data-base microshaft-data-base)

(query-driver-loop)

(assert! 
  (rule (big-shot ?person ?division)
        (and (job ?person (?division . ?job-type))
             (not (and (supervisor ?person ?supervisor)
                       (job ?supervisor (?division . ?supervisor-job-type)))))))

;;; Query input:
(big-shot (Bitdiddle Ben) computer)
;;; Query results:
;(big-shot (bitdiddle ben) computer)

;;; Query input:
(big-shot (Hacker Alyssa P) computer)
;;; Query results:

;;; Query input:
(big-shot (Warbucks Oliver) administration)
;;; Query results:
;(big-shot (warbucks oliver) administration)