; exercise 4.59

; setup microshaft database and query driver loop

(load "resources/ch4-query.scm")

(initialize-data-base microshaft-data-base)

(query-driver-loop)

(assert! (meeting accounting (Monday 9am)))
(assert! (meeting administration (Monday 10am)))
(assert! (meeting computer (Wednesday 3pm)))
(assert! (meeting administration (Friday 1pm)))
(assert! (meeting whole-company (Wednesday 4pm)))

; part a - all meetings on Friday

;;; Query input:
(meeting ?divisions (Friday ?time))
;;; Query results:
;(meeting administration (friday |1pm|))

; part b - Alyssa's new meeting-time rule

(assert!
  (rule (meeting-time ?person ?day-and-time)
        (and (job ?person (?division . ?job-type))
             (or (meeting whole-company ?day-and-time)
                 (meeting ?division ?day-and-time)))))

; part c - Alyssa's meetings on Wednesday

;;; Query input:
(meeting-time (Hacker Alyssa P) (Wednesday ?time))
;;; Query results:
;(meeting-time (hacker alyssa p) (wednesday |4pm|))
;(meeting-time (hacker alyssa p) (wednesday |3pm|))