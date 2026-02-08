; exercise 4.55

; setup microshaft database and query driver loop

(load "resources/ch4-query.scm")

(initialize-data-base microshaft-data-base)

(query-driver-loop)

; part a - all people supervised by Ben Bitdiddle

(supervisor ?x (Bitdiddle Ben))
;;; Query results:
;(supervisor (tweakit lem e) (bitdiddle ben))
;(supervisor (fect cy d) (bitdiddle ben))
;(supervisor (hacker alyssa p) (bitdiddle ben))

; part b - the names and jobs of all people in the accounting division

(job ?x (accounting . ?type))
;;; Query results:
;(job (cratchet robert) (accounting scrivener))
;(job (scrooge eben) (accounting chief accountant))

; part c - the names and addresses of all people who live in Slumerville

(address ?x (Slumerville . ?details))
;;; Query results:
;(address (aull dewitt) (slumerville (onion square) 5))
;(address (reasoner louis) (slumerville (pine tree road) 80))
;(address (bitdiddle ben) (slumerville (ridge road) 10))