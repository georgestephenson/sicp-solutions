; exercise 4.60

; setup microshaft database and query driver loop

(load "resources/ch4-query.scm")

(initialize-data-base microshaft-data-base)

(query-driver-loop)

; the problem is that lives-near as defined is a bidirectional relationship.
; A lives near B if they're in the same town. B lives near A if they're in the same town.
; To avoid listing twice, we'd have to make the relationship one-directional in some way
; We can do it with any ordering or people, like salary, however this is fragile, 
; the ordering should be something like a unique ID which is arbitrary

(assert! 
  (rule (all-lives-near ?person-1 ?person-2)
        (and (lives-near ?person-1 ?person-2)
             (salary ?person-1 ?salary-1)
             (salary ?person-2 ?salary-2)
             (lisp-value < ?salary-1 ?salary-2))))

;;; Query input:
(all-lives-near ?person-1 ?person-2)
;;; Query results:
;(all-lives-near (aull dewitt) (reasoner louis))
;(all-lives-near (aull dewitt) (bitdiddle ben))
;(all-lives-near (reasoner louis) (bitdiddle ben))
;(all-lives-near (fect cy d) (hacker alyssa p))


; the real problem with this is that is would fragment the results of queries
; if people with both lower and higher ordered names lived near Alyssa P. Hacker
; regardless of whatever order you use

(all-lives-near ?person (Hacker Alyssa P))
; people with lower salaries who live near Alyssa

(all-lives-near (Hacker Alyssa P) ?person)
; people with higher salaries who live near Alyssa
