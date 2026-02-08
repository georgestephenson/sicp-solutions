; exercise 4.62

; setup microshaft database and query driver loop

(load "resources/ch4-query.scm")

(initialize-data-base microshaft-data-base)

(query-driver-loop)

; assert last-pair rule

(assert!
  (rule (last-pair (?x) (?x))))

(assert! 
  (rule (last-pair (?z . ?y) ?x)
        (last-pair ?y ?x)))

; test

;(last-pair (3) ?x)
;;; Query results:
;(last-pair (3) (3))

;;; Query input:
;(last-pair (1 2 3) ?x)
;;; Query results:
;(last-pair (1 2 3) (3))

;;; Query input:
;(last-pair (2 ?x) (3))
;;; Query results:
;(last-pair (2 3) (3))

;;; Query input:
(last-pair ?x (3))
;;; Query results:
; runs forever...
; This last query would have an infinite number of solutions