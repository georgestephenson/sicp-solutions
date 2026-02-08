; exercise 4.62

(load "resources/ch4-query.scm")

(define last-pair-data-base
  '(
(rule (last-pair (?x) (?x)))
(rule (last-pair (?z . ?y) ?x)
      (last-pair ?y ?x))
))


(initialize-data-base last-pair-data-base)

(query-driver-loop)

; test

;;; Query input:
(last-pair (3) ?x)
;;; Query results:
;(last-pair (3) (3))

;;; Query input:
(last-pair (1 2 3) ?x)
;;; Query results:
;(last-pair (1 2 3) (3))

;;; Query input:
(last-pair (2 ?x) (3))
;;; Query results:
;(last-pair (2 3) (3))

;;; Query input:
(last-pair ?x (3))
;;; Query results:
; runs forever...
; This last query would have an infinite number of solutions