; exercise 4.68

(load "resources/ch4-query.scm")

(define reverse-data-base
  '(
(rule (append-to-form () ?y ?y))

(rule (append-to-form (?u . ?v) ?y (?u . ?z))
      (append-to-form ?v ?y ?z))

(rule (reverse () ()))

(rule (reverse (?first . ?rest) ?reversed)
      (and (reverse ?rest ?reversed-rest)
           (append-to-form ?reversed-rest (?first) ?reversed)))
))

(initialize-data-base reverse-data-base)

(query-driver-loop)

(reverse (1 2 3) ?x)

(reverse ?x (1 2 3))