; exercise 4.50

(load "resources/ch4-ambeval.scm")

; implement ramb

(define (ramb? exp) (tagged-list? exp 'ramb))
(define (ramb-choices exp) (cdr exp))

(define (analyze exp)
  (cond ((self-evaluating? exp) 
         (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((let? exp) (analyze (let->combination exp)))
        ((amb? exp) (analyze-amb exp))
        ((ramb? exp) (analyze-ramb exp))
        ((application? exp) (analyze-application exp))
        (else
         (error "Unknown expression type -- ANALYZE" exp))))

(define (random-elt lst)
  (list-ref lst (random (length lst))))

(define (remove-first item lst)
  (cond ((null? lst) '())
        ((equal? item (car lst)) (cdr lst))
        (else (cons (car lst)
                    (remove-first item (cdr lst))))))

(define (shuffle lst)
  (if (null? lst)
      '()
      (let ((x (random-elt lst)))
        (cons x
              (shuffle (remove-first x lst))))))

(define (analyze-ramb exp)
  (let ((cprocs (map analyze (shuffle (ramb-choices exp)))))
    (lambda (env succeed fail)
      (define (try-next choices)
        (if (null? choices)
            (fail)
            ((car choices) env
                           succeed
                           (lambda ()
                             (try-next (cdr choices))))))
      (try-next cprocs))))

; run same changes as exercise 4.49
; except swap amb for ramb
; because we randomly choose choices, it means Alyssa won't have the problem
; the ambeval "gets stuck" in the same recursion with very similar sentences 
; (one word difference between each result)

(define the-global-environment (setup-environment))
(driver-loop)

(define (require p)
  (if (not p) (amb)))

(define nouns '(noun student professor cat class))

(define verbs '(verb studies lectures eats talks))

(define articles '(article the a))

(define (random-word lst)
  (list-ref lst (random (length lst))))

(define (parse-word word-list)
  (let ((found-word (random-word (cdr word-list))))
    (list (car word-list) found-word)))

(define (parse)
  (let ((sent (parse-sentence)))
    sent))

(define prepositions '(prep for to in by with))

(define (parse-prepositional-phrase)
  (list 'prep-phrase
        (parse-word prepositions)
        (parse-noun-phrase)))

(define (parse-sentence)
  (list 'sentence
         (parse-noun-phrase)
         (parse-verb-phrase)))

; we need a random chance to stop extending, or it will infinitely loop.
; see exercise 4.45 and 4.46
(define (parse-verb-phrase)
  (define (extend vp)
    (ramb
     vp
     (begin
       (require (< (random 2) 1)) ; 50% chance to stop
       (extend
        (list 'verb-phrase
              vp
              (parse-prepositional-phrase))))))
  (extend (parse-word verbs)))

(define (parse-simple-noun-phrase)
  (list 'simple-noun-phrase
        (parse-word articles)
        (parse-word nouns)))

(define (parse-noun-phrase)
  (define (extend np)
    (ramb
     np
     (begin
       (require (< (random 2) 1)) ; 50% chance to stop
       (extend
        (list 'noun-phrase
              np
              (parse-prepositional-phrase))))))
  (extend (parse-simple-noun-phrase)))

;;; Amb-Eval input:
(parse)
;;; Starting a new problem 
;;; Amb-Eval value:
;(sentence (noun-phrase (noun-phrase (simple-noun-phrase (article the) (noun professor)) (prep-phrase (prep by) (simple-noun-phrase (article a) (noun professor)))) (prep-phrase (prep in) (noun-phrase (noun-phrase (noun-phrase (simple-noun-phrase (article a) (noun professor)) (prep-phrase (prep with) (noun-phrase (simple-noun-phrase (article the) (noun cat)) (prep-phrase (prep by) (noun-phrase (noun-phrase (simple-noun-phrase (article the) (noun class)) (prep-phrase (prep by) (simple-noun-phrase (article the) (noun class)))) (prep-phrase (prep for) (noun-phrase (noun-phrase (simple-noun-phrase (article the) (noun professor)) (prep-phrase (prep by) (simple-noun-phrase (article the) (noun professor)))) (prep-phrase (prep by) (noun-phrase (simple-noun-phrase (article the) (noun class)) (prep-phrase (prep for) (simple-noun-phrase (article the) (noun cat)))))))))))) (prep-phrase (prep to) (noun-phrase (simple-noun-phrase (article a) (noun student)) (prep-phrase (prep for) (noun-phrase (simple-noun-phrase (article the) (noun class)) (prep-phrase (prep to) (simple-noun-phrase (article a) (noun cat)))))))) (prep-phrase (prep by) (noun-phrase (simple-noun-phrase (article a) (noun professor)) (prep-phrase (prep to) (simple-noun-phrase (article the) (noun cat)))))))) (verb lectures))

;;; Amb-Eval input:
try-again
;;; Amb-Eval value:
;(sentence (noun-phrase (noun-phrase (noun-phrase (noun-phrase (noun-phrase (simple-noun-phrase (article the) (noun professor)) (prep-phrase (prep by) (simple-noun-phrase (article a) (noun professor)))) (prep-phrase (prep in) (noun-phrase (noun-phrase (noun-phrase (simple-noun-phrase (article a) (noun professor)) (prep-phrase (prep with) (noun-phrase (simple-noun-phrase (article the) (noun cat)) (prep-phrase (prep by) (noun-phrase (noun-phrase (simple-noun-phrase (article the) (noun class)) (prep-phrase (prep by) (simple-noun-phrase (article the) (noun class)))) (prep-phrase (prep for) (noun-phrase (noun-phrase (simple-noun-phrase (article the) (noun professor)) (prep-phrase (prep by) (simple-noun-phrase (article the) (noun professor)))) (prep-phrase (prep by) (noun-phrase (simple-noun-phrase (article the) (noun class)) (prep-phrase (prep for) (simple-noun-phrase (article the) (noun cat)))))))))))) (prep-phrase (prep to) (noun-phrase (simple-noun-phrase (article a) (noun student)) (prep-phrase (prep for) (noun-phrase (simple-noun-phrase (article the) (noun class)) (prep-phrase (prep to) (simple-noun-phrase (article a) (noun cat)))))))) (prep-phrase (prep by) (simple-noun-phrase (article a) (noun professor)))))) (prep-phrase (prep with) (noun-phrase (simple-noun-phrase (article the) (noun professor)) (prep-phrase (prep in) (simple-noun-phrase (article a) (noun cat)))))) (prep-phrase (prep with) (simple-noun-phrase (article the) (noun professor)))) (prep-phrase (prep for) (noun-phrase (simple-noun-phrase (article a) (noun cat)) (prep-phrase (prep for) (noun-phrase (simple-noun-phrase (article the) (noun professor)) (prep-phrase (prep for) (simple-noun-phrase (article a) (noun cat)))))))) (verb lectures))

;;; Amb-Eval input:
try-again
;;; Amb-Eval value:
;(sentence (noun-phrase (noun-phrase (noun-phrase (noun-phrase (noun-phrase (simple-noun-phrase (article the) (noun professor)) (prep-phrase (prep by) (simple-noun-phrase (article a) (noun professor)))) (prep-phrase (prep in) (noun-phrase (noun-phrase (noun-phrase (simple-noun-phrase (article a) (noun professor)) (prep-phrase (prep with) (noun-phrase (simple-noun-phrase (article the) (noun cat)) (prep-phrase (prep by) (noun-phrase (noun-phrase (simple-noun-phrase (article the) (noun class)) (prep-phrase (prep by) (simple-noun-phrase (article the) (noun class)))) (prep-phrase (prep for) (noun-phrase (noun-phrase (simple-noun-phrase (article the) (noun professor)) (prep-phrase (prep by) (simple-noun-phrase (article the) (noun professor)))) (prep-phrase (prep by) (noun-phrase (simple-noun-phrase (article the) (noun class)) (prep-phrase (prep for) (simple-noun-phrase (article the) (noun cat)))))))))))) (prep-phrase (prep to) (noun-phrase (simple-noun-phrase (article a) (noun student)) (prep-phrase (prep for) (noun-phrase (simple-noun-phrase (article the) (noun class)) (prep-phrase (prep to) (simple-noun-phrase (article a) (noun cat)))))))) (prep-phrase (prep by) (simple-noun-phrase (article a) (noun professor)))))) (prep-phrase (prep with) (noun-phrase (simple-noun-phrase (article the) (noun professor)) (prep-phrase (prep in) (simple-noun-phrase (article a) (noun cat)))))) (prep-phrase (prep with) (simple-noun-phrase (article the) (noun professor)))) (prep-phrase (prep for) (noun-phrase (simple-noun-phrase (article a) (noun cat)) (prep-phrase (prep for) (noun-phrase (simple-noun-phrase (article the) (noun professor)) (prep-phrase (prep for) (simple-noun-phrase (article a) (noun cat)))))))) (verb-phrase (verb lectures) (prep-phrase (prep with) (simple-noun-phrase (article the) (noun professor)))))

;;; Amb-Eval input:
try-again
;;; Amb-Eval value:
;(sentence (noun-phrase (noun-phrase (noun-phrase (noun-phrase (noun-phrase (simple-noun-phrase (article the) (noun professor)) (prep-phrase (prep by) (simple-noun-phrase (article a) (noun professor)))) (prep-phrase (prep in) (noun-phrase (noun-phrase (noun-phrase (simple-noun-phrase (article a) (noun professor)) (prep-phrase (prep with) (noun-phrase (simple-noun-phrase (article the) (noun cat)) (prep-phrase (prep by) (noun-phrase (noun-phrase (simple-noun-phrase (article the) (noun class)) (prep-phrase (prep by) (simple-noun-phrase (article the) (noun class)))) (prep-phrase (prep for) (noun-phrase (noun-phrase (simple-noun-phrase (article the) (noun professor)) (prep-phrase (prep by) (simple-noun-phrase (article the) (noun professor)))) (prep-phrase (prep by) (noun-phrase (simple-noun-phrase (article the) (noun class)) (prep-phrase (prep for) (simple-noun-phrase (article the) (noun cat)))))))))))) (prep-phrase (prep to) (noun-phrase (simple-noun-phrase (article a) (noun student)) (prep-phrase (prep for) (noun-phrase (simple-noun-phrase (article the) (noun class)) (prep-phrase (prep to) (simple-noun-phrase (article a) (noun cat)))))))) (prep-phrase (prep by) (simple-noun-phrase (article a) (noun professor)))))) (prep-phrase (prep with) (noun-phrase (simple-noun-phrase (article the) (noun professor)) (prep-phrase (prep in) (simple-noun-phrase (article a) (noun cat)))))) (prep-phrase (prep with) (simple-noun-phrase (article the) (noun professor)))) (prep-phrase (prep for) (noun-phrase (simple-noun-phrase (article a) (noun cat)) (prep-phrase (prep for) (noun-phrase (simple-noun-phrase (article the) (noun professor)) (prep-phrase (prep for) (simple-noun-phrase (article a) (noun cat)))))))) (verb-phrase (verb-phrase (verb lectures) (prep-phrase (prep with) (simple-noun-phrase (article the) (noun professor)))) (prep-phrase (prep in) (simple-noun-phrase (article the) (noun class)))))

;;; Amb-Eval input:
try-again
;;; Amb-Eval value:
;(sentence (noun-phrase (noun-phrase (noun-phrase (noun-phrase (noun-phrase (simple-noun-phrase (article the) (noun professor)) (prep-phrase (prep by) (simple-noun-phrase (article a) (noun professor)))) (prep-phrase (prep in) (noun-phrase (noun-phrase (noun-phrase (simple-noun-phrase (article a) (noun professor)) (prep-phrase (prep with) (noun-phrase (simple-noun-phrase (article the) (noun cat)) (prep-phrase (prep by) (noun-phrase (noun-phrase (simple-noun-phrase (article the) (noun class)) (prep-phrase (prep by) (simple-noun-phrase (article the) (noun class)))) (prep-phrase (prep for) (noun-phrase (noun-phrase (simple-noun-phrase (article the) (noun professor)) (prep-phrase (prep by) (simple-noun-phrase (article the) (noun professor)))) (prep-phrase (prep by) (noun-phrase (simple-noun-phrase (article the) (noun class)) (prep-phrase (prep for) (simple-noun-phrase (article the) (noun cat)))))))))))) (prep-phrase (prep to) (noun-phrase (simple-noun-phrase (article a) (noun student)) (prep-phrase (prep for) (noun-phrase (simple-noun-phrase (article the) (noun class)) (prep-phrase (prep to) (simple-noun-phrase (article a) (noun cat)))))))) (prep-phrase (prep by) (simple-noun-phrase (article a) (noun professor)))))) (prep-phrase (prep with) (noun-phrase (simple-noun-phrase (article the) (noun professor)) (prep-phrase (prep in) (simple-noun-phrase (article a) (noun cat)))))) (prep-phrase (prep with) (simple-noun-phrase (article the) (noun professor)))) (prep-phrase (prep for) (noun-phrase (simple-noun-phrase (article a) (noun cat)) (prep-phrase (prep for) (noun-phrase (simple-noun-phrase (article the) (noun professor)) (prep-phrase (prep for) (simple-noun-phrase (article a) (noun cat)))))))) (verb-phrase (verb-phrase (verb-phrase (verb lectures) (prep-phrase (prep with) (simple-noun-phrase (article the) (noun professor)))) (prep-phrase (prep in) (simple-noun-phrase (article the) (noun class)))) (prep-phrase (prep with) (noun-phrase (simple-noun-phrase (article a) (noun cat)) (prep-phrase (prep in) (noun-phrase (noun-phrase (noun-phrase (noun-phrase (simple-noun-phrase (article the) (noun class)) (prep-phrase (prep in) (simple-noun-phrase (article a) (noun cat)))) (prep-phrase (prep with) (noun-phrase (noun-phrase (simple-noun-phrase (article the) (noun professor)) (prep-phrase (prep to) (simple-noun-phrase (article the) (noun professor)))) (prep-phrase (prep for) (simple-noun-phrase (article a) (noun class)))))) (prep-phrase (prep in) (simple-noun-phrase (article the) (noun class)))) (prep-phrase (prep with) (noun-phrase (simple-noun-phrase (article a) (noun cat)) (prep-phrase (prep to) (noun-phrase (noun-phrase (simple-noun-phrase (article the) (noun professor)) (prep-phrase (prep in) (simple-noun-phrase (article the) (noun cat)))) (prep-phrase (prep for) (noun-phrase (noun-phrase (noun-phrase (simple-noun-phrase (article the) (noun cat)) (prep-phrase (prep by) (noun-phrase (noun-phrase (simple-noun-phrase (article a) (noun cat)) (prep-phrase (prep with) (simple-noun-phrase (article a) (noun class)))) (prep-phrase (prep by) (noun-phrase (simple-noun-phrase (article the) (noun student)) (prep-phrase (prep to) (simple-noun-phrase (article a) (noun student)))))))) (prep-phrase (prep to) (noun-phrase (noun-phrase (noun-phrase (simple-noun-phrase (article a) (noun professor)) (prep-phrase (prep to) (noun-phrase (simple-noun-phrase (article the) (noun professor)) (prep-phrase (prep for) (noun-phrase (simple-noun-phrase (article the) (noun class)) (prep-phrase (prep in) (noun-phrase (noun-phrase (simple-noun-phrase (article the) (noun class)) (prep-phrase (prep for) (noun-phrase (noun-phrase (noun-phrase (noun-phrase (simple-noun-phrase (article the) (noun cat)) (prep-phrase (prep by) (simple-noun-phrase (article a) (noun class)))) (prep-phrase (prep by) (simple-noun-phrase (article the) (noun student)))) (prep-phrase (prep by) (noun-phrase (simple-noun-phrase (article a) (noun cat)) (prep-phrase (prep by) (noun-phrase (simple-noun-phrase (article the) (noun cat)) (prep-phrase (prep in) (simple-noun-phrase (article a) (noun class)))))))) (prep-phrase (prep to) (simple-noun-phrase (article the) (noun class)))))) (prep-phrase (prep to) (simple-noun-phrase (article the) (noun professor)))))))))) (prep-phrase (prep by) (simple-noun-phrase (article the) (noun class)))) (prep-phrase (prep by) (noun-phrase (simple-noun-phrase (article a) (noun class)) (prep-phrase (prep to) (noun-phrase (simple-noun-phrase (article the) (noun student)) (prep-phrase (prep for) (simple-noun-phrase (article a) (noun class)))))))))) (prep-phrase (prep with) (simple-noun-phrase (article a) (noun class)))))))))))))))

;;; Amb-Eval input:
try-again
;;; Amb-Eval value:
;(sentence (noun-phrase (noun-phrase (noun-phrase (noun-phrase (noun-phrase (simple-noun-phrase (article the) (noun professor)) (prep-phrase (prep by) (simple-noun-phrase (article a) (noun professor)))) (prep-phrase (prep in) (noun-phrase (noun-phrase (noun-phrase (simple-noun-phrase (article a) (noun professor)) (prep-phrase (prep with) (noun-phrase (simple-noun-phrase (article the) (noun cat)) (prep-phrase (prep by) (noun-phrase (noun-phrase (simple-noun-phrase (article the) (noun class)) (prep-phrase (prep by) (simple-noun-phrase (article the) (noun class)))) (prep-phrase (prep for) (noun-phrase (noun-phrase (simple-noun-phrase (article the) (noun professor)) (prep-phrase (prep by) (simple-noun-phrase (article the) (noun professor)))) (prep-phrase (prep by) (noun-phrase (simple-noun-phrase (article the) (noun class)) (prep-phrase (prep for) (simple-noun-phrase (article the) (noun cat)))))))))))) (prep-phrase (prep to) (noun-phrase (simple-noun-phrase (article a) (noun student)) (prep-phrase (prep for) (noun-phrase (simple-noun-phrase (article the) (noun class)) (prep-phrase (prep to) (simple-noun-phrase (article a) (noun cat)))))))) (prep-phrase (prep by) (simple-noun-phrase (article a) (noun professor)))))) (prep-phrase (prep with) (noun-phrase (simple-noun-phrase (article the) (noun professor)) (prep-phrase (prep in) (simple-noun-phrase (article a) (noun cat)))))) (prep-phrase (prep with) (simple-noun-phrase (article the) (noun professor)))) (prep-phrase (prep for) (noun-phrase (simple-noun-phrase (article a) (noun cat)) (prep-phrase (prep for) (noun-phrase (simple-noun-phrase (article the) (noun professor)) (prep-phrase (prep for) (simple-noun-phrase (article a) (noun cat)))))))) (verb-phrase (verb-phrase (verb-phrase (verb lectures) (prep-phrase (prep with) (simple-noun-phrase (article the) (noun professor)))) (prep-phrase (prep in) (simple-noun-phrase (article the) (noun class)))) (prep-phrase (prep with) (noun-phrase (simple-noun-phrase (article a) (noun cat)) (prep-phrase (prep in) (noun-phrase (noun-phrase (noun-phrase (noun-phrase (simple-noun-phrase (article the) (noun class)) (prep-phrase (prep in) (simple-noun-phrase (article a) (noun cat)))) (prep-phrase (prep with) (noun-phrase (noun-phrase (simple-noun-phrase (article the) (noun professor)) (prep-phrase (prep to) (simple-noun-phrase (article the) (noun professor)))) (prep-phrase (prep for) (simple-noun-phrase (article a) (noun class)))))) (prep-phrase (prep in) (simple-noun-phrase (article the) (noun class)))) (prep-phrase (prep with) (noun-phrase (simple-noun-phrase (article a) (noun cat)) (prep-phrase (prep to) (noun-phrase (noun-phrase (simple-noun-phrase (article the) (noun professor)) (prep-phrase (prep in) (simple-noun-phrase (article the) (noun cat)))) (prep-phrase (prep for) (noun-phrase (noun-phrase (simple-noun-phrase (article the) (noun cat)) (prep-phrase (prep by) (noun-phrase (noun-phrase (simple-noun-phrase (article a) (noun cat)) (prep-phrase (prep with) (simple-noun-phrase (article a) (noun class)))) (prep-phrase (prep by) (noun-phrase (simple-noun-phrase (article the) (noun student)) (prep-phrase (prep to) (simple-noun-phrase (article a) (noun student)))))))) (prep-phrase (prep to) (noun-phrase (noun-phrase (noun-phrase (simple-noun-phrase (article a) (noun professor)) (prep-phrase (prep to) (noun-phrase (simple-noun-phrase (article the) (noun professor)) (prep-phrase (prep for) (noun-phrase (simple-noun-phrase (article the) (noun class)) (prep-phrase (prep in) (noun-phrase (noun-phrase (simple-noun-phrase (article the) (noun class)) (prep-phrase (prep for) (noun-phrase (noun-phrase (noun-phrase (noun-phrase (simple-noun-phrase (article the) (noun cat)) (prep-phrase (prep by) (simple-noun-phrase (article a) (noun class)))) (prep-phrase (prep by) (simple-noun-phrase (article the) (noun student)))) (prep-phrase (prep by) (noun-phrase (simple-noun-phrase (article a) (noun cat)) (prep-phrase (prep by) (noun-phrase (simple-noun-phrase (article the) (noun cat)) (prep-phrase (prep in) (simple-noun-phrase (article a) (noun class)))))))) (prep-phrase (prep to) (simple-noun-phrase (article the) (noun class)))))) (prep-phrase (prep to) (simple-noun-phrase (article the) (noun professor)))))))))) (prep-phrase (prep by) (simple-noun-phrase (article the) (noun class)))) (prep-phrase (prep by) (noun-phrase (simple-noun-phrase (article a) (noun class)) (prep-phrase (prep to) (noun-phrase (simple-noun-phrase (article the) (noun student)) (prep-phrase (prep for) (simple-noun-phrase (article a) (noun class)))))))))))))))))))))
