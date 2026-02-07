; exercise 4.49

(load "resources/ch4-ambeval.scm")

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

(define (parse-verb-phrase)
  (define (maybe-extend verb-phrase)
    (amb verb-phrase
         (maybe-extend (list 'verb-phrase
                             verb-phrase
                             (parse-prepositional-phrase)))))
  (maybe-extend (parse-word verbs)))

(define (parse-simple-noun-phrase)
  (list 'simple-noun-phrase
        (parse-word articles)
        (parse-word nouns)))

(define (parse-noun-phrase)
  (define (maybe-extend noun-phrase)
    (amb noun-phrase
         (maybe-extend (list 'noun-phrase
                             noun-phrase
                             (parse-prepositional-phrase)))))
  (maybe-extend (parse-simple-noun-phrase)))


; I almost can't believe that the SICP textbook gives these words as examples to generate sentences from:
; student, professor, sleeps, with
; This could get bad pretty fast, so I changed "sleeps" to "talks"...

(parse)
;;; Starting a new problem 
;;; Amb-Eval value:
;(sentence (simple-noun-phrase (article the) (noun professor)) (verb talks))

;;; Amb-Eval input:
try-again
;;; Amb-Eval value:
;(sentence (simple-noun-phrase (article the) (noun professor)) (verb-phrase (verb talks) (prep-phrase (prep for) (simple-noun-phrase (article the) (noun professor)))))

;;; Amb-Eval input:
try-again
;;; Amb-Eval value:
;(sentence (simple-noun-phrase (article the) (noun professor)) (verb-phrase (verb-phrase (verb talks) (prep-phrase (prep for) (simple-noun-phrase (article the) (noun professor)))) (prep-phrase (prep by) (simple-noun-phrase (article a) (noun professor)))))

;;; Amb-Eval input:
try-again
;;; Amb-Eval value:
;(sentence (simple-noun-phrase (article the) (noun professor)) (verb-phrase (verb-phrase (verb-phrase (verb talks) (prep-phrase (prep for) (simple-noun-phrase (article the) (noun professor)))) (prep-phrase (prep by) (simple-noun-phrase (article a) (noun professor)))) (prep-phrase (prep for) (simple-noun-phrase (article a) (noun student)))))

;;; Amb-Eval input:
try-again
;;; Amb-Eval value:
;(sentence (simple-noun-phrase (article the) (noun professor)) (verb-phrase (verb-phrase (verb-phrase (verb-phrase (verb talks) (prep-phrase (prep for) (simple-noun-phrase (article the) (noun professor)))) (prep-phrase (prep by) (simple-noun-phrase (article a) (noun professor)))) (prep-phrase (prep for) (simple-noun-phrase (article a) (noun student)))) (prep-phrase (prep in) (simple-noun-phrase (article a) (noun professor)))))

;;; Amb-Eval input:
try-again
;;; Amb-Eval value:
;(sentence (simple-noun-phrase (article the) (noun professor)) (verb-phrase (verb-phrase (verb-phrase (verb-phrase (verb-phrase (verb talks) (prep-phrase (prep for) (simple-noun-phrase (article the) (noun professor)))) (prep-phrase (prep by) (simple-noun-phrase (article a) (noun professor)))) (prep-phrase (prep for) (simple-noun-phrase (article a) (noun student)))) (prep-phrase (prep in) (simple-noun-phrase (article a) (noun professor)))) (prep-phrase (prep with) (simple-noun-phrase (article the) (noun cat)))))