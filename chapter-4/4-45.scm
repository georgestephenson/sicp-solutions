; exercise 4.45

(load "resources/ch4-ambeval.scm")

(define the-global-environment (setup-environment))
(driver-loop)

(define (require p)
  (if (not p) (amb)))

(define nouns '(noun student professor cat class))

(define verbs '(verb studies lectures eats sleeps))

(define articles '(article the a))

(define (parse-word word-list)
  (require (not (null? *unparsed*)))
  (require (memq (car *unparsed*) (cdr word-list)))
  (let ((found-word (car *unparsed*)))
    (set! *unparsed* (cdr *unparsed*))
    (list (car word-list) found-word)))

(define *unparsed* '())

(define (parse input)
  (set! *unparsed* input)
  (let ((sent (parse-sentence)))
    (require (null? *unparsed*))
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

;;; Amb-Eval input:
(parse '(the professor lectures to the student in the class with the cat))
;;; Starting a new problem 
;;; Amb-Eval value:
(sentence (simple-noun-phrase (article the) (noun professor)) 
          (verb-phrase (verb-phrase (verb-phrase (verb lectures) 
                                                 (prep-phrase (prep to) 
                                                              (simple-noun-phrase (article the) (noun student)))) 
                                    (prep-phrase (prep in) (simple-noun-phrase (article the) (noun class)))) 
                       (prep-phrase (prep with) (simple-noun-phrase (article the) (noun cat)))))

; explanation: the professor is lecturing with the cat, lecturing in the class, lecturing to the student

;;; Amb-Eval input:
try-again
;;; Amb-Eval value:
(sentence (simple-noun-phrase (article the) (noun professor)) 
          (verb-phrase (verb-phrase (verb lectures) 
                                    (prep-phrase (prep to) 
                                                 (simple-noun-phrase (article the) (noun student)))) 
                       (prep-phrase (prep in) 
                                    (noun-phrase (simple-noun-phrase (article the) (noun class)) 
                                                 (prep-phrase (prep with) 
                                                              (simple-noun-phrase (article the) (noun cat)))))))

; explanation: the professor is lecturing in the class, lecturing to the student, the class has the cat

;;; Amb-Eval input:
try-again
;;; Amb-Eval value:
(sentence (simple-noun-phrase (article the) (noun professor)) 
          (verb-phrase (verb-phrase (verb lectures) 
                                    (prep-phrase (prep to) 
                                                 (noun-phrase (simple-noun-phrase (article the) (noun student)) 
                                                              (prep-phrase (prep in) 
                                                                           (simple-noun-phrase (article the) (noun class)))))) 
                       (prep-phrase (prep with) (simple-noun-phrase (article the) (noun cat)))))

; explanation: the professor is lecturing with the cat, to the student in the class

;;; Amb-Eval input:
try-again
;;; Amb-Eval value:
(sentence (simple-noun-phrase (article the) (noun professor)) 
          (verb-phrase (verb lectures) 
                       (prep-phrase (prep to) 
                                    (noun-phrase (noun-phrase (simple-noun-phrase (article the) (noun student)) 
                                                              (prep-phrase (prep in) (simple-noun-phrase (article the) (noun class)))) 
                                                 (prep-phrase (prep with) (simple-noun-phrase (article the) (noun cat)))))))

; explanation: the professor is lecturing to the student, the student is in the class, the student has the cat

;;; Amb-Eval input:
try-again
;;; Amb-Eval value:
(sentence (simple-noun-phrase (article the) (noun professor)) 
          (verb-phrase (verb lectures) 
                       (prep-phrase (prep to) 
                                    (noun-phrase (simple-noun-phrase (article the) (noun student)) 
                                                 (prep-phrase (prep in) 
                                                              (noun-phrase (simple-noun-phrase (article the) (noun class)) 
                                                                           (prep-phrase (prep with) 
                                                                                        (simple-noun-phrase (article the) (noun cat)))))))))

; explanation: the professor is lecturing to the student, the student is in the class, the class has the cat