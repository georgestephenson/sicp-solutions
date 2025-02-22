; get and put
(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)    
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))


;;;;;;;;;;;;
;; PART A ;;
;;;;;;;;;;;;

(define (get-record employee-id file)
  ((get 'get-record (get-type file)) employee-id (cdr file)))

(define (get-type file)
  (car file))

(define file-a
  (cons 'a 
    (list
      (list (cons 'id 'A1) (cons 'name "Albert Abrams") (cons 'address "4 Drury Lane") (cons 'salary 30000))
      (list (cons 'id 'A2) (cons 'name "Bruno Bernardson") (cons 'address "10 Park Road") (cons 'salary 37564))
      (list (cons 'id 'A3) (cons 'name "Charlie Cruise") (cons 'address "7 The Scheme") (cons 'salary 45000)))))

(define file-b
  (cons 'b 
    (list
      (cons (cons (cons 'id 'B1) (cons 'name "David Davids")) (cons (cons 'address "21 Story Park") (cons 'salary 31250)))
      (cons (cons (cons 'id 'B2) (cons 'name "Emma Evans")) (cons (cons 'address "11b Stepson Street") (cons 'salary 42500))))))

(define file-c
  (cons 'c 
    (list
      (list 'id 'C1 (list 'name "Fiona Freeman" (list 'address "99 Nova Nave" (list 'salary 99999))))
      (list 'id 'C2 (list 'name "Gordon Gates" (list 'address "1 Bermondsey Cross" (list 'salary 18750)))))))

(define (install-division-a)
  (define (get-record employee-id records)
    (cond ((null? records) #f)
          ((eq? employee-id (cdaar records)) (car records))
          (else (get-record employee-id (cdr records)))))
  (put 'get-record 'a get-record)
  'done)

(define (install-division-b)
  (define (transform-record record)
    (list (caar record) (cdar record) (cadr record) (cddr record)))
  (define (get-record employee-id records)
    (cond ((null? records) #f)
          ((eq? employee-id (cdaaar records))
            (transform-record (car records)))
          (else (get-record employee-id (cdr records)))))
  (put 'get-record 'b get-record)
  'done)

(define (install-division-c)
  (define (transform-record record)
    (list (cons (car record) (cadr record)) 
          (cons (car (caddr record)) (cadr (caddr record)))
          (cons (car (caddr (caddr record))) (cadr (caddr (caddr record))))
          (cons (car (caddr (caddr (caddr record)))) (cadr (caddr (caddr (caddr record)))))))
  (define (get-record employee-id records)
    (cond ((null? records) #f)
          ((eq? employee-id (cadar records)) 
            (transform-record (car records)))
          (else (get-record employee-id (cdr records)))))
  (put 'get-record 'c get-record)
  'done)

(install-division-a)
(install-division-b)
(install-division-c)

(get-record 'A2 file-a)
;Value: ((id . a2) (name . "Bruno Bernardson") (address . "10 Park Road") (salary . 37564))
(get-record 'B2 file-b)
;Value: ((id . b2) (name . "Emma Evans") (address . "11b Stepson Street") (salary . 42500))
(get-record 'C2 file-c)
;Value: ((id . c2) (name . "Gordon Gates") (address . "1 Bermondsey Cross") (salary . 18750))

;;;;;;;;;;;;
;; PART B ;;
;;;;;;;;;;;;

(define (get-field field record)
  (cond ((null? record) #f)
        ((eq? (caar record) field) (cdar record))
        (else (get-field field (cdr record)))))

(define (get-salary employee-id file)
  (let ((record (get-record employee-id file)))
    (get-field 'salary record)))

(get-salary 'A3 file-a)
;Value: 45000
(get-salary 'B1 file-b)
;Value: 31250
(get-salary 'C1 file-c)
;Value: 99999


;;;;;;;;;;;;
;; PART C ;;
;;;;;;;;;;;;

(define (find-employee-record employee-id files)
  (if (null? files)
      #f
      (let ((file (car files)))
        (let ((record (get-record employee-id file)))
          (if record
              record
              (find-employee-record employee-id (cdr files)))))))

(find-employee-record 'B2 (list file-c file-a file-b))
;Value: ((id . b2) (name . "Emma Evans") (address . "11b Stepson Street") (salary . 42500))