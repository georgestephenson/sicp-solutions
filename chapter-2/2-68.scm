(load "2-67.scm")

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (define (next-bit symbol tree)
    (cond ((leaf? tree) '())
          ((memq symbol (symbols (left-branch tree))) 
                (cons '0 (next-bit symbol (left-branch tree))))
          (else (cons '1 (next-bit symbol (right-branch tree))))))
  (cond ((null? symbol) '())
        ((not (memq symbol (symbols tree))) (error "symbol not in tree -- ENCODE-SYMBOL"))
        (else (next-bit symbol tree))))

memq
symbols

(encode '(a d a b b c a) sample-tree)
;Value: (0 1 1 0 0 1 0 1 0 1 1 1 0)