(load "2-69.scm")
(load "2-68.scm")

(define rock-lyric-tree
  (generate-huffman-tree '((NA 16) (YIP 9) (SHA 3) (A 2) (GET 2) (JOB 2) (BOOM 1) (WAH 1))))

(define song 
  '(GET A JOB
    SHA NA NA NA NA NA NA NA NA
    GET A JOB
    SHA NA NA NA NA NA NA NA NA
    WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP
    SHA BOOM))

(encode song rock-lyric-tree)
;Value: (1 1 1 1 1 1 1 0 0 1 1 1 1 0 1 1 1 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 0 0 1 1 1 1 0 1 1 1 0 0 0 0 0 0 0 0 0 1 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 1 1 0 1 1 0 1 1)

; Number of bits
(length (encode song rock-lyric-tree))
;Value: 84

; Bits required for a fixed length code of 3 bits per code in 8-symbol alphabet
(* (length song) 3)
;Value: 108