#lang racket

(define input (file->lines "day2.input"))

(define sorted-words (sort input string<?))

(define pair-words
  (lambda (l1 l2)
    (if (empty? l2)
      '()
      (cons (list (car l1) (car l2))
            (pair-words (cdr l1) (cdr l2))))))

(define paired-words (pair-words sorted-words (cdr sorted-words)))

; defined only for equal-length words
(define word-distance
  (lambda (w1 w2)
    (if (empty? w1)
      0
      (+ (word-distance (cdr w1) (cdr w2))
         (if (eq? (car w1) (car w2)) 0 1)))))

(define distance
  (lambda (word-pair)
    (list (word-distance
            (string->list (first word-pair))
            (string->list (second word-pair)))
          word-pair)))

(define distances (map distance paired-words))

(define box-id-pair
  (cadar (filter (lambda (l) (= (car l) 1)) distances)))

(define get-common-chars
  (lambda (w1 w2)
    (if (empty? w1)
      '()
      (let ([h1 (first w1)]
            [h2 (first w2)]
            [next-common (get-common-chars (rest w1) (rest w2))])
        (if (eq? h1 h2)
          (cons h1 next-common)
          next-common)))))

(define construct-word
  (lambda (w1 w2)
    (list->string (get-common-chars (string->list w1) (string->list w2)))))

(println (construct-word (first box-id-pair) (second box-id-pair)))
