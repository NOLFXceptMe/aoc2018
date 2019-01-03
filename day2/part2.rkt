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
            (string->list (car word-pair))
            (string->list (cadr word-pair)))
          word-pair)))


(define distances (map distance paired-words))
(define filtered-list
  (filter (lambda (l) (= (car l) 1)) distances))

(define get-common-chars
  (lambda (w1 w2)
    (if (empty? w1)
      '()
      (let ([c1 (car w1)]
            [c2 (car w2)])
        (if (eq? c1 c2)
          (cons c1 (get-common-chars (cdr w1) (cdr w2)))
          (get-common-chars (cdr w1) (cdr w2)))))))

(define construct-word
  (lambda (word-pair)
    (let ([w1 (car word-pair)]
          [w2 (cadr word-pair)])
      (list->string (get-common-chars (string->list w1) (string->list w2))))))

(println (construct-word (cadar filtered-list)))
