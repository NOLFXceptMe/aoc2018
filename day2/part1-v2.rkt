#lang racket

(define input (file->lines "day2.input"))

(define char->index
  (lambda (ch)
    (- (char->integer ch) (char->integer #\a))))

(define get-counts
  (lambda (word)
    (let ([counter (make-vector 26)])
      (for ([i (map char->index (string->list word))])
           (vector-set! counter i (+ 1 (vector-ref counter i))))
      counter)))

(define counts (map get-counts input))

(define has-n?
  (lambda (n)
    (lambda (counts)
      (vector-member n counts))))

(define count-n
  (lambda (n)
    (length (filter (has-n? n) counts))))

(println (* (count-n 2) (count-n 3)))
