#lang racket

(define input (file->lines "day2.input"))

(define encode-char
  (lambda (char)
    (let ([index (- (char->integer char) (char->integer #\a))])
      (arithmetic-shift 1 (* 3 index)))))

(define encode-word
  (lambda (word)
    (let ([chars (string->list word)])
      (foldl + 0 (map encode-char chars)))))

(define encoded-words (map encode-word input))

(define indices (range 0 26))

(define extract
  (lambda (number index)
    (bitwise-and
      number
      (+ (arithmetic-shift 1 (* 3 index))
         (arithmetic-shift 1 (+ 1 (* 3 index)))
         (arithmetic-shift 1 (+ 2 (* 3 index)))))))

(define reduce
  (lambda (number index)
    (arithmetic-shift number (* -3 index))))

(define has-at-i?
  (lambda (encoded-word number)
    (lambda (i)
      (bitwise-xor (reduce (extract encoded-word i) i) number))))

(define has? (lambda (number)
  (lambda (encoded-word)
    (if (member 0 (map (has-at-i? encoded-word number) indices))
      1 0)))
  )

(define words (lambda (number)
  (foldl + 0 (map (has? number) encoded-words))))

(println (list (words 2) (words 3)))
