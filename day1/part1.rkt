#lang racket

(define input (map string->number (file->lines "day1.input")))

(define result (foldl + 0 input))

(println result)
