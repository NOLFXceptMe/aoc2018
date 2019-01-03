#lang racket

(require srfi/1)

(define input (map string->number (file->lines "day1.input")))

(define input-circ (apply circular-list input))

(define freq-calc
  (lambda (in-list seen-freq current-freq)
    (let ([freq (+ current-freq (car in-list))])
      (if (hash-has-key? seen-freq freq)
        freq
        (freq-calc (cdr in-list) (hash-set seen-freq freq #t) freq)
        )
      )
    )
  )

(define result (freq-calc input-circ (hash) 0))

(println result)
