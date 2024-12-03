#! /usr/bin/env racket
#lang racket

(require racket/runtime-path)
(define-runtime-path example "example.data")
(define-runtime-path full "full.data")

(define (load path) (call-with-input-file path parse ) )
(define (parse port)
  (map
   (compose
    (curry map string->number)
    string-split
    )
   (port->lines port)
   )
  )

(define (pair-andmap proc lst) ( andmap proc (drop-right lst 1) (cdr lst) ) )
(define (between a c b) (and (<= a b) (<= b c) ) )

(define (solve1 path) (count identity (map is-safe (load path) ) ) )

(define (is-safe report)
  (and
   (or (pair-andmap < report) (pair-andmap > report) )
   (pair-andmap (compose (curry between 1 3) abs -) report)
   )
  )

(define (solve2 path) (count identity (map is-safeable (load path) ) ) )

(define (is-safeable report)
  (for/or ( [x (range (length report))] )
    (define l (take report x))
    (define r (cdr (drop report x)))
    (define subreport (append l r))
    (is-safe subreport)
    )
  )


(solve1 example)
(solve1 full)
(solve2 example)
(solve2 full)
