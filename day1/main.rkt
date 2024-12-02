#! /usr/bin/env racket
#lang racket

(require racket/runtime-path)
(define-runtime-path example "example.data")
(define-runtime-path full "full.data")

(define (unzip lsts) (apply map list lsts ) )

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

(define (solve1 path)
  (let* (
         [input (load path)]
         [lists (unzip input)]
         [sorted (map (curryr sort <) lists)]
         [distances (apply map (compose abs -) sorted)]
         )
    (apply + distances)
    ))

(define (solve2 path)
  (match-let* (
               [input (load path)]
               [(list a b) (unzip input)]
               )
    (for/sum ([a a] )
      (* a (count (curry = a) b ))
      )))

(solve1 example)
(solve1 full)
(solve2 example)
(solve2 full)

