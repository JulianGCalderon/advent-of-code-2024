#! /usr/bin/env racket
#lang racket

(require racket/runtime-path)
(define-runtime-path example1 "example1.data")
(define-runtime-path example2 "example2.data")
(define-runtime-path full "full.data")

(define (load path) (call-with-input-file path parse ) )
(define (parse port) (port->string port) )

(define re1 #px"mul\\((\\d+),(\\d+)\\)")

(define (solve1 path)
  (for/sum ([mul (regexp-match* re1 (load path)
                                #:match-select cdr )])
    (apply * (map string->number mul))
    )
  )

(solve1 example1)
(solve1 full)


; (define (solve2 path) #t)
; (solve2 example2)
; (solve2 full)
