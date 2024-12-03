#! /usr/bin/env racket
#lang racket

(require racket/runtime-path)
(define-runtime-path example1 "example1.data")
(define-runtime-path example2 "example2.data")
(define-runtime-path full "full.data")

(define (load path) (call-with-input-file path parse ) )
(define (parse port) (string-append (port->string port) "do()"))

(define re1 #px"mul\\((\\d+),(\\d+)\\)")

(define (run input)
  (define filtered (regexp-match* re1 input #:match-select cdr ))
  (for/sum ([mul filtered]) (apply * (map string->number mul)) )
  )

(define (solve1 path) (run (load path)) )

(solve1 example1)
(solve1 full)

(define dont (regexp-quote "don't()"))
(define do (regexp-quote "do()"))
(define re2 (pregexp (string-append dont ".*?" do)))

(define (solve2 path) (run (regexp-replace* re2 (load path) "")) )

(solve2 example2)
(solve2 full)
