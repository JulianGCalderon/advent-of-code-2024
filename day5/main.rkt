#! /usr/bin/env racket
#lang racket

(require racket/runtime-path)
(define-runtime-path example "example.data")
(define-runtime-path full "full.data")

(define (load path) (call-with-input-file path parse ) )
(define (parse port)
  (match-define (list rules-str updates-str)
    (string-split (port->string port) "\n\n") )

  (define rules (map (curryr string-split "|") (string-split rules-str)) )
  (define updates (map (curryr string-split ",") (string-split updates-str)) )

  (values rules updates)
  )

(define (in-order? rules update)
  (for/and ([rule rules]) (valid? rule update) )
  )

(define (valid? rule update)
  (match-define (list a b) rule)

  (match (member b update)
    [#f #t]
    [other (false? (member a other)) ]
    )
  )

(define (middle-number  update)
  (string->number (list-ref update (quotient (length update) 2)))
  )

(define (solve1 path)
  (define-values (rules updates) (load path))
  (apply + (map middle-number (filter (curry in-order? rules) updates)))
  )

(define (order rules update)
  (set! update
        (for/fold ([update update])
                  ([rule rules]
                   #:unless (valid? rule update))
          (fix rule update)
          )
        )

  (if (in-order? rules update)
      update
      (order rules update)
      )
  )

(define (fix rule update)
  (match-define (list a b) rule)

  (define b-idx (index-of update b))

  (move update a b-idx)
  )

(define (move lst v dst)
  (set! lst (remove v lst))

  (define-values (before after) (split-at lst dst))

  (append before (list v) after)
  )

(define (solve2 path)
  (define-values (rules updates) (load path))
  (define unordered (filter (compose not (curry in-order? rules)) updates))

  (define ordered (map (curry order rules) unordered))

  (apply + (map middle-number ordered))
  )



(solve1 example)
(solve1 full)
(solve2 example)
(solve2 full)
