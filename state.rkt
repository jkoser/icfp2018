#lang racket

(require "coords.rkt")
(require "model.rkt")

(provide (all-defined-out))

(define-struct bot
  (bid pos seeds)
  #:transparent #:mutable)

(define-struct system
  (energy harmonics model bots trace time)
  #:transparent #:mutable)

(define (create-system r n)
  (make-system 0
               'low
               (create-model r)
               (list (make-bot 1
                               (make-c 0 0 0)
                               (range 2 (+ n 1))))
               '()
               0))
