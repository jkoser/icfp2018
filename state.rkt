#lang racket

(require "coords.rkt")
(require "model.rkt")

(provide (all-defined-out))

(define-struct bot
  (bid pos seeds)
  #:transparent #:mutable)

(define (create-first-bot n)
  (make-bot 1
            (make-c 0 0 0)
            (range 2 (+ n 2))))

(define (bot-fission! bot pos2 m)
  (let ((bot2 (make-bot (first (bot-seeds bot))
                        pos2
                        (take (rest (bot-seeds bot)) m))))
    (set-bot-seeds! bot (drop (bot-seeds bot) (+ m 1)))
    bot2))

(define (bot-fusion! bot bot2)
  (set-bot-seeds! bot (append (bot-seeds bot)
                              (list (bot-bid bot2))
                              (bot-seeds bot2))))

(define-struct system
  (energy harmonics model bots trace time)
  #:transparent #:mutable)

(define (create-system r n)
  (make-system 0
               'low
               (create-model r)
               (list (create-first-bot n))
               '()
               0))
