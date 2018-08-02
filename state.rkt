#lang typed/racket

(require "coords.rkt")
(require "model.rkt")
(require "trace.rkt")

(provide (all-defined-out))

(define-struct bot
  ([bid : Natural]
   [pos : Coord]
   [seeds : (Listof Natural)])
  #:transparent #:mutable)

(: create-first-bot (-> Natural bot))
(define (create-first-bot n)
  (make-bot 1
            (c 0 0 0)
            (range 2 (+ n 2))))

(: bot-fission! (-> bot Coord Natural bot))
(define (bot-fission! bot pos2 m)
  (let* ((seeds (sort (bot-seeds bot) <))
         (bot2 (make-bot (first seeds)
                         pos2
                         (take (rest seeds) m))))
    (set-bot-seeds! bot (drop seeds (+ m 1)))
    bot2))

(: bot-fusion! (-> bot bot Void))
(define (bot-fusion! bot bot2)
  (set-bot-seeds! bot (append (bot-seeds bot)
                              (list (bot-bid bot2))
                              (bot-seeds bot2))))

(define-struct system
  ([energy : Integer]
   [harmonics : (Union 'high 'low)]
   [model : model]
   [bots : (Listof bot)]
   [trace : Trace]
   [time : Natural])
  #:transparent #:mutable)

(: create-system (-> Res Natural Trace system))
(define (create-system res num-seeds trace)
  (make-system 0
               'low
               (create-model res)
               (list (create-first-bot num-seeds))
               trace
               0))

(: system-add-energy! (-> system Integer Void))
(define (system-add-energy! s x)
  (set-system-energy! s (+ (system-energy s) x)))
