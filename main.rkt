#lang racket

(require "model.rkt")
(require "planning.rkt")
(require "trace.rkt")

(let* ((target-model (load-problem-model 1))
       (source-model (create-model (model-res target-model)))
       (plan '((assemble-in-lane top)
               (move-to (0 0 0))))
       (trace (compile-plan plan source-model target-model 19)))
  (save-trace! "out.nbt" trace))
