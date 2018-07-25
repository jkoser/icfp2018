#lang racket

(require "model.rkt")
(require "strategy.rkt")
(require "planning.rkt")
(require "trace.rkt")

(define num-seeds 39)
(define num-assembly-problems 186)
(define num-disassembly-problems 186)
(define num-reassembly-problems 115)

(define strategy strategy-balanced-slices)

(for ((n (in-range 1 (+ num-assembly-problems 1))))
  (let* ((target-filename
           (format "problemsF/FA~a_tgt.mdl"
                   (~a n #:width 3 #:align 'right #:pad-string "0")))
         (target-model (load-model target-filename))
         (res (model-res target-model))
         (source-model (create-model res))
         (plan (strategy num-seeds source-model target-model))
         (trace (compile-plan plan source-model target-model num-seeds))
         (trace-filename
           (format "solnsF/FA~a_soln.nbt"
                   (~a n #:width 3 #:align 'right #:pad-string "0"))))
    (save-trace! trace-filename trace)))
