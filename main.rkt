#lang racket

(require "model.rkt")
(require "planning.rkt")
(require "trace.rkt")

(define (make-plan-slices n xmin xmax res)
  (when (< n 0)
    (error "negative n"))
  (if (or (= n 0) (= xmin xmax))
    '((assemble-in-lane top))
    (let* ((m (quotient (- n 1) 2))
           (width (+ (- xmax xmin) 1))
           (split (ceiling (* width (/ (- n m) (+ n 1)))))
           (xmid (+ xmin split -1)))
      `((move-to (,xmid 0 0))
        (spawn (,(+ xmid 1) 0 0)
               ,m
               (,@(make-plan-slices (- n m 1) xmin xmid res)
                (move-to (,xmid ,(- res 1) 0)))
               (,@(make-plan-slices m (+ xmid 1) xmax res)
                (move-to (,(+ xmid 1) ,(- res 1) 0))))))))


(let* ((target-model (load-problem-model 67))
       (res (model-res target-model))
       (source-model (create-model res))
       (plan `(,@(make-plan-slices 19 0 (- res 1) res)
               (move-to (0 0 0))))
       (_ (pretty-print plan))
       (trace (compile-plan plan source-model target-model 19)))
  (save-trace! "out.nbt" trace))
