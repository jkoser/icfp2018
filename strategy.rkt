#lang racket

(require "model.rkt")
(require "planning.rkt")

(provide strategy-assemble-in-slices)

;; A strategy is a function that accepts three arguments:
;;   1) the number of seeds available to the first bot
;;   2) a source model
;;   3) a target model
;; and returns a plan for transforming the source into the target.


;; Helper for strategy-assemble-in-slices
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

;; Divides the space into roughly equal-width slices and assigns a bot to each.
(define (strategy-assemble-in-slices num-seeds source target)
  `(,@(make-plan-slices num-seeds
                        1
                        (- (model-res source) 2)
                        (model-res source))
     (move-to (0 0 0))))
