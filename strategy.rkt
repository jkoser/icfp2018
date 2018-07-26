#lang racket

(require "model.rkt")
(require "planning.rkt")

(provide strategy-assemble-in-slices
         strategy-balanced-slices)

;; A strategy is a function that accepts three arguments:
;;   1) the number of seeds available to the first bot
;;   2) a source model
;;   3) a target model
;; and returns a plan for transforming the source into the target.


;; Helper for strategy-balanced-slices
(define (make-plan-balanced n xmin xmax res weights)
  (if (or (= n 0) (= xmin xmax))
    '((assemble-in-lane top))
    (let* ((m (quotient (- n 1) 2))
           (bot-ratio (/ (- n m) (+ n 1)))
           (xmid (do ((i1 xmin)
                      (i2 xmax)
                      (w1 (vector-ref weights xmin))
                      (w2 (vector-ref weights xmax)))
                   ((= (+ i1 1) i2) i1)
                   (if (or (= w1 w2 0)
                           (< (/ w1 (+ w1 w2)) bot-ratio))
                     (begin
                       (set! i1 (+ i1 1))
                       (set! w1 (+ w1 (vector-ref weights i1))))
                     (begin
                       (set! i2 (- i2 1))
                       (set! w2 (+ w2 (vector-ref weights i2))))))))
      `((move-to (,xmid 0 0))
        (spawn (,(+ xmid 1) 0 0)
               ,m
               (,@(make-plan-balanced (- n m 1) xmin xmid res weights)
                (move-to (,xmid ,(- res 1) 0)))
               (,@(make-plan-balanced m (+ xmid 1) xmax res weights)
                (move-to (,(+ xmid 1) ,(- res 1) 0))))))))

;; Divides the space into roughly equal-weight slices and assigns a bot to each.
(define (strategy-balanced-slices num-seeds source target)
  (define res (model-res source))
  (define weights
    (for/vector ((i res))
      (for*/sum ((j res) (k res) #:when (model-voxel-full? target i j k))
                1)))
  `(,@(make-plan-balanced num-seeds 1 (- res 2) res weights)
     (move-to (0 0 0))))


;; Helper for strategy-assemble-in-slices
(define (make-plan-slices n xmin xmax res)
  (if (or (= n 0) (= xmin xmax))
    '((assemble-in-lane top))
    (let* ((m (quotient (- n 1) 2))
           (width (+ (- xmax xmin) 1))
           (left (floor (* width (/ (- n m) (+ n 1)))))
           (xmid (max xmin (min (+ xmin left -1) (- xmax 1)))))
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
