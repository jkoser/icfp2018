#lang racket

(require "coords.rkt")
(require "model.rkt")
(require "planning.rkt")

(provide strategy-assemble-in-slices
         strategy-balanced-slices
         strategy-slice-and-dice)

;; A strategy is a function that accepts three arguments:
;;   1) the number of seeds available to the first bot
;;   2) a source model
;;   3) a target model
;; and returns a plan for transforming the source into the target.


;; Helper for strategy-slice-and-dice
(define (make-plan-slice-and-dice n lane res weights-xz)
  (if (or (= n 0) (< (dim lane) 2))
    '((assemble-in-lane top))
    ;; To avoid duplicating code for splits in the X and Z dimensions,
    ;; we establish a view, naming the appropriate dimensions S and T.
    ;; The "split" function produces spawn locations and child lanes.
    (let-values
      (((smin smax tmin tmax weights-view split)
        (if (>= (- (xmax lane) (xmin lane))
                (- (zmax lane) (zmin lane)))
          ;; split X dimension
          (values (xmin lane) (xmax lane) (zmin lane) (zmax lane)
                  (lambda (s t) (vector-ref (vector-ref weights-xz s) t))
                  (lambda (smid)
                    (values (make-c smid (ymin lane) (zmin lane))
                            (make-c (+ smid 1) (ymin lane) (zmin lane))
                            (region-below-x lane smid)
                            (region-above-x lane (+ smid 1)))))
          ;; split Z dimension
          (values (zmin lane) (zmax lane) (xmin lane) (xmax lane)
                  (lambda (s t) (vector-ref (vector-ref weights-xz t) s))
                  (lambda (smid)
                    (values (make-c (xmin lane) (ymin lane) smid)
                            (make-c (xmin lane) (ymin lane) (+ smid 1))
                            (region-below-z lane smid)
                            (region-above-z lane (+ smid 1))))))))
      (let* ((m (quotient (- n 1) 2))
             (bot-ratio (/ (- n m) (+ n 1)))
             (weights-sum (lambda (s)
                            (for/sum ((t (in-range tmin (+ tmax 1))))
                              (weights-view s t))))
             (smid (do ((i1 smin)
                        (i2 smax)
                        (w1 (weights-sum smin))
                        (w2 (weights-sum smax)))
                     ((= (+ i1 1) i2) i1)
                     (cond ((= w1 0)
                            (begin
                              (set! i1 (+ i1 1))
                              (set! w1 (+ w1 (weights-sum i1)))))
                           ((= w2 0)
                            (begin
                              (set! i2 (- i2 1))
                              (set! w2 (+ w2 (weights-sum i2)))))
                           (else
                             (let* ((w1-prime (weights-sum (+ i1 1)))
                                    (rat1 (/ (+ w1 w1-prime)
                                             (+ w1 w1-prime w2)))
                                    (w2-prime (weights-sum (- i2 1)))
                                    (rat2 (/ w1 (+ w1 w2 w2-prime))))
                               (if (< (abs (- (/ rat1 bot-ratio) 1))
                                      (abs (- (/ rat2 bot-ratio) 1)))
                                 (begin
                                   (set! i1 (+ i1 1))
                                   (set! w1 (+ w1 w1-prime)))
                                 (begin
                                   (set! i2 (- i2 1))
                                   (set! w2 (+ w2 w2-prime))))))))))
        (let-values
          (((pos1 pos2 lane1 lane2) (split smid)))
          `((move-to ,pos1)
            (spawn ,pos2
                   ,m
                   (,@(make-plan-slice-and-dice (- n m 1) lane1 res weights-xz)
                     (move-to ,(make-c (x pos1) (- res 1) (z pos1))))
                   (,@(make-plan-slice-and-dice m lane2 res weights-xz)
                     (move-to ,(make-c (x pos2) (- res 1) (z pos2)))))))))))


;; Divides the space into roughly equal-weight XZ areas and assigns a bot
;; to each.
(define (strategy-slice-and-dice num-seeds source target)
  (define res (model-res source))
  (define weights-xz
    (for/vector ((i res))
      (for/vector ((k res))
        (for/sum ((j res) #:when (model-voxel-full? target i j k))
                  1))))
  ;; TODO: shrink lane to actual object(s)
  (define lane (make-region (list 1 0 1)
                            (list (- res 2) (- res 1) (- res 2))))
  `(,@(make-plan-slice-and-dice num-seeds lane res weights-xz)
     (move-to (0 0 0))))


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
