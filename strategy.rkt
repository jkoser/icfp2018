#lang typed/racket

(require "coords.rkt")
(require "model.rkt")
(require "planning.rkt")

(provide Strategy
         strategy-assemble-in-slices
         strategy-balanced-slices
         strategy-slice-and-dice)

;; A strategy is a function that accepts three arguments:
;;   1) the number of seeds available to the first bot
;;   2) a source model
;;   3) a target model
;; and returns a plan for transforming the source into the target.
(define-type Strategy (-> Natural model model Plan))


;; Helper for strategy-slice-and-dice
(: make-plan-slice-and-dice
   (-> Natural Region Res (Vectorof (Vectorof Integer))
       Plan))
(define (make-plan-slice-and-dice n lane res weights-xz)
  (if (or (= n 0) (< (dim lane) 2))
    (list (assemble-in-lane 'top))
    ;; To avoid duplicating code for splits in the X and Z dimensions,
    ;; we establish a view, naming the appropriate dimensions S and T.
    ;; The "split" function produces spawn locations and child lanes.
    (let-values
      ((([smin : Integer]
         [smax : Integer]
         [tmin : Integer]
         [tmax : Integer]
         [weights-view : (-> Integer Integer Integer)]
         [split : (-> Integer (Values Coord Coord Region Region))])
        (if (>= (- (xmax lane) (xmin lane))
                (- (zmax lane) (zmin lane)))
          ;; split X dimension
          (values (xmin lane) (xmax lane) (zmin lane) (zmax lane)
                  (lambda (s t) (vector-ref (vector-ref weights-xz s) t))
                  (lambda (smid)
                    (values (c smid (ymin lane) (zmin lane))
                            (c (+ smid 1) (ymin lane) (zmin lane))
                            (region-below-x lane smid)
                            (region-above-x lane (+ smid 1)))))
          ;; split Z dimension
          (values (zmin lane) (zmax lane) (xmin lane) (xmax lane)
                  (lambda (s t) (vector-ref (vector-ref weights-xz t) s))
                  (lambda (smid)
                    (values (c (xmin lane) (ymin lane) smid)
                            (c (xmin lane) (ymin lane) (+ smid 1))
                            (region-below-z lane smid)
                            (region-above-z lane (+ smid 1))))))))
      (let* ((m (quotient (- n 1) 2))
             (bot-ratio : Real (/ (- n m) (+ n 1)))
             (weights-sum (lambda ([s : Integer]) : Integer
                            (for/sum : Integer ((t (in-range tmin (+ tmax 1))))
                              (weights-view s t))))
             (smid : Integer
                   (do ((i1 : Integer smin)
                        (i2 : Integer smax)
                        (w1 : Integer (weights-sum smin))
                        (w2 : Integer (weights-sum smax)))
                     ((= (+ i1 1) i2)
                      i1)
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
          (list
            (move-to pos1)
            (spawn
              pos2 m
              (append (make-plan-slice-and-dice (cast (- n m 1) Natural)
                                                lane1 res weights-xz)
                      (list (move-to (c (x pos1) (ymax lane) (z pos1)))))
              (append (make-plan-slice-and-dice m lane2 res weights-xz)
                      (list (move-to (c (x pos2) (ymax lane) (z pos2))))))))))))


;; Divides the space into roughly equal-weight XZ areas and assigns a bot
;; to each.
(: strategy-slice-and-dice (-> Natural model model Plan))
(define (strategy-slice-and-dice num-seeds source target)
  (define res (model-res source))
  (define weights-xz
    (for/vector : (Vectorof (Vectorof Integer)) ((i res))
      (for/vector : (Vectorof Integer) ((k res))
        (for/sum : Integer
          ((j res) #:when (model-voxel-full? target (c i j k)))
          1))))
  (define bb (model-bounding-box target))
  ;; Leave a free plane on top for movement.
  (define lane (region (c (xmin bb) (ymin bb) (zmin bb))
                       (c (xmax bb) (+ (ymax bb) 1) (zmax bb))))
  (list (with-lane
          lane
          (make-plan-slice-and-dice num-seeds lane res weights-xz))
        (move-to (c 0 0 0))))


;; Helper for strategy-balanced-slices
(: make-plan-balanced
   (-> Natural Integer Integer Res (Vectorof Integer)
       Plan))
(define (make-plan-balanced n xmin xmax res weights)
  (if (or (= n 0) (= xmin xmax))
    (list (assemble-in-lane 'top))
    (let* ((m (quotient (- n 1) 2))
           (bot-ratio (/ (- n m) (+ n 1)))
           (xmid : Integer
                 (do ((i1 : Integer xmin)
                      (i2 : Integer xmax)
                      (w1 : Integer (vector-ref weights xmin))
                      (w2 : Integer (vector-ref weights xmax)))
                   ((= (+ i1 1) i2)
                    i1)
                   (if (or (= w1 w2 0)
                           (< (/ w1 (+ w1 w2)) bot-ratio))
                     (begin
                       (set! i1 (+ i1 1))
                       (set! w1 (+ w1 (vector-ref weights i1))))
                     (begin
                       (set! i2 (- i2 1))
                       (set! w2 (+ w2 (vector-ref weights i2))))))))
      (list
        (move-to (c xmid 0 0))
        (spawn (c (+ xmid 1) 0 0)
               m
               (append
                (make-plan-balanced (cast (- n m 1) Natural)
                                   xmin xmid res weights)
                (list (move-to (c xmid (- res 1) 0))))
               (append
                (make-plan-balanced m (+ xmid 1) xmax res weights)
                (list (move-to (c (+ xmid 1) (- res 1) 0)))))))))

;; Divides the space into roughly equal-weight slices and assigns a bot to each.
(: strategy-balanced-slices (-> Natural model model Plan))
(define (strategy-balanced-slices num-seeds source target)
  (define res (model-res source))
  (define weights
    (for/vector : (Vectorof Integer) ((i res))
      (for*/sum : Integer
                ((j res) (k res) #:when (model-voxel-full? target (c i j k)))
                1)))
  (append (make-plan-balanced num-seeds 1 (- res 2) res weights)
          (list (move-to (c 0 0 0)))))


;; Helper for strategy-assemble-in-slices
(: make-plan-slices (-> Natural Integer Integer Res Plan))
(define (make-plan-slices n xmin xmax res)
  (if (or (= n 0) (= xmin xmax))
    (list (assemble-in-lane 'top))
    (let* ((m (quotient (- n 1) 2))
           (width (+ (- xmax xmin) 1))
           (left (floor (* width (/ (- n m) (+ n 1)))))
           (xmid (max xmin (min (+ xmin left -1) (- xmax 1)))))
      (list
        (move-to (c xmid 0 0))
        (spawn (c (+ xmid 1) 0 0)
               m
               (append (make-plan-slices (cast (- n m 1) Natural)
                                         xmin xmid res)
                       (list (move-to (c xmid (- res 1) 0))))
               (append (make-plan-slices m (+ xmid 1) xmax res)
                       (list (move-to (c (+ xmid 1) (- res 1) 0)))))))))

;; Divides the space into roughly equal-width slices and assigns a bot to each.
(: strategy-assemble-in-slices (-> Natural model model Plan))
(define (strategy-assemble-in-slices num-seeds source target)
  (append (make-plan-slices num-seeds
                            1
                            (- (model-res source) 2)
                            (model-res source))
          (list (move-to (c 0 0 0)))))
