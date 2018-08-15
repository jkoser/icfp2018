#lang typed/racket

(require "coords.rkt")
(require "model.rkt")
(require "planning.rkt")

(provide Strategy
         strategies-by-name
         strategy-assemble-in-slices
         strategy-balanced-slices
         strategy-slice-and-dice)

;; A strategy is a function that accepts three arguments:
;;   1) the number of seeds available to the first bot
;;   2) a source model, or #f for an assembly problem
;;   3) a target model, or #f for a disassembly problem
;; and returns a plan for transforming the source into the target.
(define-type Strategy
             (-> Natural
                 (Option model)
                 (Option model)
                 Plan))


;; Helper for strategy-slice-and-dice
(: make-plan-slice-and-dice
   (-> Natural Region Res (Vectorof (Vectorof Integer)) Boolean Boolean
       Plan))
(define (make-plan-slice-and-dice n lane res weights-xz assemble? disassemble?)
  (if (or (= n 0) (< (dim lane) 2))
    ;; Base case:
    (append
     (if disassemble?
       (list (disassemble-in-lane 'top))
       '())
     (if assemble?
       (list (assemble-in-lane 'top))
       '()))
    ;; Recursive case:
    (let*-values
      (((y0) (if disassemble? (ymax lane) (ymin lane)))
       ((y1) (if assemble? (ymax lane) (ymin lane)))
       ;; To avoid duplicating code for splits in the X and Z dimensions,
       ;; we establish a view, naming the appropriate dimensions S and T.
       ;; The "split" function produces spawn locations and child lanes.
       (([smin : Integer]
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
                    (values (c smid y0 (zmin lane))
                            (c (+ smid 1) y0 (zmin lane))
                            (region-below-x lane smid)
                            (region-above-x lane (+ smid 1)))))
          ;; split Z dimension
          (values (zmin lane) (zmax lane) (xmin lane) (xmax lane)
                  (lambda (s t) (vector-ref (vector-ref weights-xz t) s))
                  (lambda (smid)
                    (values (c (xmin lane) y0 smid)
                            (c (xmin lane) y0 (+ smid 1))
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
                                                lane1 res weights-xz
                                                assemble? disassemble?)
                      (list (move-to (c (x pos1) y1 (z pos1)))))
              (append (make-plan-slice-and-dice m lane2 res weights-xz
                                                assemble? disassemble?)
                      (list (move-to (c (x pos2) y1 (z pos2))))))))))))


;; Divides the space into roughly equal-weight XZ areas and assigns a bot
;; to each.
(: strategy-slice-and-dice (-> Natural (Option model) (Option model) Plan))
(define (strategy-slice-and-dice num-seeds source target)
  (define nonempty-model : model
    (or source target (error "need at least one model")))
  (define res (model-res nonempty-model))
  (define weights-xz
    (if (and source target)
      (for/vector : (Vectorof (Vectorof Integer)) ((i res))
        (for/vector : (Vectorof Integer) ((k res))
          (for/sum : Integer ((j res))
            (+ (if (model-voxel-full? source (c i j k)) 1 0)
               (if (model-voxel-full? target (c i j k)) 1 0)))))
      (for/vector : (Vectorof (Vectorof Integer)) ((i res))
        (for/vector : (Vectorof Integer) ((k res))
          (for/sum : Integer
            ((j res) #:when (model-voxel-full? nonempty-model (c i j k)))
            1)))))
  (define bb
    (if (and source target)
      (region-union-bounding-box (model-bounding-box source)
                                 (model-bounding-box target))
      (model-bounding-box nonempty-model)))
  ;; Leave a free plane on top for movement, and maybe a couple on the sides.
  (define lane (region (if source
                         (c (- (xmin bb) 1) (ymin bb) (- (zmin bb) 1))
                         (c (xmin bb) (ymin bb) (zmin bb)))
                       (c (xmax bb) (+ (ymax bb) 1) (zmax bb))))
  (list (with-lane
          lane
          (make-plan-slice-and-dice num-seeds lane res weights-xz
                                    (and target #t)
                                    (and source #t)))
        (move-to (c 0 0 0))))


;; Helper for strategy-balanced-slices
(: make-plan-balanced
   (-> Natural Integer Integer Res (Vectorof Integer) Boolean Boolean
       Plan))
(define (make-plan-balanced n xmin xmax res weights assemble? disassemble?)
  (if (or (= n 0) (= xmin xmax))
    ;; Base case:
    (append
     (if disassemble?
       (list (disassemble-in-lane 'top))
       '())
     (if assemble?
       (list (assemble-in-lane 'top))
       '()))
    ;; Recursive case:
    (let* ((y0 (if disassemble? (- res 1) 0))
           (y1 (if assemble? (- res 1) 0))
           (m (quotient (- n 1) 2))
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
        (move-to (c xmid y0 0))
        (spawn (c (+ xmid 1) y0 0)
               m
               (append
                (make-plan-balanced (cast (- n m 1) Natural)
                                    xmin xmid res weights
                                    assemble? disassemble?)
                (list (move-to (c xmid y1 0))))
               (append
                (make-plan-balanced m (+ xmid 1) xmax res weights
                                    assemble? disassemble?)
                (list (move-to (c (+ xmid 1) y1 0)))))))))

;; Divides the space into roughly equal-weight slices and assigns a bot to each.
(: strategy-balanced-slices (-> Natural (Option model) (Option model) Plan))
(define (strategy-balanced-slices num-seeds source target)
  (define nonempty-model : model
    (or source target (error "need at least one model")))
  (define res (model-res nonempty-model))
  (define weights
    (if (and source target)
      (for/vector : (Vectorof Integer) ((i res))
        (for*/sum : Integer ((j res) (k res))
                  (+ (if (model-voxel-full? source (c i j k)) 1 0)
                     (if (model-voxel-full? target (c i j k)) 1 0))))
      (for/vector : (Vectorof Integer) ((i res))
        (for*/sum : Integer
                  ((j res) (k res)
                           #:when (model-voxel-full? nonempty-model (c i j k)))
                  1))))
  (append (make-plan-balanced num-seeds 1 (- res 2) res weights
                              (and target #t)
                              (and source #t))
          (list (move-to (c 0 0 0)))))


;; Helper for strategy-assemble-in-slices
(: make-plan-slices (-> Natural Integer Integer Res Boolean Boolean Plan))
(define (make-plan-slices n xmin xmax res assemble? disassemble?)
  (if (or (= n 0) (= xmin xmax))
    ;; Base case:
    (append
     (if disassemble?
       (list (disassemble-in-lane 'top))
       '())
     (if assemble?
       (list (assemble-in-lane 'top))
       '()))
    ;; Recursive case:
    (let* ((y0 (if disassemble? (- res 1) 0))
           (y1 (if assemble? (- res 1) 0))
           (m (quotient (- n 1) 2))
           (width (+ (- xmax xmin) 1))
           (left (floor (* width (/ (- n m) (+ n 1)))))
           (xmid (max xmin (min (+ xmin left -1) (- xmax 1)))))
      (list
        (move-to (c xmid y0 0))
        (spawn (c (+ xmid 1) y0 0)
               m
               (append (make-plan-slices (cast (- n m 1) Natural)
                                         xmin xmid res
                                         assemble? disassemble?)
                       (list (move-to (c xmid y1 0))))
               (append (make-plan-slices m (+ xmid 1) xmax res
                                         assemble? disassemble?)
                       (list (move-to (c (+ xmid 1) y1 0)))))))))

;; Divides the space into roughly equal-width slices and assigns a bot to each.
(: strategy-assemble-in-slices
   (-> Natural (Option model) (Option model) Plan))
(define (strategy-assemble-in-slices num-seeds source target)
  (define nonempty-model : model
    (or source target (error "need at least one model")))
  (define res (model-res nonempty-model))
  (append (make-plan-slices num-seeds 1 (- res 2) res
                            (and target #t)
                            (and source #t))
          (list (move-to (c 0 0 0)))))


(: strategies-by-name (Listof (Pairof String Strategy)))
(define strategies-by-name
  `(("assemble-in-slices" . ,strategy-assemble-in-slices)
    ("balanced-slices" . ,strategy-balanced-slices)
    ("slice-and-dice" . ,strategy-slice-and-dice)))
