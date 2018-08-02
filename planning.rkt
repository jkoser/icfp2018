#lang typed/racket

(require "coords.rkt")
(require "model.rkt")
(require "state.rkt")
(require "trace.rkt")

(provide (all-defined-out))

;; A plan is a high-level description of nanobot actions.  Each bot
;; operates in a "lane", which is a rectangular 3D region of space of
;; which it has exclusive use.  Lane information is used when a plan
;; is compiled down into a command trace.

;; The most basic plan uses a single bot to assemble the target object
;; in its lane (which is the entire space for the initial bot):
#;
((assemble-in-lane 'top)
 (move-to (c 0 0 0)))

;; The assemble-in-lane instruction reserves one plane for movement,
;; and does not fill any voxels in that plane.  (The above plan uses the
;; property that well-formed models have empty side faces, so the bot
;; can freely return to the origin.)

;; Plans use a restricted fission/fusion pattern: a bot that is spawned
;; in a fission must eventually fuse with its parent.  Fission must
;; create the new bot in a location adjacent to the parent, and it
;; implicitly splits the parent's lane so that the parent and
;; child can operate independently until they fuse again.  Fusion does
;; not necessarily happen at the same location(s) as fission, but bots
;; must end up close enough to fuse.  In a space with resolution 20,
;; we might have the plan:
#;
((move-to (c 9 0 0)) ; initial lane is ((0 19) (0 19) (0 19))
 (spawn (c 10 0 0)   ; new lanes are ((0 9) ...) and ((10 19) ...)
        0            ; give the child no seeds
        ((assemble-in-lane 'top)  ; parent
         (move-to (c 9 19 0)))
        ((assemble-in-lane 'top)  ; child
         (move-to (c 10 19 0))))
 (move-to (c 0 0 0)))

;; Note that once the two bots finish their individual tasks, fusion is
;; implicit.


(struct move-to
  ([dest : Coord]))
(define-type FreePlane (Union 'top))
(struct assemble-in-lane
  ([free-plane : FreePlane]))
(struct spawn
  ([child-pos : Coord]
   [num-seeds : Natural]
   [parent-plan : Plan]
   [child-plan : Plan]))
(struct with-lane
  ([lane : Region]
   [body : Plan]))
(define-type PlanCmd
             (Union move-to
                    assemble-in-lane
                    spawn
                    with-lane))
(define-type Plan (Listof PlanCmd))


;; Returns the new lanes as two values, with the first value
;; corresponding to pos1 and the second to pos2.
(: lane-split (-> Region Coord Coord (Values Region Region)))
(define (lane-split lane pos1 pos2)
  (cond ((> (x pos1) (x pos2))
         (values (region-above-x lane (x pos1))
                 (region-below-x lane (x pos2))))
        ((< (x pos1) (x pos2))
         (values (region-below-x lane (x pos1))
                 (region-above-x lane (x pos2))))
        ((> (y pos1) (y pos2))
         (values (region-above-y lane (y pos1))
                 (region-below-y lane (y pos2))))
        ((< (y pos1) (y pos2))
         (values (region-below-y lane (y pos1))
                 (region-above-y lane (y pos2))))
        ((> (z pos1) (z pos2))
         (values (region-above-z lane (z pos1))
                 (region-below-z lane (z pos2))))
        ((< (z pos1) (z pos2))
         (values (region-below-z lane (z pos1))
                 (region-above-z lane (z pos2))))
        (else
          (error "coordinates are the same"))))

;; In building a trace, we must associate each instruction with the ID
;; of the bot that performs it.  This way we can make sure the instructions
;; for each time step are properly sorted in the final trace.  Thus, in an
;; intermediate trace, each element is a sorted assoc-list containing the
;; instructions for each bot during that time step.
#;
(((1 smove (6 0 0)) (2 fill (0 -1 0)))
 ((1 fill (0 -1 0)) (2 lmove (3 0 0) (0 1 0)))
 ...)

(define-type TaggedTrace (Listof (Listof (Pairof Natural Cmd))))

;; Tags the commands in a single-bot trace with ids.
(: tag-trace (-> Trace Natural TaggedTrace))
(define (tag-trace trace id)
  (for/list ((cmd trace))
    (list (cons id cmd))))

(: merge-sorted-lists
   (All (V) (-> (Listof (Pairof Natural V)) (Listof (Pairof Natural V))
                (Listof (Pairof Natural V)))))
(define (merge-sorted-lists s1 s2)
  (match* (s1 s2)
    (('() _) s2)
    ((_ '()) s1)
    (((list-rest (and c1 (cons k1 _)) cs1)
      (list-rest (and c2 (cons k2 _)) cs2))
     (if (< k1 k2)
        (cons c1 (merge-sorted-lists cs1 s2))
        (cons c2 (merge-sorted-lists s1 cs2))))))

;; Combines two tagged traces, adding (wait) instructions as necessary
;; if the traces are of unequal length.
(: interleave-traces (-> TaggedTrace bot TaggedTrace bot TaggedTrace))
(define (interleave-traces trace1 bot1 trace2 bot2)
  (let* ((n1 (length trace1))
         (n2 (length trace2))
         (padded1
           (if (>= n1 n2)
             trace1
             (append trace1
                     (make-list (- n2 n1)
                                (list (cons (bot-bid bot1) (wait)))))))
         (padded2
           (if (<= n1 n2)
             trace2
             (append trace2
                     (make-list (- n1 n2)
                                (list (cons (bot-bid bot2) (wait))))))))
    (map (inst merge-sorted-lists Cmd) padded1 padded2)))

;; Removes bot ids and flattens the trace across time steps.
(: strip-trace (-> TaggedTrace Trace))
(define (strip-trace trace)
  (apply append
         (map (lambda ([cmds : (Listof (Pairof Natural Cmd))])
                (map (inst cdr Natural Cmd) cmds))
              trace)))


;; Produces a list of commands to move from pos to dest, which must be
;; in the given lane.  The current implementation assumes a clear path
;; along the Z, then X, then Y dimensions.
(: compile-move (-> Coord Coord Region (Listof Cmd) Trace))
(define (compile-move pos dest lane acc)
  (if (equal? pos dest)
    (reverse acc)
    (let* ((diff (c- dest pos))
           (step (cond ((> (dz diff) 0)
                        (d 0 0 (min (dz diff) LLD-MAX)))
                       ((< (dz diff) 0)
                        (d 0 0 (max (dz diff) (- LLD-MAX))))
                       ((> (dx diff) 0)
                        (d (min (dx diff) LLD-MAX) 0 0))
                       ((< (dx diff) 0)
                        (d (max (dx diff) (- LLD-MAX)) 0 0))
                       ((> (dy diff) 0)
                        (d 0 (min (dy diff) LLD-MAX) 0))
                       ((< (dy diff) 0)
                        (d 0 (max (dy diff) (- LLD-MAX)) 0))
                       (else
                         (error "never reached")))))
      (compile-move (c+ pos step)
                    dest
                    lane
                    (cons (smove step) acc)))))


;; Produces a list of commands the bot can follow to assemble the part
;; of the target model within the given lane.  As a side effect, updates
;; the bot's position.
(: compile-assemble-in-lane (-> bot Region FreePlane model Trace))
(define (compile-assemble-in-lane bot lane free-plane target-model)
  (when (not (equal? free-plane 'top))
    (error "assemble-in-lane only implemented for top" free-plane))
  ;; We treat the assembly area as a set of 3x3 towers, with each tower
  ;; constructed in layers.  At each stop, the bot can fill the ring
  ;; around it in the same layer, as well as the center of the layer below.
  (define offsets (list (d 1 0 0) (d 1 0 1) (d 0 0 1) (d -1 0 1)
                        (d -1 0 0) (d -1 0 -1) (d 0 0 -1) (d 1 0 -1)
                        (d 0 -1 0)))
  (define x-stops : (Listof Integer)
    (append (range (+ (xmin lane) 1) (+ (xmax lane) 1) 3)
            (if (= (remainder (- (xmax lane) (xmin lane)) 3) 0)
              (list (xmax lane))
              '())))
  (define z-stops : (Listof Integer)
    (append (range (+ (zmin lane) 1) (+ (zmax lane) 1) 3)
            (if (= (remainder (- (zmax lane) (zmin lane)) 3) 0)
              (list (zmax lane))
              '())))
  (define almost-everything
    (apply
      append
      (for*/list : (Listof Trace) ((k z-stops) (i x-stops))
        (define make-tower : (Listof Trace)
          (for/list ((j (in-range (ymin lane)
                                  (+ (ymax lane) 1))))
            (define stop-pos (c i j k))
            (define to-fill : (Listof CoordDiff)
              (filter (lambda ([nd : CoordDiff])
                        (let ((p (c+ stop-pos nd)))
                          (and (region-includes? lane p)
                               (model-voxel-full? target-model p))))
                      offsets))
            (if (empty? to-fill)
              '()
              (begin0
                (append (compile-move (bot-pos bot) stop-pos
                                      lane '())
                        (map (lambda (nd) (sfill nd)) to-fill))
                (set-bot-pos! bot stop-pos)))))
        ;; Ensure a clear Z-X-Y move to the base of the next tower.
        (let ((pos-x1 (c+ (bot-pos bot) (d 1 0 0)))
              (pos-z1 (c+ (bot-pos bot) (d 0 0 1))))
          (if (or (and (region-includes? lane pos-x1)
                       (model-voxel-full? target-model pos-x1))
                  (and (region-includes? lane pos-z1)
                       (model-voxel-full? target-model pos-z1)))
            (begin0
              (append (apply append make-tower)
                      (list (smove (d 0 1 0))))
              (set-bot-pos! bot (c+ (bot-pos bot) (d 0 1 0))))
            (apply append make-tower))))))
  ;; We need to ensure that the bot will have a clear Z-X-Y move to its
  ;; next waypoint, so we move to the top of the lane.
  (define pos (bot-pos bot))
  (define end-pos (c (x pos) (ymax lane) (z pos)))
  (set-bot-pos! bot end-pos)
  (append almost-everything
          (compile-move pos end-pos lane '())))


(: compile-plan (-> Plan model model Natural Trace))
(define (compile-plan plan source-model target-model num-seeds)

  (: compile-cmds (-> Plan bot Region TaggedTrace))
  (define (compile-cmds cmds bot lane)
    (apply append
           (for/list : (Listof TaggedTrace) ((cmd cmds))
             (compile-cmd cmd bot lane))))

  (: compile-cmd (-> PlanCmd bot Region TaggedTrace))
  (define (compile-cmd cmd bot lane)
    (match cmd
      ((move-to dest)
       (let ((trace (tag-trace (compile-move (bot-pos bot) dest lane '())
                               (bot-bid bot))))
         (set-bot-pos! bot dest)
         trace))
      ((assemble-in-lane free-plane)
       (tag-trace (compile-assemble-in-lane
                    bot lane free-plane target-model)
                  (bot-bid bot)))
      ((spawn pos2 num-seeds p-plan c-plan)
       (let*-values (((pos1) (bot-pos bot))
                     ((lane1 lane2) (lane-split lane pos1 pos2))
                     ((bot2) (bot-fission! bot pos2 num-seeds))
                     ((trace1) (compile-cmds p-plan bot lane1))
                     ((trace2) (compile-cmds c-plan bot2 lane2))
                     ((trace12) (interleave-traces trace1 bot trace2 bot2)))
         (bot-fusion! bot bot2)
         `((,(cons (bot-bid bot)
                   (fission (c- pos2 pos1) num-seeds)))
           ,@trace12
           ,(merge-sorted-lists
              (list (cons (bot-bid bot)
                          (fusionp (c- (bot-pos bot2) (bot-pos bot)))))
              (list (cons (bot-bid bot2)
                          (fusions (c- (bot-pos bot) (bot-pos bot2)))))))))
      ((with-lane lane body)
       (compile-cmds body bot lane))))

  (let* ((res (model-res source-model))
         (bot (create-first-bot num-seeds))
         (lane (region (c 0 0 0)
                       (c (- res 1) (- res 1) (- res 1)))))
    (append (list (flip))
            (strip-trace (compile-cmds plan bot lane))
            (list (flip)
                  (halt)))))
