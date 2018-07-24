#lang racket

(require "coords.rkt")
(require "model.rkt")
(require "state.rkt")

(provide compile-plan)

;; A plan is a high-level description of nanobot actions.  Each bot
;; operates in a "lane", which is a rectangular 3D region of space of
;; which it has exclusive use.  Lane information is used when a plan
;; is compiled down into a command trace.

;; The most basic plan uses a single bot to assemble the target object
;; in its lane (which is the entire space for the initial bot):
#;
((assemble-in-lane top)
 (move-to (0 0 0)))

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
((move-to (9 0 0))   ; initial lane is ((0 19) (0 19) (0 19))
 (spawn (10 0 0)     ; new lanes are ((0 9) ...) and ((10 19) ...)
        0            ; give the child no seeds
        ((assemble-in-lane top)  ; parent
         (move-to (9 19 0)))
        ((assemble-in-lane top)  ; child
         (move-to (10 19 0))))
 (move-to (0 0 0)))

;; Note that once the two bots finish their individual tasks, fusion is
;; implicit.

(define-struct lane (min-corner max-corner) #:transparent)

;; Returns the new lanes as two values, with the first value
;; corresponding to pos1 and the second to pos2.
(define (lane-split lane pos1 pos2)
  (let ((minc (lane-min-corner lane))
        (maxc (lane-max-corner lane)))
    (cond ((> (x pos1) (x pos2))
           (values (make-lane (make-c (x pos1) (y minc) (z minc))
                              maxc)
                   (make-lane minc
                              (make-c (x pos2) (y maxc) (z maxc)))))
          ((< (x pos1) (x pos2))
           (values (make-lane minc
                              (make-c (x pos1) (y maxc) (z maxc)))
                   (make-lane (make-c (x pos2) (y minc) (z minc))
                              maxc)))
          ((> (y pos1) (y pos2))
           (values (make-lane (make-c (x minc) (y pos1) (z minc))
                              maxc)
                   (make-lane minc
                              (make-c (x maxc) (y pos2) (z maxc)))))
          ((< (y pos1) (y pos2))
           (values (make-lane minc
                              (make-c (x maxc) (y pos1) (z maxc)))
                   (make-lane (make-c (x minc) (y pos2) (z minc))
                              maxc)))
          ((> (z pos1) (z pos2))
           (values (make-lane (make-c (x minc) (y minc) (z pos1))
                              maxc)
                   (make-lane minc
                              (make-c (x maxc) (y maxc) (z pos2)))))
          ((< (z pos1) (z pos2))
           (values (make-lane minc
                              (make-c (x maxc) (y maxc) (z pos1)))
                   (make-lane (make-c (x minc) (y minc) (z pos2))
                              maxc))))))

;; In building a trace, we must associate each instruction with the ID
;; of the bot that performs it.  This way we can make sure the instructions
;; for each time step are properly sorted in the final trace.  Thus, in an
;; intermediate trace, each element is an assoc-list containing the
;; instructions for each bot during that time step.
#;
(((1 smove (6 0 0)) (2 fill (0 -1 0)))
 ((1 fill (0 -1 0)) (2 lmove (3 0 0) (0 1 0)))
 ...)

;; Tags the commands in a single-bot trace with ids.
(define (tag-trace trace id)
  (for/list ((cmd trace))
    (list (cons id cmd))))

;; Removes bot ids and flattens the trace across time steps.
(define (strip-trace trace)
  (apply append
         (map (lambda (cmds) (map rest
                                  (sort cmds < #:key first)))
              trace)))


;; Produces a list of commands to move from pos to dest, which must be
;; in the given lane.  The current implementation assumes a clear path
;; along the Z, then X, then Y dimensions.
(define (compile-move pos dest lane acc)
  (if (equal? pos dest)
    (reverse acc)
    (let* ((diff (c- dest pos))
           (step (cond ((> (dz diff) 0)
                        (make-d 0 0 (min (dz diff) LLD-MAX)))
                       ((< (dz diff) 0)
                        (make-d 0 0 (max (dz diff) (- LLD-MAX))))
                       ((> (dx diff) 0)
                        (make-d (min (dx diff) LLD-MAX) 0 0))
                       ((< (dx diff) 0)
                        (make-d (max (dx diff) (- LLD-MAX)) 0 0))
                       ((> (dy diff) 0)
                        (make-d 0 (min (dy diff) LLD-MAX) 0))
                       ((< (dy diff) 0)
                        (make-d 0 (max (dy diff) (- LLD-MAX)) 0)))))
      (compile-move (c+ pos step)
                    dest
                    lane
                    (cons (list 'smove step) acc)))))


;; Produces a list of commands the bot can follow to assemble the part
;; of the target model within the given lane.  As a side effect, updates
;; the bot's position.
(define (compile-assemble-in-lane bot lane free-plane target-model)
  (when (not (equal? free-plane 'top))
    (error "assemble-in-lane only implemented for top" free-plane))
  (let ((corner0 (lane-min-corner lane))
        (corner1 (lane-max-corner lane)))
    (apply
      append
      (for*/list ((j (in-range (y corner0) (y corner1)))
                  (i (in-range (x corner0) (+ (x corner1) 1)))
                  (k (in-range (z corner0) (+ (z corner1) 1)))
                  #:when (model-voxel-full? target-model i j k))
        (begin0
          (append (compile-move (bot-pos bot) (make-c i (+ j 1) k)
                                lane '())
                  (list '(fill (0 -1 0))))
          (set-bot-pos! bot (make-c i (+ j 1) k)))))))


(define (compile-plan plan source-model target-model num-seeds)
  (define (compile-cmd cmd bot lane)
    (match cmd
      ((list 'move-to dest)
       (let ((trace (tag-trace (compile-move (bot-pos bot) dest lane '())
                               (bot-bid bot))))
         (set-bot-pos! bot dest)
         trace))
      ((list 'assemble-in-lane free-plane)
       (tag-trace (compile-assemble-in-lane
                    bot lane free-plane target-model)
                  (bot-bid bot)))
      ((list 'spawn pos2 num-seeds)
       'TODO)))
  (let* ((res (model-res source-model))
         (bot (create-first-bot num-seeds))
         (lane (make-lane (make-c 0 0 0)
                          (make-c (- res 1) (- res 1) (- res 1)))))
    `((flip)
      ,@(strip-trace
          (apply append
                 (for/list ((cmd plan))
                   (compile-cmd cmd bot lane))))
      (flip)
      (halt))))
