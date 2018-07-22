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
((assemble-in-lane 'top)
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
        ((assemble-in-lane 'top)  ; parent
         (move-to (9 19 0)))
        ((assemble-in-lane 'top)  ; child
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
    (cond ((> (c-x pos1) (c-x pos2))
           (values (make-lane (make-c (c-x pos1) (c-y minc) (c-z minc))
                              maxc)
                   (make-lane minc
                              (make-c (c-x pos2) (c-y maxc) (c-z maxc)))))
          ((< (c-x pos1) (c-x pos2))
           (values (make-lane minc
                              (make-c (c-x pos1) (c-y maxc) (c-z maxc)))
                   (make-lane (make-c (c-x pos2) (c-y minc) (c-z minc))
                              maxc)))
          ((> (c-y pos1) (c-y pos2))
           (values (make-lane (make-c (c-x minc) (c-y pos1) (c-z minc))
                              maxc)
                   (make-lane minc
                              (make-c (c-x maxc) (c-y pos2) (c-z maxc)))))
          ((< (c-y pos1) (c-y pos2))
           (values (make-lane minc
                              (make-c (c-x maxc) (c-y pos1) (c-z maxc)))
                   (make-lane (make-c (c-x minc) (c-y pos2) (c-z minc))
                              maxc)))
          ((> (c-z pos1) (c-z pos2))
           (values (make-lane (make-c (c-x minc) (c-y minc) (c-z pos1))
                              maxc)
                   (make-lane minc
                              (make-c (c-x maxc) (c-y maxc) (c-z pos2)))))
          ((< (c-z pos1) (c-z pos2))
           (values (make-lane minc
                              (make-c (c-x maxc) (c-y maxc) (c-z pos1)))
                   (make-lane (make-c (c-x minc) (c-y minc) (c-z pos2))
                              maxc))))))

(define (compile-plan plan source-model target-model)
  'TODO)
