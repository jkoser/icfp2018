#lang racket

(require "coords.rkt")
(require "state.rkt")
(require "trace.rkt")

(provide run-system!
         step-system!)


;; Helper for bot-cmd-groups
(define (fusions-finder b1 nd1)
  (match-lambda
    ((list b2 'fusions nd2)
     (and (equal? (bot-pos b1) (c+ (bot-pos b2) nd2))
          (equal? (bot-pos b2) (c+ (bot-pos b1) nd1))))
    (else #f)))

;; Helper for bot-cmd-groups
(define (gfill-gvoid-finder op region)
  (match-lambda
    ((list b2 op2 nd2 fd2)
     #:when (equal? op op2)
     (let* ((corner (c+ (bot-pos b2) nd2))
            (region2 (make-region corner (c+ corner fd2))))
       (equal? region region2)))
    (else #f)))

;; (Bot Command Args ...)* -> ((Bot ...) Command Args ...)*
(define (bot-cmd-groups bcs)
  ;; Here groups is the result accumulator, pending is the unprocessed input,
  ;; and secondary holds FusionS commands we encounter before FusionP.
  (define (loop groups pending secondary)
    (if (empty? pending)
      (if (empty? secondary)
        groups
        (error "unmatched" secondary))
      (match (first pending)
        ((list b1 'fusionp nd1)
         (let-values (((matches others)
                       (partition (fusions-finder b1 nd1)
                                  (append (rest pending) secondary))))
           (if (= (length matches) 1)
             (loop (cons (list (list b1 (first (first matches)))
                               'fusionp nd1)
                         groups)
                   others
                   '())
             (error "unmatched" (first pending)))))
        ((list b1 'fusions nd1)
         (loop groups
               (rest pending)
               (cons (first pending) secondary)))
        ((list b1 op nd1 fd1)
         #:when (member op '(gfill gvoid))
         (let*-values (((corner) (c+ (bot-pos b1) nd1))
                       ((region) (make-region corner (c+ corner fd1)))
                       ((matches others)
                        (partition (gfill-gvoid-finder op region) pending)))
           (if (= (length matches) (expt 2 (dim region)))
             (loop (cons (list (map first matches)
                               op nd1 fd1)
                         groups)
                   others
                   secondary)
             (error "wrong number of bots" op))))
        ((list b cmd ...)
         (loop (cons (cons (list b) cmd)
                     groups)
               (rest pending)
               secondary)))))
  (loop '() bcs '()))


(define (step-system! s)
  (define bots (sort (system-bots s)
                     <
                     #:key bot-bid))
  (define n (length bots))
  (when (< (length (system-trace s)) n)
      (error "not enough commands"))
  (define bot-cmds (map cons bots (take (system-trace s) n)))
  (define groups (bot-cmd-groups bot-cmds))
  ;; TODO: execute
  (set-system-trace! s (drop (system-trace s) n)))


(define (run-system! s)
  (do ()
    ((empty? (system-bots s)))
    (step-system! s)))
