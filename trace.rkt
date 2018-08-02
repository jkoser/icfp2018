#lang typed/racket

(require "coords.rkt")

(provide (all-defined-out))

;; A trace is represented as a list of commands, where each command
;; is a struct identified by the opcode.  For example,
;;   (list (smove (d 0 0 7))
;;         (sfill (d 1 0 0))
;;         (smove (d 0 0 -7))
;;         (halt))

(struct halt ())
(struct wait ())
(struct flip ())
(struct smove ([lld : CoordDiff]))
(struct lmove ([sld1 : CoordDiff] [sld2 : CoordDiff]))
(struct fusionp ([nd : CoordDiff]))
(struct fusions ([nd : CoordDiff]))
(struct fission ([nd : CoordDiff] [m : Natural]))
(struct sfill ([nd : CoordDiff]))  ; renamed to mirror svoid
(struct svoid ([nd : CoordDiff]))  ; renamed to avoid conflict with void fn
(struct gfill ([nd : CoordDiff] [fd : CoordDiff]))
(struct gvoid ([nd : CoordDiff] [fd : CoordDiff]))

(define-type Cmd (Union halt wait flip smove lmove fusionp fusions fission
                        sfill svoid gfill gvoid))
(define-type Trace (Listof Cmd))

(: save-trace! (-> String Trace Void))
(define (save-trace! filename trace)
  (call-with-output-file
    filename
    #:exists 'truncate
    (lambda ([out : Output-Port])
      (for ((c trace))
        (write-bytes (command-encoding c) out)))))

;; Returns two values, encoding the axis and magnitude
(: sld-encoding (-> CoordDiff (Values Byte Byte)))
(define (sld-encoding q)
  (cond ((not (= (dx q) 0))
         (values #b01
                 (cast (+ (dx q) 5) Byte)))
        ((not (= (dy q) 0))
         (values #b10
                 (cast (+ (dy q) 5) Byte)))
        ((not (= (dz q) 0))
         (values #b11
                 (cast (+ (dz q) 5) Byte)))
        (else
          (error "sld cannot be all zeros"))))

;; Returns two values, encoding the axis and magnitude
(: lld-encoding (-> CoordDiff (Values Byte Byte)))
(define (lld-encoding q)
  (cond ((not (= (dx q) 0))
         (values #b01
                 (cast (+ (dx q) 15) Byte)))
        ((not (= (dy q) 0))
         (values #b10
                 (cast (+ (dy q) 15) Byte)))
        ((not (= (dz q) 0))
         (values #b11
                 (cast (+ (dz q) 15) Byte)))
        (else
          (error "lld cannot be all zeros"))))

(: nd-encoding (-> CoordDiff Byte))
(define (nd-encoding q)
  (cast (+ (* (+ (dx q) 1) 9)
           (* (+ (dy q) 1) 3)
           (+ (dz q) 1))
        Byte))

;; Returns three values, one for each dimension
(: fd-encoding (-> CoordDiff (Values Byte Byte Byte)))
(define (fd-encoding q)
  (values (cast (+ (dx q) 30) Byte)
          (cast (+ (dy q) 30) Byte)
          (cast (+ (dz q) 30) Byte)))

(define (<< n k)
  (arithmetic-shift n k))

;; Returns a byte vector, whose length depends on the command
(: command-encoding (-> Cmd Bytes))
(define (command-encoding c)
  (match c
    ((halt)
     (bytes #b11111111))
    ((wait)
     (bytes #b11111110))
    ((flip)
     (bytes #b11111101))
    ((smove lld)
     (let-values (((llda lldi) (lld-encoding lld)))
       (bytes (+ (<< llda 4) #b0100)
              lldi)))
    ((lmove sld1 sld2)
     (let-values (((sld1a sld1i) (sld-encoding sld1))
                  ((sld2a sld2i) (sld-encoding sld2)))
       (bytes (+ (<< sld2a 6) (<< sld1a 4) #b1100)
              (+ (<< sld2i 4) sld1i))))
    ((fusionp nd)
     (let ((nde (nd-encoding nd)))
       (bytes (+ (<< nde 3) #b111))))
    ((fusions nd)
     (let ((nde (nd-encoding nd)))
       (bytes (+ (<< nde 3) #b110))))
    ((fission nd m)
     (let ((nde (nd-encoding nd)))
       (bytes (+ (<< nde 3) #b101)
              m)))
    ((sfill nd)
     (let ((nde (nd-encoding nd)))
       (bytes (+ (<< nde 3) #b011))))
    ((svoid nd)
     (let ((nde (nd-encoding nd)))
       (bytes (+ (<< nde 3) #b010))))
    ((gfill nd fd)
     (let-values (((nde) (nd-encoding nd))
                  ((fddx fddy fddz) (fd-encoding fd)))
       (bytes (+ (<< nde 3) #b001)
              fddx fddy fddz)))
    ((gvoid nd fd)
     (let-values (((nde) (nd-encoding nd))
                  ((fddx fddy fddz) (fd-encoding fd)))
       (bytes (+ (<< nde 3) #b000)
              fddx fddy fddz)))))
