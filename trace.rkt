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


(define bit-field bitwise-bit-field)


(: load-trace (-> String Trace))
(define (load-trace filename)
  (call-with-input-file
    filename
    load-trace-from-port))


(: load-trace-from-port (-> Input-Port Trace))
(define (load-trace-from-port in)

  (: next-cmd (-> (Option Cmd)))
  (define (next-cmd)
    (define b1 (read-byte in))
    (cond ((eof-object? b1) #f)
          ((= b1 #b11111111) (halt))
          ((= b1 #b11111110) (wait))
          ((= b1 #b11111101) (flip))
          ((= (bit-field b1 0 4) #b0100)
           (let ((b2 (read-byte in)))
             (if (eof-object? b2)
               (error "unexpected eof")
               (smove (lld-from-encoding (bit-field b1 4 6)
                                         (bit-field b2 0 5))))))
          ((= (bit-field b1 0 4) #b1100)
           (let ((b2 (read-byte in)))
             (if (eof-object? b2)
               (error "unexpected eof")
               (lmove (sld-from-encoding (bit-field b1 4 6)
                                         (bit-field b2 0 4))
                      (sld-from-encoding (bit-field b1 6 8)
                                         (bit-field b2 4 8))))))
          ((= (bit-field b1 0 3) #b111)
           (fusionp (nd-from-encoding (bit-field b1 3 8))))
          ((= (bit-field b1 0 3) #b110)
           (fusions (nd-from-encoding (bit-field b1 3 8))))
          ((= (bit-field b1 0 3) #b101)
           (let ((b2 (read-byte in)))
             (if (eof-object? b2)
               (error "unexpected eof")
               (fission (nd-from-encoding (bit-field b1 3 8))
                        b2))))
          ((= (bit-field b1 0 3) #b011)
           (sfill (nd-from-encoding (bit-field b1 3 8))))
          ((= (bit-field b1 0 3) #b010)
           (svoid (nd-from-encoding (bit-field b1 3 8))))
          ((= (bit-field b1 0 3) #b001)
           (let* ((b2 (read-byte in))
                  (b3 (read-byte in))
                  (b4 (read-byte in)))
             (if (or (eof-object? b2) (eof-object? b3) (eof-object? b4))
               (error "unexpected eof")
               (gfill (nd-from-encoding (bit-field b1 3 8))
                      (fd-from-encoding b2 b3 b4)))))
          ((= (bit-field b1 0 3) #b000)
           (let* ((b2 (read-byte in))
                  (b3 (read-byte in))
                  (b4 (read-byte in)))
             (if (or (eof-object? b2) (eof-object? b3) (eof-object? b4))
               (error "unexpected eof")
               (gvoid (nd-from-encoding (bit-field b1 3 8))
                      (fd-from-encoding b2 b3 b4)))))
          (else (error "never reached"))))

  (: loop (-> (Listof Cmd) Trace))
  (define (loop rtrace)
    (let ((cmd (next-cmd)))
      (if cmd
        (loop (cons cmd rtrace))
        (reverse rtrace))))
  (loop '()))


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

(: sld-from-encoding (-> Byte Byte CoordDiff))
(define (sld-from-encoding a i)
  (cond ((= a #b01) (d (- i 5) 0 0))
        ((= a #b10) (d 0 (- i 5) 0))
        ((= a #b11) (d 0 0 (- i 5)))
        (else (error "bad sld encoding" (cons a i)))))

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

(: lld-from-encoding (-> Byte Byte CoordDiff))
(define (lld-from-encoding a i)
  (cond ((= a #b01) (d (- i 15) 0 0))
        ((= a #b10) (d 0 (- i 15) 0))
        ((= a #b11) (d 0 0 (- i 15)))
        (else (error "bad lld encoding" (cons a i)))))

(: nd-encoding (-> CoordDiff Byte))
(define (nd-encoding q)
  (cast (+ (* (+ (dx q) 1) 9)
           (* (+ (dy q) 1) 3)
           (+ (dz q) 1))
        Byte))

(: nd-from-encoding (-> Byte CoordDiff))
(define (nd-from-encoding n)
  (let*-values (((q3 r3) (quotient/remainder n 3))
                ((q9 r9) (quotient/remainder q3 3)))
    (d (- q9 1) (- r9 1) (- r3 1))))

;; Returns three values, one for each dimension
(: fd-encoding (-> CoordDiff (Values Byte Byte Byte)))
(define (fd-encoding q)
  (values (cast (+ (dx q) 30) Byte)
          (cast (+ (dy q) 30) Byte)
          (cast (+ (dz q) 30) Byte)))

(: fd-from-encoding (-> Byte Byte Byte CoordDiff))
(define (fd-from-encoding i j k)
  (d (- i 30) (- j 30) (- k 30)))

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
