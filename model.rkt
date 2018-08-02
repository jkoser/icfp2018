#lang typed/racket

(require "coords.rkt")

(provide Res
         model
         load-model
         load-problem-model
         create-model
         copy-model
         model=?
         model-res
         model-voxel-full?
         model-voxel-fill!
         model-voxel-void!
         model-bounding-box)

;; A model is represented as a resolution R and a bit vector, indexed as
;; X * R * R + Y * R + Z, where set bits represent filled voxels.  Bits are
;; packed into a byte vector, with 8 bits per byte.

(define-type Res Index)
(define-struct model ([res : Res] [bits : Bytes]))


(: bit-count (-> Byte Integer))
(define (bit-count b)
  (let* ((b2 (+ (bitwise-and b #x55)
                (bitwise-and (arithmetic-shift b -1) #x55)))
         (b4 (+ (bitwise-and b2 #x33)
                (bitwise-and (arithmetic-shift b2 -2) #x33)))
         (b8 (+ (bitwise-and b4 #x0f)
                (bitwise-and (arithmetic-shift b4 -4) #x0f))))
    b8))

(: load-model (-> String model))
(define (load-model filename)
  (: pct-filled (-> Res Bytes Real))
  (define (pct-filled res bits)
    (* (/ (for/sum : Integer ((b : Byte bits))
              (bit-count b))
          (* res res res))
       100.0))
  (call-with-input-file
    filename
    (lambda ([in : Input-Port])
      (let ((res (read-byte in))
            (bits (port->bytes in)))
        (when (eof-object? res)
          (error "empty file"))
        (printf "Loaded ~a: res=~a, ~a% full, ~a bytes.~n"
                filename
                res
                (~a #:max-width 6 (pct-filled res bits))
                (bytes-length bits))
        (make-model res bits)))))

(: load-problem-model (-> Natural model))
(define (load-problem-model n)
  (load-model (format "problemsL/LA~a_tgt.mdl"
                      (~a n #:width 3 #:align 'right #:pad-string "0"))))

(: create-model (-> Res model))
(define (create-model res)
  (make-model res
              (make-bytes (ceiling (/ (* res res res) 8)) 0)))

(: copy-model (-> model model))
(define (copy-model m)
  (make-model (model-res m)
              (bytes-copy (model-bits m))))

(: model=? (-> model model Boolean))
(define (model=? m1 m2)
  (and (= (model-res m1) (model-res m2))
       (equal? (model-bits m1) (model-bits m2))))

(: model-bit-index (-> model Coord (Values Integer Fixnum)))
(define (model-bit-index m p)
  (let ((r : Res (model-res m)))
    (quotient/remainder (+ (* (x p) r r) (* (y p) r) (z p)) 8)))

(: model-voxel-full? (-> model Coord Boolean))
(define (model-voxel-full? m p)
  (let-values (((i j) (model-bit-index m p)))
    (bitwise-bit-set? (bytes-ref (model-bits m) i) j)))

(: model-voxel-fill! (-> model Coord Void))
(define (model-voxel-fill! m p)
  (let-values (((i j) (model-bit-index m p)))
    (bytes-set! (model-bits m)
                i
                (bitwise-ior (bytes-ref (model-bits m) i)
                             (arithmetic-shift 1 j)))))

(: model-voxel-void! (-> model Coord Void))
(define (model-voxel-void! m p)
  (let-values (((i j) (model-bit-index m p)))
    (bytes-set! (model-bits m)
                i
                (bitwise-and (bytes-ref (model-bits m) i)
                             (bitwise-not (arithmetic-shift 1 j))))))

(: model-bounding-box (-> model Region))
(define (model-bounding-box m)
  (define res (model-res m))
  (define-values (xmin xmax ymin ymax zmin zmax)
    (for*/fold ([xmin : Integer res]
                [xmax : Integer 0]
                [ymin : Integer res]
                [ymax : Integer 0]
                [zmin : Integer res]
                [zmax : Integer 0])
      ((i res) (j res) (k res)
               #:when (model-voxel-full? m (c i j k)))
      (values (min xmin i) (max xmax i)
              (min ymin j) (max ymax j)
              (min zmin k) (max zmax k))))
  (region (c xmin ymin zmin)
          (c xmax ymax zmax)))
