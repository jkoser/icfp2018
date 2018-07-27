#lang racket

(require (only-in srfi/60
                  bit-count))

(provide load-model
         load-problem-model
         create-model
         copy-model
         model=?
         model-res
         model-voxel-full?
         model-voxel-fill!
         model-voxel-void!)

;; A model is represented as a bit vector with indexes X * R * R + Y * R + Z,
;; where set bits represent filled voxels.  Bits are packed into a byte vector,
;; with 8 bits per byte.

(define-struct model (res bits))

(define (load-model filename)
  (define (pct-filled res bits)
    (* (/ (for/sum ((b bits))
            (bit-count b))
          (* res res res))
       100.0))
  (call-with-input-file
    filename
    (lambda (in)
      (let ((res (read-byte in))
            (bits (port->bytes in)))
        (printf "Loaded ~a: res=~a, ~a% full, ~a bytes.~n"
                filename
                res
                (~a #:max-width 6 (pct-filled res bits))
                (bytes-length bits))
        (make-model res bits)))))

(define (load-problem-model n)
  (load-model (format "problemsL/LA~a_tgt.mdl"
                      (~a n #:width 3 #:align 'right #:pad-string "0"))))

(define (create-model res)
  (make-model res
              (make-bytes (ceiling (/ (* res res res) 8)) 0)))

(define (copy-model m)
  (make-model (model-res m)
              (bytes-copy (model-bits m))))

(define (model=? m1 m2)
  (and (= (model-res m1) (model-res m2))
       (equal? (model-bits m1) (model-bits m2))))

(define (model-bit-index m x y z)
  (let ((r (model-res m)))
    (quotient/remainder (+ (* x r r) (* y r) z) 8)))

(define (model-voxel-full? m x y z)
  (let-values (((i j) (model-bit-index m x y z)))
    (bitwise-bit-set? (bytes-ref (model-bits m) i) j)))

(define (model-voxel-fill! m x y z)
  (let-values (((i j) (model-bit-index m x y z)))
    (bytes-set! (model-bits m)
                i
                (bitwise-ior (bytes-ref (model-bits m) i)
                             (arithmetic-shift 1 j)))))

(define (model-voxel-void! m x y z)
  (let-values (((i j) (model-bit-index m x y z)))
    (bytes-set! (model-bits m)
                i
                (bitwise-and (bytes-ref (model-bits m) i)
                             (bitwise-not (arithmetic-shift 1 j))))))
