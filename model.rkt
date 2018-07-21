#lang racket

(provide load-model
	 load-program-model
	 model-voxel-full?
	 model-voxel-fill!)

;; A model is represented as a bit vector with indexes X * R * R + Y * R + Z,
;; where set bits represent filled voxels.  Bits are packed into a byte vector,
;; with 8 bits per byte.

(define-struct model (res bits) #:transparent)

(define (load-model filename)
  (call-with-input-file
    filename
    (lambda (in)
      (let ((res (read-byte in))
	    (bits (port->bytes in)))
	(printf "Loaded ~a: res=~a, ~a bytes (immutable? ~a).~n"
		filename
		res
		(bytes-length bits)
		(immutable? bits))
	(make-model res bits)))))

(define (load-problem-model n)
  (load-model (format "problemsL/LA~a_tgt.mdl"
		      (~a n #:width 3 #:align 'right #:pad-string "0"))))

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
