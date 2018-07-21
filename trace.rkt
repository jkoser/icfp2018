#lang racket

(provide save-trace!
	 command-encoding)

;; A trace is represented as a sequence of commands, where each command
;; is a list containing the opcode and arguments.  For example,
;;   ((smove (0 0 7))
;;    (fill (1 0 0))
;;    (smove (0 0 -7))
;;    (halt))

(define (save-trace! filename trace)
  (call-with-output-file
    filename
    #:exists 'truncate
    (lambda (out)
      (for ((c trace))
	(write-bytes (command-encoding c) out)))))

;; Returns two values, encoding the axis and magnitude
(define (sld-encoding dx dy dz)
  (cond ((not (= dx 0))
	 (values #b01 (+ dx 5)))
	((not (= dy 0))
	 (values #b10 (+ dy 5)))
	((not (= dz 0))
	 (values #b11 (+ dz 5)))))

;; Returns two values, encoding the axis and magnitude
(define (lld-encoding dx dy dz)
  (cond ((not (= dx 0))
	 (values #b01 (+ dx 15)))
	((not (= dy 0))
	 (values #b10 (+ dy 15)))
	((not (= dz 0))
	 (values #b11 (+ dz 15)))))

(define (nd-encoding dx dy dz)
  (+ (* (+ dx 1) 9)
     (* (+ dy 1) 3)
     (+ dz 1)))

(define (<< n k)
  (arithmetic-shift n k))
(define (>> n k)
  (arithmetic-shift n (- k)))

;; Returns a byte vector, whose length depends on the command
(define (command-encoding c)
  (case (first c)
    ((halt)
     (bytes #b11111111))
    ((wait)
     (bytes #b11111110))
    ((flip)
     (bytes #b11111101))
    ((smove)
     (let-values (((llda lldi) (apply lld-encoding (second c))))
       (bytes (+ (<< llda 4) #b0100)
	      lldi)))
    ((lmove)
     (let-values (((sld1a sld1i) (apply sld-encoding (second c)))
		  ((sld2a sld2i) (apply sld-encoding (third c))))
       (bytes (+ (<< sld2a 6) (<< sld1a 4) #b1100)
	      (+ (<< sld2i 4) sld1i))))
    ((fusionp)
     (let ((nd (apply nd-encoding (second c))))
       (bytes (+ (<< nd 3) #b111))))
    ((fusions)
     (let ((nd (apply nd-encoding (second c))))
       (bytes (+ (<< nd 3) #b110))))
    ((fission)
     (let ((nd (apply nd-encoding (second c)))
	   (m (third c)))
       (bytes (+ (<< nd 3) #b101)
	      m)))
    ((fill)
     (let ((nd (apply nd-encoding (second c))))
       (bytes (+ (<< nd 3) #b011))))))
