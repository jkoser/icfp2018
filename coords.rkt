#lang racket

(provide (all-defined-out))

;; Coordinates are represented as 3-lists
(define (make-c x y z)
  (list x y z))
(define x first)
(define y second)
(define z third)

;; Coordinate diffs are represented as 3-lists
(define (make-d x y z)
  (list x y z))
(define dx first)
(define dy second)
(define dz third)

;; Regions are represented as 2-lists of coordinates, normalized so
;; we can use equal? to compare.
(define (make-region c1 c2)
  (list (make-c (min (x c1) (x c2))
                (min (y c1) (y c2))
                (min (z c1) (z c2)))
        (make-c (max (x c1) (x c2))
                (max (y c1) (y c2))
                (max (z c1) (z c2)))))

(define (xmin r) (x (first r)))
(define (xmax r) (x (second r)))
(define (ymin r) (y (first r)))
(define (ymax r) (y (second r)))
(define (zmin r) (z (first r)))
(define (zmax r) (z (second r)))

(define (region-below-x r x)
  (make-region (first r)
               (make-c (min x (xmax r)) (ymax r) (zmax r))))

(define (region-above-x r x)
  (make-region (make-c (max x (xmin r)) (ymin r) (zmin r))
               (second r)))

(define (region-below-y r y)
  (make-region (first r)
               (make-c (xmax r) (min y (ymax r)) (zmax r))))

(define (region-above-y r y)
  (make-region (make-c (xmin r) (max y (ymin r)) (zmin r))
               (second r)))

(define (region-below-z r z)
  (make-region (first r)
               (make-c (xmax r) (ymax r) (min z (zmax r)))))

(define (region-above-z r z)
  (make-region (make-c (xmin r) (ymin r) (max z (zmin r)))
               (second r)))

(define (region-includes? r c)
  (and (<= (xmin r) (x c) (xmax r))
       (<= (ymin r) (y c) (ymax r))
       (<= (zmin r) (z c) (zmax r))))

;; Region dimension
(define (dim r)
  (+ (if (= (x (first r)) (x (second r))) 0 1)
     (if (= (y (first r)) (y (second r))) 0 1)
     (if (= (z (first r)) (z (second r))) 0 1)))

;; Some possibly helpful units
(define up '(0 1 0))
(define down '(0 -1 0))
(define north '(0 0 1))
(define south '(0 0 -1))
(define east '(1 0 0))
(define west '(-1 0 0))

(define (c+ c d)
  (map + c d))

(define (c- c1 c2)
  (map - c1 c2))

;; Manhattan length
(define (mlen d)
  (apply + (map abs d)))

;; Chessboard length
(define (clen d)
  (apply max (map abs d)))

;; Adjacency
(define (adj? c1 c2)
  (= (mlen (c- c1 c2)) 1))

;; Linear coordinate diff
(define (ld? d)
  (let ((ml (mlen d))
        (cl (clen d)))
    (and (= ml cl)
         (> ml 0))))

;; Short linear coordinate diff
(define SLD-MAX 5)
(define (sld? d)
  (and (ld? d)
       (<= (mlen d) SLD-MAX)))

;; Long linear coordinate diff
(define LLD-MAX 15)
(define (lld? d)
  (and (ld? d)
       (<= (mlen d) LLD-MAX)))

;; Near coordinate diff
(define (nd? d)
  (and (<= 1 (mlen d) 2)
       (= (clen d) 1)))

;; Far coordinate diff
(define FD-MAX 30)
(define (fd? d)
  (<= 1 (clen d) FD-MAX))
