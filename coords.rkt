#lang typed/racket

(provide (all-defined-out))

;; Coordinates
(struct c ([x : Integer] [y : Integer] [z : Integer]) #:transparent)
(define x c-x)
(define y c-y)
(define z c-z)
(define-type Coord c)

;; Coordinate differences
(struct d ([x : Integer] [y : Integer] [z : Integer]) #:transparent)
(define dx d-x)
(define dy d-y)
(define dz d-z)
(define-type CoordDiff d)

;; Regions are represented as pairs of coordinates, normalized so we can
;; use equal? to compare.
(define-type Region (Pairof Coord Coord))
(: region (-> Coord Coord Region))
(define (region c1 c2)
  (cons (c (min (x c1) (x c2))
           (min (y c1) (y c2))
           (min (z c1) (z c2)))
        (c (max (x c1) (x c2))
           (max (y c1) (y c2))
           (max (z c1) (z c2)))))

(: xmin (-> Region Integer))
(define (xmin r) (x (car r)))
(: xmax (-> Region Integer))
(define (xmax r) (x (cdr r)))
(: ymin (-> Region Integer))
(define (ymin r) (y (car r)))
(: ymax (-> Region Integer))
(define (ymax r) (y (cdr r)))
(: zmin (-> Region Integer))
(define (zmin r) (z (car r)))
(: zmax (-> Region Integer))
(define (zmax r) (z (cdr r)))

(: region-below-x (-> Region Integer Region))
(define (region-below-x r x)
  (region (car r)
          (c (min x (xmax r)) (ymax r) (zmax r))))

(: region-above-x (-> Region Integer Region))
(define (region-above-x r x)
  (region (c (max x (xmin r)) (ymin r) (zmin r))
          (cdr r)))

(: region-below-y (-> Region Integer Region))
(define (region-below-y r y)
  (region (car r)
          (c (xmax r) (min y (ymax r)) (zmax r))))

(: region-above-y (-> Region Integer Region))
(define (region-above-y r y)
  (region (c (xmin r) (max y (ymin r)) (zmin r))
          (cdr r)))

(: region-below-z (-> Region Integer Region))
(define (region-below-z r z)
  (region (car r)
          (c (xmax r) (ymax r) (min z (zmax r)))))

(: region-above-z (-> Region Integer Region))
(define (region-above-z r z)
  (region (c (xmin r) (ymin r) (max z (zmin r)))
          (cdr r)))

(: region-includes? (-> Region Coord Boolean))
(define (region-includes? r p)
  (and (<= (xmin r) (x p) (xmax r))
       (<= (ymin r) (y p) (ymax r))
       (<= (zmin r) (z p) (zmax r))))

;; Region dimension
(: dim (-> Region Nonnegative-Integer))
(define (dim r)
  (+ (if (= (xmin r) (xmax r)) 0 1)
     (if (= (ymin r) (ymax r)) 0 1)
     (if (= (zmin r) (zmax r)) 0 1)))

(: c+ (-> Coord CoordDiff Coord))
(define (c+ p q)
  (c (+ (x p) (dx q))
     (+ (y p) (dy q))
     (+ (z p) (dz q))))

(: c- (-> Coord Coord CoordDiff))
(define (c- p q)
  (d (- (x p) (x q))
     (- (y p) (y q))
     (- (z p) (z q))))

;; Manhattan length
(: mlen (-> CoordDiff Nonnegative-Integer))
(define (mlen q)
  (+ (abs (dx q)) (abs (dy q)) (abs (dz q))))

;; Chessboard length
(: clen (-> CoordDiff Nonnegative-Integer))
(define (clen q)
  (max (abs (dx q)) (abs (dy q)) (abs (dz q))))

;; Adjacency
(: adj? (-> Coord Coord Boolean))
(define (adj? c1 c2)
  (= (mlen (c- c1 c2)) 1))

;; Linear coordinate diff
(: ld? (-> CoordDiff Boolean))
(define (ld? d)
  (let ((ml (mlen d))
        (cl (clen d)))
    (and (= ml cl)
         (> ml 0))))

;; Short linear coordinate diff
(define SLD-MAX 5)
(: sld? (-> CoordDiff Boolean))
(define (sld? d)
  (and (ld? d)
       (<= (mlen d) SLD-MAX)))

;; Long linear coordinate diff
(define LLD-MAX 15)
(: lld? (-> CoordDiff Boolean))
(define (lld? d)
  (and (ld? d)
       (<= (mlen d) LLD-MAX)))

;; Near coordinate diff
(: nd? (-> CoordDiff Boolean))
(define (nd? d)
  (and (<= 1 (mlen d) 2)
       (= (clen d) 1)))

;; Far coordinate diff
(define FD-MAX 30)
(: fd? (-> CoordDiff Boolean))
(define (fd? d)
  (<= 1 (clen d) FD-MAX))
