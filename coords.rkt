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
