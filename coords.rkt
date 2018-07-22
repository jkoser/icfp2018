#lang racket

(provide (all-defined-out))

;; Coordinates are represented as 3-lists
(define (make-c x y z)
  (list x y z))
(define c-x first)
(define c-y second)
(define c-z third)

;; Coordinate diffs are represented as 3-lists
(define (make-d x y z)
  (list x y z))
(define d-x first)
(define d-y second)
(define d-z third)

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
(define (sld? d)
  (and (ld? d)
       (<= (mlen d) 5)))

;; Long linear coordinate diff
(define (lld? d)
  (and (ld? d)
       (<= (mlen d) 15)))

;; Near coordinate diff
(define (nd? d)
  (and (<= 1 (mlen d) 2)
       (= (clen d) 1)))
