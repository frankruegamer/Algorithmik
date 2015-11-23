#lang racket
; Aufgabe 1
(define x (- (/ (+ 9 6) (* (- 3 1) 5)) (* (- (/ 7 8) 2) 4)))
; Aufgabe 2
(define (g u v w) (+ (/ (- v (* 7 u)) (- u w)) (/ (+ u v) (- (* w 6) v))))
; Aufgabe 3
(define (my-max x y) (cond ((> x y) x) (else y)))
; Aufgabe 4
(define (groesser-zehn? x) (> x 10))
; Aufgabe 5
(define (groesserp? x y z) (> (+ x y) z))
; Aufgabe 6
(define (quadrat x) (* x x))
(define (squared-max1 x y z)
  (cond
    ((and (>= (quadrat x) (quadrat y)) (>= (quadrat x) (quadrat z))) (quadrat x))
    ((and (>= (quadrat y) (quadrat x)) (>= (quadrat y) (quadrat z))) (quadrat y))
    ((and (>= (quadrat z) (quadrat x)) (>= (quadrat z) (quadrat y))) (quadrat z))
    ))
(define (squared-max2 x y z)
  (cond
    ((>= (quadrat x) (/ (+ (quadrat y) (quadrat z)) 2)) (quadrat x))
    ((>= (quadrat y) (/ (+ (quadrat x) (quadrat z)) 2)) (quadrat y))
    ((>= (quadrat z) (/ (+ (quadrat x) (quadrat y)) 2)) (quadrat z))
    ))
(define (squared-max3 x y z)
  (cond
    ((>= (abs x) (/ (+ (abs y) (abs z)) 2)) (* x x))
    ((>= (abs y) (/ (+ (abs x) (abs z)) 2)) (* y y))
    ((>= (abs z) (/ (+ (abs x) (abs y)) 2)) (* z z))
    ))
; ACHTUNG: Erfüllt nicht vollständig die Vorgaben!
(define (squared-max4 x y z)
  (cond
    ((>= (abs x) (/ (+ (abs y) (abs z)) 2)) (* x x))
    ((>= (abs y) (abs z)) (* y y))
    (else (* z z))
    ))
(define (squared-max x y z)
  (cond
    ((and (>= (abs x) (abs y)) (>= (abs x) (abs z))) (* x x))
    ((>= (abs y) (abs z)) (* y y))
    (else (* z z))
    ))