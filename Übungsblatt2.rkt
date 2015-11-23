#lang racket
; Aufgabe 7
(define (nat-wurzel x)
  (wurzel-iter x 1 0))
(define (wurzel-iter x p s)
  (if (< s x) (+ 1 (wurzel-iter x (+ p 2) (+ s p)))
              0))

; Aufgabe 8
(define (zahl-umdrehen x)
  (zahl-help 0 x))
(define (zahl-help zahl y)
  (if (= y 0) (/ zahl 10) (zahl-help (* (+ zahl (remainder y 10)) 10) (quotient y 10))))

; Aufgabe 9
; ACHTUNG: Summe, kein Produkt!
(define (aufsteigendes-produkt?1 a b c d)
  (and (< a b) (< b c) (< c d) (= (+ a b c) d)))
(define (aufsteigendes-produkt? a b c d)
  (and (< a b c d) (= (* a b c) d)))

; Aufgabe 10
(define (f1 a b)
  (and (not (or a b)) (or a b) a (not b)))
(define (f2 a b c)
  (or a (and a b (not c)) (and (not a) c) (and (not (and a b)) c)))
(define (f3 a b c d)
  (and (and (or (not a) b) (not (and (not a) b))) (not (or a (not b) c)) (and (not d) (not c) (not b) (not a))))