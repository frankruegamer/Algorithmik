#lang racket
; Aufgabe 11
(define (sinus-approx x)
  (if (<= x 0.1) x
  (- (* 3 (sinus-approx (/ x 3))) (* 4 (expt (sinus-approx (/ x 3)) 3)))))

; Aufgabe 12
(define (count-perm x)
  (fak x))
(define (fak x)
  (fak-iter 1 1 x))
(define (fak-iter produkt zaehler n)
  (if (> zaehler n) produkt
  (fak-iter (* zaehler produkt) (+ zaehler 1) n)))

; Aufgabe 13
(define (isbn-test isbn)
  (kontr-help (kontrollz (summe isbn 9))))
(define (summe x n)
  (if (<= n 1) x
  (+ (* n (remainder x 10)) (summe (quotient x 10) (- n 1)))))
(define (kontrollz summe)
  (modulo summe 11))
(define (kontr-help kontrollz)
  (if (= kontrollz 10) "X" kontrollz))

; Aufgabe 14
(define (zylinder-kegel radius-zylinder hoehe-zylinder
                        radius-kegel hoehe-kegel)
  (/ (volumen-zylinder radius-zylinder hoehe-zylinder)
     (volumen-kegel radius-kegel hoehe-kegel)))
(define (volumen-zylinder radius hoehe)
  (* pi (expt radius 2) hoehe))
(define (volumen-kegel radius hoehe)
  (/ (volumen-zylinder radius hoehe) 3))