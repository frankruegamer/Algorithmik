#lang racket
; Aufgabe 1
(define (note vortrag hausarbeit kolloqium)
  (define (verhaeltnis)
    (/ (+ vortrag hausarbeit (* 2 kolloqium)) 4))
  (define (runden ver)
    (cond ((> ver 4) 5.0)
          ((> ver 3.85) 4.0)
          ((> ver 3.5) 3.7)
          ((> ver 3.15) 3.3)
          ((> ver 2.85) 3.0)
          ((> ver 2.5) 2.7)
          ((> ver 2.15) 2.3)
          ((> ver 1.85) 2.0)
          ((> ver 1.5) 1.7)
          ((> ver 1.15) 1.3)
          (else 1.0)))
  (runden (verhaeltnis)))

; Aufgabe 2
(define (anzahl a b c)
  (if (= a 0) #f
      (let ((diskriminante (- (expt b 2) (* 4 a c))))
        (cond ((> diskriminante 0) 2)
              ((= diskriminante 0) 1)
              (else 0)))))

; Aufgabe 3
(define (f a b c d e)
  (or (and a b d e) (not (or (and c a d) (not (and (and (or a e) (not (and a e))) c))))))