#lang racket
; Aufgabe 15
(define (ganzzahlige-wurzel? n)
  (integer? (sqrt n)))

; Aufgabe 16
(define (fakt n)
  (define (new-b2 a)
      (- (* a a) n))
  (define (fakt-help a)
    (let ((b2 (new-b2 a)))
      (if (ganzzahlige-wurzel? b2) (- a (sqrt b2))
         (fakt-help (+ a 1)))))
  (if (even? n) 2 (fakt-help (ceiling (sqrt n)))))

; Aufgabe 17
(define (primzahl? n)
  (cond ((< n 2) #f)
        ((= n 2))
        ((= (fakt n) 1))))

; Aufgabe 18
(define (kubiksumme n)
  (define (quersumme n)
    (if (= n 0) 0
        (+ (remainder n 10) (quersumme (quotient n 10)))))
  (expt (quersumme n) 3))

; Aufgabe 19
(define (caesar_encrypt2 n k)
  (define (zahl-umdrehen x)
    (define (zahl-help zahl y)
      (if (= y 0) (/ zahl 10)
          (zahl-help (* (+ zahl (remainder y 10)) 10) (quotient y 10))))
    (zahl-help 0 x))
  (define (move x)
    (modulo (+ x k) 10))
  (define (encrypt zahl y)
    (if (= y 0) (/ zahl 10)
        (encrypt (* (+ zahl (move (remainder y 10))) 10) (quotient y 10))))
  (encrypt 0 (zahl-umdrehen n)))

(define (caesar_encrypt n k)  
  (define (caesar-help n c s)
    (define (move x)
    (modulo (+ x k) 10))
    (if (= n 0) (* c (expt 10 (- s 1)))
        (caesar-help (quotient n 10) (+ (/ c 10) (move n)) (+ s 1))))
  (caesar-help n 0 0))
