#lang racket
; Sommersemester 2012
; Aufgabe T2.G1.A1
(define (zaehleteiler n)
  (define (zaehle-rek x)
    (if (>= x n) 0
        (+ (if (= (modulo n x) 0) 1 0) (zaehle-rek (+ x 1)))))
  (zaehle-rek 2))

; Aufgabe T2.G1.A2
(define (findeziffer a b)
  (define (make-listr n)  ; Zahl umgekehrt in Liste
    (if (= n 0) '()
        (list* (remainder n 10) (make-listr (quotient n 10)))))
  (define (scan-list l bool)
    (cond ((null? l) (if bool b 0))
          ((= (car l) b) (scan-list (cdr l) #t))
          (bool (car l))
          (else (scan-list (cdr l) #f))))
  (scan-list (make-listr (abs a)) #f))

; Sommersemester 2013
; Aufgabe T2.G1.A1
(define (zahl-modulieren zahl teiler)
  (define (iter-n n ex)
    (if (= n 0) 0
        (+ (* (expt 10 ex) (remainder (remainder n 10) teiler)) (iter-n (quotient n 10) (+ ex 1)))))
  (if (or (< zahl 0) (not (< 0 teiler 11))) -1
      (iter-n zahl 0)))

; Aufgabe T2.G1.A2
(define (einstellige-quersumme zahl)
  (define (quersumme n)
    (if (= n 0) 0
        (+ (remainder n 10) (quersumme (quotient n 10)))))
  (if (= (quotient zahl 10) 0) zahl
      (einstellige-quersumme (quersumme (abs zahl)))))

; Sommersemester 2014
; Aufgabe T2.G1.A1
(define (zaehle start ende praedikat)
  (define (zaehle-ziffern n)
    (if (= n 0) 0
        (+ (if (praedikat (remainder n 10)) 0 1) (zaehle-ziffern (quotient n 10)))))
  (if (> start ende) 0
      (+ (zaehle-ziffern (abs start)) (zaehle (+ start 1) ende praedikat))))

; Aufgabe T2.G1.A2
(define (r n a)
  (if (<= n 0) 0
      (+ (a n) (r (- n 1) a))))