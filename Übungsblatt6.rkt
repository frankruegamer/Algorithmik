#lang racket
; Aufgabe 27
(define (zaehlen start ende n)
  (define (teilbar? a d)
    (= (modulo a d) 0))
  (define (teilbar-iter x n)
    (cond ((= n 1) x)
          ((or (< (abs (- ende x)) 21) (< n 1)) 0)
          (else (teilbar-iter ((if (< x ende) + -) x 21) (- n 1)))))
  (cond ((= start ende) 0)
        ((teilbar? start 21) (teilbar-iter start n))
        (else (zaehlen ((if (< start ende) + -) start 1) ende n))))

; Aufgabe 28
(define (gleiche-ziffern zahl)
  (define (make-list n)
    (if (= n 0) '()
        (list* (remainder n 10) (make-list (quotient n 10)))))
  (define (last l)
    (if (= (length l) 1) (car l)
        (last (cdr l))))
  (define (gleiche-ziffer? n)
    (let ((l (make-list n)))
      (= (car l) (last l))))
  (if (gleiche-ziffer? zahl) zahl
      (gleiche-ziffern (+ zahl 1))))

; Aufgabe 29
(define (konst-addierer n)
  (lambda (a) (+ n a)))

; Aufgabe 30
(define (konst-ggt b)
  (define (ggt a b)
    (if (= b 0) a
        (ggt b (modulo a b))))
  (lambda (a) (ggt b a)))

; Aufgabe 31
(define (paar-operation op)
  (lambda (paar) (op (car paar) (cdr paar))))

; Aufgabe 32
(define struktur1
  (list (list 24 1 2) 20 1 1))
(define struktur2
  (list (list 7 2 3 1)))
(define struktur3
  (list (list 3 3) 3 3 3 3))
(define struktur4
  (list (list (list '() 6 1 0 2 4) 3 3) 10 7 8 9 3))