#lang racket
; Aufgabe 33
(define (loesche liste praedikat)
  (if (not (praedikat (car liste))) liste
      (loesche (cdr liste) praedikat)))

; Aufgabe 34
(define (drehe liste)
  (reverse liste))

; Aufgabe 35
(define (typ-or typ1 typ2)
  (lambda (x) (or (typ1 x) (typ2 x))))

; Aufgabe 36
(define (operation operatoren n)
  (define (nth liste n)
    (if (= n 1) (car liste)
        (nth (cdr liste) (- n 1))))
  (lambda (liste) ((nth operatoren n) (car liste) (cadr liste))))

; Aufgabe 37
(define (caesar_encrypt_list data key)
  (define (shift liste)
    (append (cdr liste) (list (car liste))))
  (define (iter-list liste key)
    (if (null? liste) '()
        (append (list (modulo (+ (car liste) (car key)) 10)) (iter-list (cdr liste) (shift key)))))
  (iter-list data key))