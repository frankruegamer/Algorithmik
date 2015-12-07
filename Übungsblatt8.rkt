#lang racket
; Aufgabe 38
(define (compress liste)
  (define (collect-list rliste cmpr n)
    (cond ((null? rliste) (flatten (list (if (= n 1) '() n) cmpr)))
          ((equal? (car rliste) cmpr) (collect-list (cdr rliste) cmpr (+ n 1)))
          (else (append (collect-list '() cmpr n) (collect-list (cdr rliste) (car rliste) 1)))))
  (if (null? liste) '()
      (collect-list liste (car liste) 0)))

; Aufgabe 39
(define (expandiere sym-liste)
  (define (expand-cons liste)
    (if (= (car liste) 1) (cdr liste)
        (expand-cons (list* (- (car liste) 1) (cadr liste) (cdr liste)))))
  (if (null? sym-liste) '()
      (let ((liste (if (symbol? (car sym-liste)) (append '(1) sym-liste) sym-liste)))
        (flatten (list (expand-cons (take liste 2)) (expandiere (drop liste 2)))))))

; Aufgabe 40
(define (loeschen liste n)
    (if (or (null? liste) (= n 0)) liste
        (loeschen (cdr liste) (- n 1))))