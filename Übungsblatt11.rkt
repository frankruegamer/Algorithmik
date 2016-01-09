#lang racket
; Aufgabe 49
(define (werte-aus term zuweisung)
  (define op (case (car term) ('+ +) ('- -) ('* *) ('/ /)))
  (define (eval symbol)
    (if (integer? symbol) symbol
        (for/first [(l zuweisung)
                    #:when (equal? symbol (car l))]
          (cadr l))))
  (op (eval (cadr term)) (eval (caddr term))))

; Aufgabe 50
(define (deep-memq symbol liste)
  (if (not (memq symbol (flatten liste))) #f #t))

; Aufgabe 51
(define (alle-kleineren grenze liste)
  (filter (lambda (x) (< x grenze)) (flatten liste)))

; Aufgabe 52
(define (suche-schrittzahl n)
  (define (schrittzahl? number)
    (cond ((< number 10) #t)
          ((not (= (abs (- (remainder number 10) (remainder (quotient number 10) 10))) 1)) #f)
          (else (schrittzahl? (quotient number 10)))))
  (define (iteration zahl n)
    (if (schrittzahl? zahl)
        (if (= n 1) zahl (iteration (+ zahl 1) (- n 1)))
        (iteration (+ zahl 1) n)))
  (iteration 10 n))