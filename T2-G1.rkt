#lang racket
(require math/number-theory)

; Aufgabe 1
(define (vergleich zahl op)
  (define (make-list n)
    (if (= n 0) '()
        (flatten (list* (make-list (quotient n 10)) (remainder n 10)))))
  (define (scan-list l n)
    (if (> n (- (length l) 2)) 0
        (let ((m (+ n 1)))
          (+ (if (op (list-ref l n) (list-ref l m)) 1 0) (scan-list l (+ n 1))))))
  (scan-list (make-list (abs zahl)) 0))

; Aufgabe 2
(define (primzahlen f)
  (define (primzahlen-iter n)
    (if (not (prime? (f n))) (if (= n 0) #f (- n 1))
        (primzahlen-iter (+ n 1))))
  (primzahlen-iter 0))