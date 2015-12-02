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
  (if (= (quotient zahl 10) 0) #f
      (scan-list (make-list (abs zahl)) 0)))

; Aufgabe 2
(define (primzahlen f)
  (define (primzahlen-iter n)
    (if (not (prime? (f n))) (if (= n 0) #f (- n 1))
        (primzahlen-iter (+ n 1))))
  (primzahlen-iter 0))

; Version für Algo-Server, da (prime? 0) und (prime? 1) dort #t
(define (primzahlen-server f)
  (define (prime-fix? n)
    (if (< n 2) #f
        (prime? n)))
  (define (primzahlen-iter n)
    (if (not (prime-fix? (f n))) (if (= n 0) #f (- n 1))
        (primzahlen-iter (+ n 1))))
  (primzahlen-iter 0))