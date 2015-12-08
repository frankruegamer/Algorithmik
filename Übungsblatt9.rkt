#lang racket
; Aufgabe 41
(define (tuerme-von-hanoi n)
  (0))

; Aufgabe 42
(define (liste-teilen eingabe)
  (define (sort-iter liste ll rl bool)
    (if (null? liste) (list ll rl)
        (if bool (sort-iter (cdr liste) ll (append rl (take liste 1)) (not bool))
            (sort-iter (cdr liste) (append ll (take liste 1)) rl (not bool)))))
  (sort-iter eingabe '() '() #f))

; Aufgabe 43
(define (listen-verschmelzen eingabe)
  (define (sort-iter dliste eliste bool)
    (if (and (null? (car dliste)) (null? (cadr dliste))) eliste
        (if bool (sort-iter (list (car dliste) (drop (cadr dliste) 1)) (append eliste (take (cadr dliste) 1)) (not bool))
            (sort-iter (list (drop (car dliste) 1) (cadr dliste)) (append eliste (take (car dliste) 1)) (not bool)))))
  (sort-iter eingabe '() #f))

; Aufgabe 44
(define (hamming bin1 bin2)
  (if (null? bin1) 0
      (+ (if (= (car bin1) (car bin2)) 0 1) (hamming (drop bin1 1) (drop bin2 1)))))