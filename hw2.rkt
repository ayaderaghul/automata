#lang racket
(require "./dfa.rkt")

(define R1 (rx "(000*+111*)*"))
(define M1
  (dfa (alphabet: "01")
       (table:
        [∅  : ∅  ∅]
        [ε  : o  1]
        [o  : oo ∅]
        [1  : ∅ 11]
        [oo : oo 1]
        [11 : o 11])
       (start: ε)
       (accept: ε oo 11)))

(define R2 (rx "(01+10)(01+10)(01+10)"))
(define M2
  (dfa (alphabet: "01")
       (table:
        [     ∅ :     ∅     ∅]
        [(0  ε) : (0 0) (0 1)]
        [(0  0) :     ∅ (1 ε)]
        [(0  1) : (1 ε)     ∅]
        [(1  ε) : (1 0) (1 1)]
        [(1  0) :     ∅ (2 ε)]
        [(1  1) : (2 ε)     ∅]
        [(2  ε) : (2 0) (2 1)]
        [(2  0) :     ∅ (3 ε)]
        [(2  1) : (3 ε)     ∅]
        [(3  ε) :     ∅     ∅])
       (start: (0 ε))
       (accept: (3 ε))))

(define R3 (rx "(0+1(01*0)*1)*"))
(define M3
  (dfa (alphabet: "01")
       (table:
        [ε   : ε    1]
        [1   : 10   ε]
        [10  : 1  10])
       (start: ε)
       (accept: ε)))

(define R4 (rx "a(a+b)*"))
(define M4
  (dfa (alphabet: "ab")
       (table:
        [∅     :     ∅ ∅]
        [(1)   : (1 2) ∅]
        [(1 2) : (1 2) (1 2)])
       (start: (1))
       (accept: (1 2))))

(define R5 (rx "(a+b)(aa+ab)*b((a+b)(a+b)(aa+ab)*b)*"))
(define M5
  (dfa (alphabet: "ab")
       (table:
        [∅   : ∅ ∅]
        [(1) : (2) (2)]
        [(2) : (1) (3)]
        [(3) : (1) (1)])
       (start: (1))
       (accept: (3))))

(and
 (brute-force-check M1 R1 '("0" "1") 15)
 (brute-force-check M2 R2 '("0" "1") 15)
 (brute-force-check M3 R3 '("0" "1") 15)
 (brute-force-check M4 R4 '("a" "b") 15)
 (brute-force-check M5 R5 '("a" "b") 15))
