#lang racket
(require "machines.rkt" "syntax.rkt" "tools.rkt" "minimize.rkt")

   
(define M1
  (dfa
   (alphabet: "ab")
   (table:
    [1 : 2 5]
    [2 : 1 4]
    [3 : 7 2]
    [4 : 5 7]
    [5 : 4 3]
    [6 : 3 6]
    [7 : 3 1])
   (start: 1)
   (accept: 1 2)))

(define M2
  (dfa
   (alphabet: "ab")
   (table:
    [1 : 2 6]
    [2 : 1 7]
    [3 : 5 2]
    [4 : 2 3]
    [5 : 3 1]
    [6 : 7 3]
    [7 : 6 5])
   (start: 1)
   (accept: 1 2)))

(define M3
  (dfa
   (alphabet: "ab")
   (table:
    [1 : 1 3]
    [2 : 6 3]
    [3 : 5 7]
    [4 : 6 1]
    [5 : 1 7]
    [6 : 2 7]
    [7 : 5 3])
   (start: 1)
   (accept: 2 4 6)))
(define M4
  (dfa
   (alphabet: "ab")
   (table:
    [1 : 2 5]
    [2 : 1 6]
    [3 : 4 3]
    [4 : 7 1]
    [5 : 6 7]
    [6 : 5 4]
    [7 : 4 2])
   (start: 1)
   (accept: 1 2)))

(define M0
  (dfa
   (alphabet: "ab")
   (table:
    [1 : 1 1]
    [0 : 0 0])
   (start: 1)
   (accept: 1)))

(define N4
  (nfa (alphabet: "ab")
       (table:
        [s : (s t) (s)]
        [t : (u)    ()]
        [u : ()    (v)]
        [v : ()     ()])
       (start: s)
       (accept: v)))
