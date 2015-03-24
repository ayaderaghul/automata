#lang racket
(require "machines.rkt")
(define (pairs-of A)
  (let ([lst (set->list A)])
    (apply set-union
     (let loop ([a (car lst)][rst (cdr lst)])
       (if (null? rst) '()
           (cons (list->set (map (λ(x)(set x a)) rst))
                 (loop (car rst) (cdr rst))))))))

(define (accessible M)
  (define alp (alphabet-of M))
  (let loop ([A (set (start-of M))])
    (let ([A* (for*/set ([a alp][p A]) (transition M p a))])
      (if (equal? A A*) A (loop (set-union A A*))))))


(define (stage1 M S)
  (define F? (machine-accept M))
  (let-values
      ([(Marked Unmarked)
        (for/fold ([Marked '()][Unmarked '()])
                  ([pq S])
          (match-let ([(list p q) (set->list pq)])
            (if (xor (F? p) (F? q))
                (values (cons pq Marked) Unmarked)
                (values Marked (cons pq Unmarked)))))])
    (values (list->set Marked) (list->set Unmarked))))

(define (trans-pair M pq a)
  (match-let ([(list p q) (set->list pq)])
    (set (transition M p a) (transition M q a))))


(define (pass-once Marked Unmarked M)
  (define F? (machine-accept M))
  (define alp (alphabet-of M))
  (for/set ([pq Unmarked]
            #:when (for/or ([a alp])(set-member? Marked (trans-pair M pq a))))
            pq))

(define (classes M)
  (let-values ([(Marked₁ Unmarked₁)(stage1 M (pairs-of (accessible M)))])
    (let loop ([Marked Marked₁][Unmarked Unmarked₁])
      (let ([NewMarked (pass-once Marked Unmarked M)])
        (if (set-empty? NewMarked) Unmarked
            (loop (set-union Marked NewMarked) (set-subtract Unmarked NewMarked)))))))


(define (pass-n Marked Unmarked M n)
  (let ([NewMarked (pass-once Marked Unmarked M)])
    (if (= n 0) NewMarked
        (pass-n (set-union Marked NewMarked) (set-subtract Unmarked NewMarked) M (- n 1)))))

#|
(let-values ([(Marked Unmarked)
              (stage1 M1 (pairs-of (accessible M1)))])
  (pass-n Marked Unmarked M1 0))|#


