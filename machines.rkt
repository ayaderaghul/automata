#lang racket
(require racket/generic)

(define-generics extendable-transition
  (extended-transition extendable-transition s seq)
  #:defaults ([(λ(i)#t)
               (define (extended-transition M state seq)
                 (raise-support-error 'extended-transition M))]))

(define-generics determines-set
  (in-machine? determines-set seq)
  #:defaults ([(λ(i)#t)
               (define (extended-transition M state seq)
                 (raise-support-error 'extended-transition M))]))

(struct machine (transition start accept)
  #:methods gen:determines-set
  [(define (in-machine? M seq)
     ((machine-accept M)
      (extended-transition M (machine-start M) seq )))])

(struct finite-machine (states alphabet)
  #:super struct:machine)

(struct deterministic-finite-machine ()
  #:super struct:finite-machine
  #:methods gen:extendable-transition
  [(define (extended-transition M state seq)
     (sequence-fold (machine-transition M) state seq))])

(define-syntax-rule (U ([x S]) E)
  (apply set-union (for/list ([x S]) E)))

(struct non-deterministic-finite-machine ()
  #:super struct:finite-machine
  #:methods gen:extendable-transition
  [(define (extended-transition M S seq)
     (let ([Δ (machine-transition M)])
       (for/fold ([A S]) ([a seq])
         (U ([p A]) (Δ p a)))))])

(define (transition M s a)
  ((machine-transition M) s a))

(provide in-machine? transition machine-accept
         deterministic-finite-machine
         non-deterministic-finite-machine
         (rename-out
          [machine-start start-of]
          [finite-machine-states states-of]
          [finite-machine-alphabet alphabet-of]))
