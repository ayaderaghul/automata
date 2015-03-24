#lang racket
(require "./machines.rkt")

(define-syntax (dfa stx)
  (define (q-list stx)
    (map (λ(stx)(syntax-case stx (:) [(p : q ...) #'(quote p)]))
         (syntax->list stx)))
  (define (list-rows stx alp)
    (map (λ(stx)(syntax-case stx (:) [(p : q ...) #`(map cons #,alp '(q ...))]))
         (syntax->list stx)))
  (syntax-case stx (alphabet: states: table: start: accept:)
    [(_ (alphabet: Σ) (table: row ...) (start: s0) (accept: sa ...))
     #`(let* ([Q (list #,@(q-list #'(row ...))) ]
              [alp (sequence->list Σ)]
              [rows (map make-immutable-hash
                         (list #,@(list-rows #'(row ...) #'alp)))]
              [table (make-immutable-hash (map cons Q rows))])
         (deterministic-finite-machine
          (λ(state input)(hash-ref (hash-ref table state) input))
          's0 (λ(s)(set-member? (set 'sa ...) s)) Q  Σ))]))

(define-syntax (nfa stx)
  (define (q-list stx)
    (map (λ(stx)(syntax-case stx (:) [(p : qs ...) #'(quote p)]))
         (syntax->list stx)))
  (define (list-rows stx alp)
    (map (λ(stx)(syntax-case stx (:) [(p : qs ...)
                                      #`(map cons #,alp (list (apply set 'qs) ...))]))
         (syntax->list stx)))
  (syntax-case stx (alphabet: states: table: start: accept:)
    [(_ (alphabet: Σ) (table: row ...) (start: s0 ...) (accept: sa ...))
     #`(let* ([Q (list #,@(q-list #'(row ...))) ]
              [alp (sequence->list Σ)]
              [rows (map make-immutable-hash
                         (list #,@(list-rows #'(row ...) #'alp)))]
              [table (make-immutable-hash (map cons Q rows))])
         (non-deterministic-finite-machine
          (λ(p a)(hash-ref (hash-ref table p) a))
          (set 's0 ...) (λ(S)(not (set-empty? (set-intersect S (set 'sa ...))))) Q Σ))]))

(provide dfa nfa)
