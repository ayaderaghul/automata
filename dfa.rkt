#lang racket
(require (for-syntax syntax/parse racket/match))
(struct machine (transition start accept?))
(define (dfa-apply M seq)
  (let ([δ (machine-transition M)]
          [s (machine-start M)])
      (sequence-fold δ s seq)))
(define (in-dfa? M seq)
  ((machine-accept? M) (dfa-apply M seq)))

(define (alternate seq1 seq2)
  (for/fold ([lst '()])
            ([i seq1][j seq2])
    (cons i (cons j lst))))
(define-syntax (table stx)
  (define (transform alp lst)
    (match lst 
      ['() '()]
      [(cons row rst)
       (syntax-case row (:)
         [(x : s ...)
          (cons #'(quote x)
                (cons #`(apply hash (alternate #,alp (quote (s ...))))
                      (transform alp rst)))])]))
  (syntax-case stx ()
    [(table #:alphabet alp
       row ...)
     (with-syntax 
       ([(new-row ...)
         (transform #'alp (syntax->list #'(row ...)))])
       #'(let ([h (hash new-row ...)])
           (λ(state input)(hash-ref (hash-ref h state) input))))]))
(define-syntax (dfa stx)
  (syntax-case stx (alphabet: table: start: accept:)
      [(_ (alphabet: seq) (table: row ...) (start: si) (accept: sa ...))
       #'(machine (table #:alphabet seq row ...) (quote si)
                  (λ(s)(set-member? (quote (sa ...)) s)))]))
(define (rx str)
  (regexp (string-append "^" (string-replace str "+" "|") "$")))

(define (brute-force-check M r alp len)
  (let loop ([str ""][num len])
    (if (zero? num) #t
        (for/and ([a alp])
          (let ([str0 (string-append str a)])
            (and (if (boolean=? (in-dfa? M str0) (regexp-match? r str0)) #t
                     (begin (printf "~a\n" str0) #f))
                 (loop str0 (- num 1))))))))
(provide dfa in-dfa? dfa-apply brute-force-check rx)
