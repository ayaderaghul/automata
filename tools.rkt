#lang racket
(require "./machines.rkt" "./syntax.rkt")

(define (rx str)
  (regexp (string-append "^" (string-replace str "+" "|") "$")))

(define (brute-force-check M r alp len)
  (let loop ([str ""][num len])
    (if (zero? num) #t
        (for/and ([a alp])
          (let ([str0 (string-append str a)])
            (and (if (boolean=? (in-machine? M str0) (regexp-match? r str0)) #t
                     (begin (printf "~a\n" str0) #f))
                 (loop str0 (- num 1))))))))


(provide brute-force-check rx)
