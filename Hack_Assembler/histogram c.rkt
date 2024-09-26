;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |histogram c|) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
; [List-of 1String] -> [List N N N]
; ...

(check-expect
 (histogram (explode "")) (list 0 0 0))

(check-expect
 (histogram (explode "a b c")) (list 3 0 0))

(check-expect
 (histogram
  (explode "a b c aaaaa bbbbbbbbbb ccccccccccc"))
 (list 3 1 2))

(define (histogram stream)
  (local (; [List-of !String] N [List N N N] -> [List N N N]
          (define (histogram/a l pool)
            (cond
              [(empty? l) pool]
              [else
               (histogram/a (next l)
                            (update-pool (length (current l)) pool))])))
    (histogram/a stream (list 0 0 0))))

; [List-of 1String] -> [List-of 1String]
; ...
(check-expect (next (explode ""))
              (explode ""))
(check-expect (next (explode "a b c"))
              (explode "b c"))
(check-expect (next (explode "first second"))
              (explode "second"))
(define (next l)
  (cond
    [(empty? l) '()]
    [else
     (if (equal? " " (first l))
         (rest l)
         (next (rest l)))]))

; [List-of 1String] -> [List-of 1String]
; ...
(check-expect (current (explode ""))
              (explode ""))
(check-expect (current (explode "a b c"))
              (explode "a"))
(check-expect (current (explode "first second"))
              (explode "first"))
(define (current l)
  (cond
    [(empty? l) '()]
    [else
     (if (equal? (first l) " ")
         '()
         (cons (first l) (current (rest l))))]))

; N (List N N N) -> (List N N N)
; ...

(check-expect (update-pool 0 (list 0 0 0))
              (list 0 0 0))
(check-expect (update-pool 1 (list 0 0 0))
              (list 1 0 0))
(check-expect (update-pool 5 (list 0 0 0))
              (list 0 1 0))
(check-expect (update-pool 10 (list 0 0 0))
              (list 0 0 1))
(check-expect (update-pool 11 (list 0 0 0))
              (list 0 0 1))

(define (update-pool n pool)
  (list (if (and (< n 5) (> n 0)) (add1 (first pool)) (first pool))
        (if (and (= n 5) (< n 10)) (add1 (second pool)) (second pool))
        (if (or (= n 10) (> n 10)) (add1 (third pool)) (third pool))))


















