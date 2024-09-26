;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 4bit-addition) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
; Binary is one of:
; - 0
; - 1

; 4Bit is (list Binary Binary Binary Binary)
; example:
(define ZERO (list 0 0 0 0))
(define ONE  (list 0 0 0 1))
;(list 0 1 0 0)
;(list 1 1 1 1)

; 4Bit -> 4Bit
; perform 4-bit addition.
;(check-expect (add-binary ZERO ZERO) ZERO)
;(check-expect (add-binary ZERO ONE) ONE)
;(check-expect (add-binary ONE ONE) '(0 0 1 0))
;(check-expect (add-binary '(0 1 0 0) '(0 1 0 1)) '(1 0 0 1)) ; 4 + 5
(define (add-binary a b)
  (local ((define x (reverse a))
          (define y (reverse b))
          ; 4Bit N [List-of Binary] -> 4Bit
          (define (add/a a b remainder result)
            (cond
              [(empty? a) result]
              [else
               (local ((define add (+ (first a) (first b) remainder))
                       (define rest-a (rest a))
                       (define rest-b (rest b)))
                 (if (<= add 1)
                     (add/a rest-a rest-b 0 (cons add result))
                     (add/a rest-a rest-b 1 (cons 0 result))))])))
    (add/a x y 0 '())))

; [List-of Binary] -> [List-of Binary]
; increment n.
(define (add1-binary n)
  (local ((define ONE '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1)))
    (add-binary n ONE)))

; N -> [List-of Binary]
; translate decimal notation number into 15-bit binary notation.
(check-expect (binary16 0)     '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
(check-expect (binary16 1)     '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1))
(check-expect (binary16 15)    '(0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1))
(check-expect (binary16 32767) '(0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1))
(define (binary16 n)
  (local (; N [List-of Binary] -> [List-of Binary]
          (define (binary/a n accu)
            (cond [(zero? n) accu]
                  [else (binary/a (sub1 n) (add1-binary accu))])))
    (binary/a n '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))))
  