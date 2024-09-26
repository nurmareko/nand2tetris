;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname coder) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
(define-struct A [address])
(define-struct C [destination compute jump])

; Code is a [List Binary Binary ... Binary] of length 16.
; example:
;'(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) represent 0
;'(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1) represent 1
;'(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1) represent 65535

; Binary is one of:
; - 0
; - 1

; Instruction is one of:
; - A-instruction
; - C-instruction

; A-instruction is a structure (make-A Number)

; C-instruction is a structure (make-C Dest Comp Jump)

; Dest is the first item of a pair on this list of pair:
(define DEST
  '((""   (0 0 0))
    ("M"  (0 0 1))
    ("D"  (0 1 0))
    ("DM" (0 1 1))
    ("MD" (0 1 1))
    ("A"  (1 0 0))
    ("AM" (1 0 1))
    ("MA" (1 0 1))
    ("AD" (1 1 0))
    ("DA" (1 1 0))
    ("ADM"(1 1 1))
    ("AMD"(1 1 1))
    ("DAM"(1 1 1))
    ("MAD"(1 1 1))
    ("DMA"(1 1 1))
    ("MDA"(1 1 1))))

; Comp is the first item of a pair on this list of pair:
(define COMP
  '(("0"  (0 1 0 1 0 1 0))
    ("1"  (0 1 1 1 1 1 1))
    ("-1" (0 1 1 1 0 1 0))
    ("D"  (0 0 0 1 1 0 0))
    ("A"  (0 1 1 0 0 0 0))
    ("M"  (1 1 1 0 0 0 0))
    ("!D" (0 0 0 1 1 0 1))
    ("!A" (0 1 1 0 0 0 1))
    ("!M" (1 1 1 0 0 0 1))
    ("-D" (0 0 0 1 1 1 1))
    ("-A" (0 1 1 0 0 1 1))
    ("-M" (1 1 1 0 0 1 1))
    ("D+1"(0 0 1 1 1 1 1))
    ("1+D"(0 0 1 1 1 1 1))
    ("A+1"(0 1 1 0 1 1 1))
    ("1+A"(0 1 1 0 1 1 1))
    ("M+1"(1 1 1 0 1 1 1))
    ("1+M"(1 1 1 0 1 1 1))
    ("D-1"(0 0 0 1 1 1 0))
    ("1-D"(0 0 0 1 1 1 0))
    ("A-1"(0 1 1 0 0 1 0))
    ("1-A"(0 1 1 0 0 1 0))
    ("M-1"(1 1 1 0 0 1 0))
    ("1-M"(1 1 1 0 0 1 0))
    ("D+A"(0 0 0 0 0 1 0))
    ("A+D"(0 0 0 0 0 1 0))
    ("D+M"(1 0 0 0 0 1 0))
    ("M+D"(1 0 0 0 0 1 0))
    ("D-A"(0 0 1 0 0 1 1))
    ("D-M"(1 0 1 0 0 1 1))
    ("A-D"(0 0 0 0 1 1 1))
    ("M-D"(1 0 0 0 1 1 1))
    ("D&A"(0 0 0 0 0 0 0))
    ("A&D"(0 0 0 0 0 0 0))
    ("D&M"(1 0 0 0 0 0 0))
    ("M&D"(1 0 0 0 0 0 0))
    ("D|A"(0 0 1 0 1 0 1))
    ("A|D"(0 0 1 0 1 0 1))
    ("D|M"(1 0 1 0 1 0 1))
    ("M|D"(1 0 1 0 1 0 1))))

; Jump is the first item of a pair on this list of pair:
(define JUMP
  '((""    (0 0 0))
    ("JGT" (0 0 1))
    ("JEQ" (0 1 0))
    ("JGE" (0 1 1))
    ("JLT" (1 0 0))
    ("JNE" (1 0 1))
    ("JLE" (1 1 0))
    ("JMP" (1 1 1))))
;===================================================================;
(define GIVEN
  (list (make-A 0)
        (make-A 15)
        (make-C "D" "0" "")
        (make-C "M" "M+D" "")
        (make-C "" "0" "JMP")
        (make-C "" "D" "JGT")
        (make-C "D" "M" "")))
;===================================================================;
; [List-of Instruction] -> [List-of Code]
; translate a list of instruction into a list of code.
(check-expect (coder GIVEN)
              '((0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                (0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1)
                (1 1 1 0 1 0 1 0 1 0 0 1 0 0 0 0)
                (1 1 1 1 0 0 0 0 1 0 0 0 1 0 0 0)
                (1 1 1 0 1 0 1 0 1 0 0 0 0 1 1 1)
                (1 1 1 0 0 0 1 1 0 0 0 0 0 0 0 1)
                (1 1 1 1 1 1 0 0 0 0 0 1 0 0 0 0)))
(define (coder l)
  (local (; Instruction -> Code
          (define (translate inst)
            (local (; N -> [List-of Binary]
                    (define (binary16 n)
                      (local (; N [List-of Binary] -> [List-of Binary]
                              (define (binary/a n accu)
                                (cond [(zero? n) accu]
                                      [else (binary/a (sub1 n) (add1-binary accu))])))
                        (binary/a n '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))))
                    ; C-instruction -> Code
                    (define (translate-c inst)
                      (local ((define d (C-destination inst))
                              (define c (C-compute inst))
                              (define j (C-jump inst)))
                        (append (list 1 1 1)
                                (second (assoc c COMP))
                                (second (assoc d DEST))
                                (second (assoc j JUMP))))))
              (if (A? inst)
                  (binary16 (A-address inst))
                  (translate-c inst)))))
    (map translate l)))

; Instruction -> Code
; translate instruction into code.
;(check-expect (translate (make-A 0)) '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
;(check-expect (translate (make-A 15)) '(0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1))
;(check-expect (translate (make-C "" "D" "JGT")) '(1 1 1 0 0 0 1 1 0 0 0 0 0 0 0 1))
;(check-expect (translate (make-C "D" "M" "")) '(1 1 1 1 1 1 0 0 0 0 0 1 0 0 0 0))
;(define (translate inst)
;  (cond
;    [(A? inst) (binary16 (A-address inst))]
;    [(C? inst) (translate-c inst)]))

; C-instruction -> Code
; translate C-instruction into binary code.
;(check-expect (translate-c (make-C "" "0" "JMP")) '(1 1 1 0 1 0 1 0 1 0 0 0 0 1 1 1))
;(check-expect (translate-c (make-C "M" "M+D" "")) '(1 1 1 1 0 0 0 0 1 0 0 0 1 0 0 0))
;(define (translate-c inst)
;  (local ((define d (C-destination inst))
;          (define c (C-compute inst))
;          (define j (C-jump inst)))
;    (append (list 1 1 1)
;            (second (assoc c COMP))
;            (second (assoc d DEST))
;            (second (assoc j JUMP)))))
; 4Bit -> 4Bit
; perform 4-bit addition.
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
;(define (binary16 n)
;  (local (; N [List-of Binary] -> [List-of Binary]
;          (define (binary/a n accu)
;            (cond [(zero? n) accu]
;                  [else (binary/a (sub1 n) (add1-binary accu))])))
;    (binary/a n '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))))