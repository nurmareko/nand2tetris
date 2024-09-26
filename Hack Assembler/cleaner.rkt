;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname cleaner) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
(define GIVEN
  '(("//" "this" "is" "a" "comment.")
    ()
    ("@0")
    ("@15")
    ()
    ("D=0")
    ("M=M+D")
    ("0;JMP")
    ("D;JGT")
    ("D=M" "//" "this" "is" "a" "comment.")))
;===================================================================;
; [List-of [List-of String]] -> [List-of Instruction]
; parse the given lines of word into an instructions
(check-expect
 (parser GIVEN)
 (list (make-A 0)
       (make-A 0)
       (make-C "D" "0" "")
       (make-C "M" "M+D" "")
       (make-C "" "0" "JMP")
       (make-C "" "D" "JGT")
       (make-C "D" "M" "")))
(define (parser alist)
  (map parse (cleaner alist)))
;===================================================================;
; [List-of [List-of String]] -> [List-of String]
; remove comment and whitespace.
(check-expect
 (cleaner GIVEN)
 '("@0"
   "@15"
   "D=0"
   "M=M+D"
   "0;JMP"
   "D;JGT"
   "D=M"))
(define (cleaner alist)
  (local (;[List-of String] -> Boolean
          (define (keep-line? line)
            (if (or (empty? line) (equal? "//" (first line)))
                #false
                #true)))
    (map first (filter keep-line? alist))))

;[List-of String] -> Boolean
; is the given line a model of Instruction?
;(check-expect (keep-line? '()) #false)
;(check-expect (keep-line? '("//" "comment.")) #false)
;(check-expect (keep-line? '("D;JGT")) #true)
;(check-expect (keep-line? '("D=M" "//" "comment.")) #true)
;(define (keep-line? line)
;  (if (or (empty? line) (equal? "//" (first line))) #false #true))

; String -> Instruction
