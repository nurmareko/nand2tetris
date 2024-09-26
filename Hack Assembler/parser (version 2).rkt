;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |parser (version 2)|) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
(define-struct A [address])
(define-struct C [destination compute jump])

; Instruction is one of:
; - A-instruction
; - C-instruction

; A-instruction is a structure (make-A Number)

; C-instruction is a structure (make-C Dest Comp Jump)

; Dest is a member? of this list:
;(define DEST
;  '("" "M" "D" "DM" "A" "AM" "ADM"))

; Comp is a member? of this list:
;(define COMP
;  '("0"   "1"   "-1"  "D"   "A"   "M"   "!D"  "!A"
;    "!M"  "-D"  "-A"  "-M"  "D+1" "A+1" "M+1" "D- 1"
;    "A-1" "M-1" "D+A" "D+M" "D-A" "D-M" "A-D" "M-D"
;    "D&A" "D&M" "D|A" "D|M"))

; Jump is a member? of this list:
;(define JUMP
;  '("" "JGT" "JEQ" "JGE" "JLE" "JLT" "JNE" "JMP"))
;==================================================================;
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
;==================================================================;
; [List-of [List-of String]] -> [List-of Instruction]
; parse the given lines of word into an instructions
(check-expect (parser GIVEN)
              (list (make-A 0)
                    (make-A 15)
                    (make-C "D" "0" "")
                    (make-C "M" "M+D" "")
                    (make-C "" "0" "JMP")
                    (make-C "" "D" "JGT")
                    (make-C "D" "M" "")))
(define (parser l)
  (local (; [List-of [List-of String]] -> [List-of String]
          ; remove comment and whitespace.
          (define (cleaner l)
            (local (;[List-of String] -> Boolean
                    (define (keep-line? line)
                      (if (or (empty? line) (equal? "//" (first line)))
                          #false
                          #true)))
              (map first (filter keep-line? l))))
          (define (parse str)
            (local ((define (parse-c str)
                      (local (; String -> String
                              (define (get-dest str)
                                (local ((define exploded (explode str))
                                        ; [List-of 1String] String -> String
                                        (define (get-dest/a l accu)
                                          (cond
                                            [(equal? "=" (first l)) accu]
                                            [else (get-dest/a (rest l) (string-append accu (first l)))])))
                                  (if (member? "=" exploded) (get-dest/a exploded "") "")))
                              ; String -> String
                              (define (get-comp str)
                                (local ((define exploded (explode str))
                                        ; [List-of 1String] -> String
                                        (define (get-comp/a l accu)
                                          (cond
                                            [(or (empty? l) (equal? ";" (first l))) accu]
                                            [else (if (member? "=" l)
                                                      (get-comp/a (rest l) accu)
                                                      (get-comp/a (rest l) (string-append accu (first l))))])))
                                  (if (or (member? "=" exploded) (member? ";" exploded)) (get-comp/a exploded "") str)))
                              ; String -> String
                              (define (get-jump l)
                                (local ((define exploded (explode l))
                                        ; [List-of 1String] -> String
                                        (define (get-jump l)
                                          (cond
                                            [(equal? ";" (first l)) (implode (rest l))]
                                            [else (get-jump (rest l))])))
                                  (if (member? ";" exploded) (get-jump exploded) ""))))
                        (make-C (get-dest str) (get-comp str) (get-jump str)))))
              (if (equal? "@" (substring str 0 1))
                  (make-A (string->number (substring str 1)))
                  (parse-c str)))))
    (map parse (cleaner l))))
;==================================================================;
; [List-of [List-of String]] -> [List-of String]
; remove comment and whitespace.
;(check-expect
; (cleaner GIVEN)
; '("@0"
;   "@15"
;   "D=0"
;   "M=M+D"
;   "0;JMP"
;   "D;JGT"
;   "D=M"))
;(define (cleaner l)
;  (local (;[List-of String] -> Boolean
;          (define (keep-line? line)
;            (if (or (empty? line) (equal? "//" (first line)))
;                #false
;                #true)))
;    (map first (filter keep-line? l))))

;[List-of String] -> Boolean
; is the given line a model of Instruction?
;(check-expect (keep-line? '()) #false)
;(check-expect (keep-line? '("//" "comment.")) #false)
;(check-expect (keep-line? '("D;JGT")) #true)
;(check-expect (keep-line? '("D=M" "//" "comment.")) #true)
;(define (keep-line? line)
;  (if (or (empty? line) (equal? "//" (first line))) #false #true))

; String -> Instruction
;; parse string into instruction.
;(check-expect (parse "@0") (make-A 0))
;(check-expect (parse "@15") (make-A 15))
;(check-expect (parse "ADM=0") (make-C "ADM" "0" ""))
;(check-expect (parse "M=M+D") (make-C "M" "M+D" ""))
;(check-expect (parse "0;JMP") (make-C "" "0" "JMP"))
;(define (parse str)
;  (local ((define A-inst? (equal? "@" (substring str 0 1))))
;    (if A-inst?
;        (make-A (string->number (substring str 1)))
;        (parse-c str))))

; String -> C-instruction
; parse string into C-instruction
;(check-expect (parse-c "ADM=0") (make-C "ADM" "0" ""))
;(check-expect (parse-c "M=M+D") (make-C "M" "M+D" ""))
;(check-expect (parse-c "0;JMP") (make-C "" "0" "JMP"))
;(check-expect (parse-c "ADM=M+D;JMP") (make-C "ADM" "M+D" "JMP"))
;(check-expect (parse-c "M+D") (make-C "" "M+D" ""))
;(define (parse-c str)
;  (local (; String -> String
;          (define (get-dest str)
;            (local ((define exploded (explode str))
;                    ; [List-of 1String] String -> String
;                    (define (get-dest/a l accu)
;                      (cond
;                        [(equal? "=" (first l)) accu]
;                        [else (get-dest/a (rest l) (string-append accu (first l)))])))
;              (if (member? "=" exploded) (get-dest/a exploded "") "")))
;          ; String -> String
;          (define (get-comp str)
;            (local ((define exploded (explode str))
;                    ; [List-of 1String] -> String
;                    (define (get-comp/a l accu)
;                      (cond
;                        [(or (empty? l) (equal? ";" (first l))) accu]
;                        [else (if (member? "=" l)
;                                  (get-comp/a (rest l) accu)
;                                  (get-comp/a (rest l) (string-append accu (first l))))])))
;              (if (or (member? "=" exploded) (member? ";" exploded)) (get-comp/a exploded "") str)))
;          ; String -> String
;          (define (get-jump l)
;            (local ((define exploded (explode l))
;                    ; [List-of 1String] -> String
;                    (define (get-jump l)
;                      (cond
;                        [(equal? ";" (first l)) (implode (rest l))]
;                        [else (get-jump (rest l))])))
;              (if (member? ";" exploded) (get-jump exploded) ""))))
;    (make-C (get-dest str) (get-comp str) (get-jump str))))
