;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |parser full|) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
(define-struct A [address])
(define-struct C [destination compute jump])
(define-struct L [symbol])

; Instruction is one of:
; - A-instruction
; - C-instruction
; - L-instruction

; Label is (list String N)

; A-instruction is one of structure:
; - (make-A N)
; - (make-A String)

; C-instruction is a structure (make-C String String String)

; L-instruction is a structure (make-L String)

;===================================================================;
(define predefined
  '(("R0"         0)
    ("R1"         1)
    ("R2"         2)
    ("R3"         3)
    ("R4"         4)
    ("R5"         5)
    ("R6"         6)
    ("R7"         7)
    ("R8"         8)
    ("R9"         9)
    ("R10"       10)
    ("R11"       11)
    ("R12"       12)
    ("R13"       13)
    ("R14"       14)
    ("R15"       15)
    ("SP"         0)
    ("LCL"        1)
    ("ARG"        2)
    ("THIS"       3)
    ("THAT"       4)
    ("SCREEN" 16384)
    ("KBD"    24576)))
(define DEST
  '((""    "000")
    ("M"   "001")
    ("D"   "010")
    ("DM"  "011")
    ("MD"  "011")
    ("A"   "100")
    ("AM"  "101")
    ("MA"  "101")
    ("AD"  "110")
    ("DA"  "110")
    ("ADM" "111")
    ("AMD" "111")
    ("DAM" "111")
    ("MAD" "111")
    ("DMA" "111")
    ("MDA" "111")))
(define COMP
  '(("0"   "0101010")
    ("1"   "0111111")
    ("-1"  "0111010")
    ("D"   "0001100")
    ("A"   "0110000")
    ("M"   "1110000")
    ("!D"  "0001101")
    ("!A"  "0110001")
    ("!M"  "1110001")
    ("-D"  "0001111")
    ("-A"  "0110011")
    ("-M"  "1110011")
    ("D+1" "0011111")
    ("1+D" "0011111")
    ("A+1" "0110111")
    ("1+A" "0110111")
    ("M+1" "1110111")
    ("1+M" "1110111")
    ("D-1" "0001110")
    ("1-D" "0001110")
    ("A-1" "0110010")
    ("1-A" "0110010")
    ("M-1" "1110010")
    ("1-M" "1110010")
    ("D+A" "0000010")
    ("A+D" "0000010")
    ("D+M" "1000010")
    ("M+D" "1000010")
    ("D-A" "0010011")
    ("D-M" "1010011")
    ("A-D" "0000111")
    ("M-D" "1000111")
    ("D&A" "0000000")
    ("A&D" "0000000")
    ("D&M" "1000000")
    ("M&D" "1000000")
    ("D|A" "0010101")
    ("A|D" "0010101")
    ("D|M" "1010101")
    ("M|D" "1010101")))
(define JUMP
  '((""    "000")
    ("JGT" "001")
    ("JEQ" "010")
    ("JGE" "011")
    ("JLT" "100")
    ("JNE" "101")
    ("JLE" "110")
    ("JMP" "111")))
(define GIVEN
  '(("//" "This" "file" "is" "part" "of" "www.nand2tetris.org")
    ("//" "and" "the" "book" "\"The" "Elements" "of" "Computing" "Systems\"")
    ("//" "by" "Nisan" "and" "Schocken," "MIT" "Press.")
    ("//" "File" "name:" "projects/06/max/Max.asm")
    ()
    ("//" "Computes" "R2" "=" "max(R0," "R1)" "(R0,R1,R2" "refer" "to" "RAM[0],RAM[1],RAM[2])")
    ()
    ("@R0")
    ("D=M" "//" "D" "=" "first" "number")
    ("@R1")
    ("D=D-M" "//" "D" "=" "first" "number" "-" "second" "number")
    ("@OUTPUT_FIRST")
    ("D;JGT" "//" "if" "D>0" "(first" "is" "greater)" "goto" "output_first")
    ("@R1")
    ("D=M" "//" "D" "=" "second" "number")
    ("@OUTPUT_D")
    ("0;JMP" "//" "goto" "output_d")
    ("(OUTPUT_FIRST)")
    ("@R0")
    ("D=M" "//" "D" "=" "first" "number")
    ("(OUTPUT_D)")
    ("@R2")
    ("M=D" "//" "M[2]" "=" "D" "(greatest" "number)")
    ("(INFINITE_LOOP)")
    ("@INFINITE_LOOP")
    ("0;JMP" "//" "infinite" "loop")))
;===================================================================;
; String -> String
; translate file writen in hack assembly language and create a file
; writen in hack machine language.
;(define (assember file)
;  (local (; Initialization
;          (define content (read-words/line file))
;          (define parsed (parser content))
;          (define out-name ...)
;          ; First pass
;          (define first-pass ...)
;          ; Second pass
;          (define second-pass ...)
;          ...
;          (define out-content ...))
;    (write-file out-name out-content)))

; [List-of [List-of String]] -> [List-of Instruction]
; parse the given lines of word into an instructions

(check-expect (parser GIVEN)
              (list (make-A "R0")
                    (make-C "D" "M" "")
                    (make-A "R1")
                    (make-C "D" "D-M" "")
                    (make-A "OUTPUT_FIRST")
                    (make-C "" "D" "JGT")
                    (make-A "R1")
                    (make-C "D" "M" "")
                    (make-A "OUTPUT_D")
                    (make-C "" "0" "JMP")
                    (make-L "OUTPUT_FIRST")
                    (make-A "R0")
                    (make-C "D" "M" "")
                    (make-L "OUTPUT_D")
                    (make-A "R2")
                    (make-C "M" "D" "")
                    (make-L "INFINITE_LOOP")
                    (make-A "INFINITE_LOOP")
                    (make-C "" "0" "JMP")))

(define (parser l)
  (local (; String -> Instruction
          (define (parse str)
            (local (; String -> A-Instruction
                    (define (build-a str)
                      (make-A (substring str 1)))
                    ; String -> L-Instruction
                    (define (build-l str)
                      (local ((define length (string-length str)))
                        (make-L (substring (substring str 0 (- length 1)) 1))))
                    ; String -> C-Instruction
                    (define (build-c str)
                      (local ((define exploded (explode str))
                              (define dest (get-dest exploded))
                              (define comp (get-comp exploded))
                              (define jump (get-jump exploded)))
                        (make-C dest comp jump))))
              (cond
                [(equal? "@" (substring str 0 1)) (build-a str)]
                [(equal? "(" (substring str 0 1)) (build-l str)]
                [else (build-c str)]))))
    (map parse (map first (filter (lambda (line) (not (or (empty? line) (equal? "//" (first line))))) l)))))

; [List-of [List-of String]] -> [List-of String]
; remove comment and whitespace.
;(check-expect (cleaner GIVEN)
;              '("@R0"
;                "D=M"
;                "@R1"
;                "D=D-M"
;                "@OUTPUT_FIRST"
;                "D;JGT"
;                "@R1"
;                "D=M"
;                "@OUTPUT_D"
;                "0;JMP"
;                "(OUTPUT_FIRST)"
;                "@R0"
;                "D=M"
;                "(OUTPUT_D)"
;                "@R2"
;                "M=D"
;                "(INFINITE_LOOP)"
;                "@INFINITE_LOOP"
;                "0;JMP"))
;(define (cleaner l)
;  (map first (filter (lambda (line) (not (or (empty? line) (equal? "//" (first line))))) l)))

; String -> Instruction
; parse string into instruction.
;(check-expect (parse "@INFINITE_LOOP") (make-A "INFINITE_LOOP"))
;(check-expect (parse "@0") (make-A "0"))
;(check-expect (parse "D=D-M") (make-C "D" "D-M" ""))
;(check-expect (parse "D;JGT") (make-C "" "D" "JGT"))
;(check-expect (parse "(OUTPUT_D)") (make-L "OUTPUT_D"))
;(check-expect (parse "(INFINITE_LOOP)") (make-L "INFINITE_LOOP"))
;(define (parse str)
;  (local (; String -> A-Instruction
;          (define (build-a str)
;            (make-A (substring str 1)))
;          ; String -> L-Instruction
;          (define (build-l str)
;            (local ((define length (string-length str)))
;              (make-L (substring (substring str 0 (- length 1)) 1))))
;          ; String -> C-Instruction
;          (define (build-c str)
;            (local ((define exploded (explode str))
;                    (define dest (get-dest exploded))
;                    (define comp (get-comp exploded))
;                    (define jump (get-jump exploded)))
;              (make-C dest comp jump))))
;    (cond
;      [(equal? "@" (substring str 0 1)) (build-a str)]
;      [(equal? "(" (substring str 0 1)) (build-l str)]
;      [else (build-c str)])))

; String -> A-Instruction
; parse string into an A-instruction.
;(check-expect (build-a "@INFINITE_LOOP") (make-A "INFINITE_LOOP"))
;(check-expect (build-a "@0") (make-A "0"))
;(define (build-a str)
;  (make-A (substring str 1)))

; String -> L-Instruction
; parse string into an L-instruction.
;(check-expect (build-l "(OUTPUT_D)") (make-L "OUTPUT_D"))
;(check-expect (build-l "(INFINITE_LOOP)") (make-L "INFINITE_LOOP"))
;(define (build-l str)
;  (local ((define length (string-length str)))
;    (make-L (substring (substring str 0 (- length 1)) 1))))

; String -> C-Instruction
; parse string into an C-instruction.
;(check-expect (build-c "ADM=0") (make-C "ADM" "0" ""))
;(check-expect (build-c "M=M+D") (make-C "M" "M+D" ""))
;(check-expect (build-c "0;JMP") (make-C "" "0" "JMP"))
;(check-expect (build-c "ADM=M+D;JMP") (make-C "ADM" "M+D" "JMP"))
;(check-expect (build-c "M+D") (make-C "" "M+D" ""))
;(define (build-c str)
;  (local ((define exploded (explode str))
;          (define dest (get-dest exploded))
;          (define comp (get-comp exploded))
;          (define jump (get-jump exploded)))
;    (make-C dest comp jump)))

; [List-of String] -> String
; get the destination field.
;(check-expect (get-dest (explode "ADM=0")) "ADM")
;(check-expect (get-dest (explode "M=M+D")) "M")
;(check-expect (get-dest (explode "0;JMP")) "")
;(check-expect (get-dest (explode "ADM=M+D;JMP")) "ADM")
;(check-expect (get-dest (explode "M+D")) "")
(define (get-dest l)
  (local (; [List-of 1String] String -> String
          (define (get-dest/a l accu)
            (cond
              [(equal? "=" (first l)) accu]
              [else (get-dest/a (rest l)
                                (string-append accu (first l)))])))
    (if (member? "=" l) (get-dest/a l "") "")))

; [List-of String] -> String
; get the computation field.
;(check-expect (get-comp (explode "ADM=0")) "0")
;(check-expect (get-comp (explode "M=M+D")) "M+D")
;(check-expect (get-comp (explode "0;JMP")) "0")
;(check-expect (get-comp (explode "ADM=M+D;JMP")) "M+D")
;(check-expect (get-comp (explode "M+D")) "M+D")
(define (get-comp l)
  (local (; [List-of 1String] -> String
          (define (get-comp/a l accu)
            (cond
              [(or (empty? l) (equal? ";" (first l))) accu]
              [else (if (member? "=" l)
                        (get-comp/a (rest l) accu)
                        (get-comp/a (rest l) (string-append accu (first l))))])))
    (if (or (member? "=" l) (member? ";" l)) (get-comp/a l "") (implode l))))

; [List-of String] -> String
; get the jump field.
;(check-expect (get-jump (explode "ADM=0")) "")
;(check-expect (get-jump (explode "M=M+D")) "")
;(check-expect (get-jump (explode "0;JMP")) "JMP")
;(check-expect (get-jump (explode "ADM=M+D;JMP")) "JMP")
;(check-expect (get-jump (explode "M+D")) "")
(define (get-jump l)
  (local (; [List-of 1String] -> String
          (define (get-jump l)
            (cond
              [(equal? ";" (first l)) (implode (rest l))]
              [else (get-jump (rest l))])))
    (if (member? ";" l) (get-jump l) "")))
;==================================================================;
; Content Table -> Table
; 
(define (first-pass content table) ...)
