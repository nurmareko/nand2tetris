;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Assembler) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
(define-struct A [address])
(define-struct C [destination compute jump])
(define-struct L [symbol])

; Instruction is one of:
; - A-instruction
; - C-instruction
; - L-instruction

; A-instruction is one of structure:
; - (make-A N)
; - (make-A String)

; C-instruction is a structure (make-C String String String)

; L-instruction is a structure (make-L String)

; Table is [List-of (list String N)]

; code is [List-of String]
; constraint:
;   only compose of "0" and "1"

; Binary16 is (list Binary Binary ... Binary)
; of which its (length l) is equal 16

; Binary is one of
; - 0
; - 1
;===================================================================;
(define ZERO16 '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
(define ONE16  '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1))
(define DEFINED
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
(define (assembler file)
  (local (; Initialization
          (define content (read-words/line file))
          (define parsed (parser content))
          ; First pass
          (define first (first-pass parsed))
          ; Second pass
          (define instructions
            (filter (lambda (inst) (not (L? inst))) parsed))
          (define defined-table (second-pass instructions first))
          (define coded (coder instructions defined-table))
          ; set for output
          (define out-name
            (foldr (lambda (x rest) (if (equal? "." x) ".hack" (string-append x rest))) "" (explode file)))
          (define out-content
            (foldr (lambda (x rest) (string-append x "\n" rest)) "" coded)))
    (write-file out-name out-content)))

; [List-of [List-of String]] -> [List-of Instruction]
; parse the given lines of word into an instructions
;(check-expect (parser GIVEN)
;              (list (make-A "R0")
;                    (make-C "D" "M" "")
;                    (make-A "R1")
;                    (make-C "D" "D-M" "")
;                    (make-A "OUTPUT_FIRST")
;                    (make-C "" "D" "JGT")
;                    (make-A "R1")
;                    (make-C "D" "M" "")
;                    (make-A "OUTPUT_D")
;                    (make-C "" "0" "JMP")
;                    (make-L "OUTPUT_FIRST")
;                    (make-A "R0")
;                    (make-C "D" "M" "")
;                    (make-L "OUTPUT_D")
;                    (make-A "R2")
;                    (make-C "M" "D" "")
;                    (make-L "INFINITE_LOOP")
;                    (make-A "INFINITE_LOOP")
;                    (make-C "" "0" "JMP")))
(define (parser l)
  (local (; String -> Instruction
          (define (parse str)
            (local (; String -> A-Instruction
                    (define (build-a str)
                      (local ((define as-string (substring str 1))
                              (define as-number (string->number as-string)))
                        (if (boolean? as-number) (make-A as-string) (make-A as-number))))
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

; [List-of String] -> String
; get the destination field.
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
(define (get-jump l)
  (local (; [List-of 1String] -> String
          (define (get-jump l)
            (cond
              [(equal? ";" (first l)) (implode (rest l))]
              [else (get-jump (rest l))])))
    (if (member? ";" l) (get-jump l) "")))
;==================================================================;
; [List-of Instruction] -> Table
; collect all L-instruction into table.
;(check-expect
; (first-pass (parser GIVEN))
; '(("INFINITE_LOOP"   14)
;   ("OUTPUT_D"        12)
;   ("OUTPUT_FIRST"    10)
;   ("R0"               0)
;   ("R1"               1)
;   ("R2"               2)
;   ("R3"               3)
;   ("R4"               4)
;   ("R5"               5)
;   ("R6"               6)
;   ("R7"               7)
;   ("R8"               8)
;   ("R9"               9)
;   ("R10"             10)
;   ("R11"             11)
;   ("R12"             12)
;   ("R13"             13)
;   ("R14"             14)
;   ("R15"             15)
;   ("SP"               0)
;   ("LCL"              1)
;   ("ARG"              2)
;   ("THIS"             3)
;   ("THAT"             4)
;   ("SCREEN"       16384)
;   ("KBD"          24576)))
(define (first-pass l)
  (local (; [List-of Instruction] N Table -> Table
          (define (build-table/a l counter accu)
            (cond
              [(empty? l) accu]
              [else
               (if (L? (first l))
                   (build-table/a (rest l) counter
                                  (cons (list (L-symbol (first l)) counter) accu))
                   (build-table/a (rest l) (add1 counter) accu))])))
    (build-table/a l 0 DEFINED)))

; [List-of Instruction] Table -> Table
; collect all symbolic A-instruction into table.
;(check-expect
; (second-pass
;  (list (make-A "R0")
;        (make-A 16)
;        (make-A "first")
;        (make-A "second")
;        (make-A "first")
;        (make-A "first"))
;  (filter (lambda (inst) (not (L? inst))) (first-pass (parser GIVEN))))
; '(("second"          17)
;   ("first"           16)
;   ("INFINITE_LOOP"   14)
;   ("OUTPUT_D"        12)
;   ("OUTPUT_FIRST"    10)
;   ("R0"               0)
;   ("R1"               1)
;   ("R2"               2)
;   ("R3"               3)
;   ("R4"               4)
;   ("R5"               5)
;   ("R6"               6)
;   ("R7"               7)
;   ("R8"               8)
;   ("R9"               9)
;   ("R10"             10)
;   ("R11"             11)
;   ("R12"             12)
;   ("R13"             13)
;   ("R14"             14)
;   ("R15"             15)
;   ("SP"               0)
;   ("LCL"              1)
;   ("ARG"              2)
;   ("THIS"             3)
;   ("THAT"             4)
;   ("SCREEN"       16384)
;   ("KBD"          24576)))
(define (second-pass l table)
  (local ((define filtered (filter A? l))
          ; A-instructon Table -> Boolean
          (define (a-defined? inst table)
            (local ((define defined (map first table)))
              (member? (A-address inst) defined)))
          ; A-instruction Table N -> Table
          (define (build-table/a l table count)
            (cond
              [(empty? l) table]
              [else
               (local ((define current (first l))
                       (define remain (rest l)))
                 (if (string? (A-address current))
                     (if (a-defined? current table)
                         (build-table/a remain table count)
                         (build-table/a remain
                                        (cons (list (A-address current) count) table)
                                        (add1 count)))
                     (build-table/a remain table count)))])))
    (build-table/a filtered table 16)))

; A-instructon Table -> Boolean
;(check-expect (a-defined? (make-A "R10") DEFINED) #true)
;(check-expect (a-defined? (make-A "first") DEFINED) #false)
;(define (a-defined? inst table)
;  (local ((define defined (map first table)))
;    (member? (A-address inst) defined)))
;==================================================================;
; [List-of Instruction] Table -> [List-of Code]
; translate instruction into codes.

;(check-expect
; (coder
;  (list
;   (make-A "R0")
;   (make-C "D" "M" "")
;   (make-A "R1")
;   (make-C "D" "D-M" "")
;   (make-A "OUTPUT_FIRST")
;   (make-C "" "D" "JGT")
;   (make-A "R1")
;   (make-C "D" "M" "")
;   (make-A "OUTPUT_D")
;   (make-C "" "0" "JMP")
;   (make-A "R0")
;   (make-C "D" "M" "")
;   (make-A "R2")
;   (make-C "M" "D" "")
;   (make-A "INFINITE_LOOP")
;   (make-C "" "0" "JMP"))
;  '(("INFINITE_LOOP" 14)
;    ("OUTPUT_D" 12)
;    ("OUTPUT_FIRST" 10)
;    ("R0" 0)
;    ("R1" 1)
;    ("R2" 2)
;    ("R3" 3)
;    ("R4" 4)
;    ("R5" 5)
;    ("R6" 6)
;    ("R7" 7)
;    ("R8" 8)
;    ("R9" 9)
;    ("R10" 10)
;    ("R11" 11)
;    ("R12" 12)
;    ("R13" 13)
;    ("R14" 14)
;    ("R15" 15)
;    ("SP" 0)
;    ("LCL" 1)
;    ("ARG" 2)
;    ("THIS" 3)
;    ("THAT" 4)
;    ("SCREEN" 16384)
;    ("KBD" 24576)))
; '("0000000000000000"
;   "1111110000010000"
;   "0000000000000001"
;   "1111010011010000"
;   "0000000000001010"
;   "1110001100000001"
;   "0000000000000001"
;   "1111110000010000"
;   "0000000000001100"
;   "1110101010000111"
;   "0000000000000000"
;   "1111110000010000"
;   "0000000000000010"
;   "1110001100001000"
;   "0000000000001110"
;   "1110101010000111"))
 
(define (coder l table)
  (local (; A-instruction -> Code
          (define (translate-a inst)
            (local ((define addr (A-address inst)))
              (if (number? addr)
                  (implode (map number->string (binary16 addr)))
                  (implode (map number->string (binary16 (second (assoc addr table))))))))
          ; C-instruction -> Code
          (define (translate-c inst)
            (string-append "111"
                           (second (assoc (C-compute inst) COMP))
                           (second (assoc (C-destination inst) DEST))
                           (second (assoc (C-jump inst) JUMP))))
          ; Instruction -> Code
          (define (translate inst)
            (if (A? inst) (translate-a inst) (translate-c inst))))
    (map translate l)))

; N -> Binary16
; convert n into its 16-bit binary representation.
;(check-expect (binary16 0) '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
;(check-expect (binary16 2) '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0))
;(check-expect (binary16 65535) '(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1))
(define (binary16 n)
  (local (; N Binary16 -> Binary16
          (define (binary16/a counter accu)
            (cond
              [(zero? counter) accu]
              [else
               (binary16/a (sub1 counter) (add1-binary16 accu))])))
    (binary16/a n ZERO16)))

; Binary16 -> Binary16
; increment by one.
;(check-expect (add1-binary16 '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
;              '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1))
;(check-expect (add1-binary16 '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1))
;              '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0))
;(check-expect (add1-binary16 '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0))
;              '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1))
;(check-expect (add1-binary16 '(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0))
;              '(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1))
(define (add1-binary16 n)
  (local ((define a (reverse n))
          (define b (reverse ONE16))
          ; Binary16 N [List-of Binary] -> Binary16
          (define (add1/a n one remainder result)
            (cond
              [(empty? n) result]
              [else
               (local ((define add (+ (first n) (first one) remainder)))
                 (if (<= add 1)
                     (add1/a (rest n) (rest one) 0 (cons add result))
                     (add1/a (rest n) (rest one) 1 (cons 0 result))))])))
    (add1/a a b 0 '())))
