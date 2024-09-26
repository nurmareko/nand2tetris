;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname VMTranslator) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
; A VMCommand is one of:
; - PushPop
; - ArithLogic

(define-struct push [segment address])
; A Push is a structure:
;   (make-push String String)

(define-struct pop [segment address])
; A Pop is a structure:
;   (make-pop String String)

(define-struct arithlogic [command])
; An ArithLogic is a structure:
;   (make-arithlogic String)

; An AssemblyCode is one of:
; - Address
; - Compute
; - Comment
; - Label

(define-struct A [location])
; An Address is a structure:
;   (make-A String)

(define-struct C [destination compute jump])
; A Compute is a structure:
;   (make-C String String String)

(define-struct L [symbol])
; A Label is a structure:
;   (make-label String)

; A Comment is (list "//" String ...)

;====================================================;
; String -> String
; main function
(define (vmtranslator file)
  (local ((define FILENAME (get-filename file))
          (define content (read-words/line file))
          (define parsed (parser content))
          (define coded (generate-label (codewriter parsed FILENAME))))
    (write-file (string-append FILENAME ".asm") (codes->string coded))))

; String -> String
; get the file name
(define (get-filename s)
  (implode (reverse (rest (rest (rest (reverse (explode s))))))))

; [List-of AssemblyCode] -> String
(define (codes->string l)
  (local (; AssemblyCode -> String
          (define (code->string code)
            (cond
              [(A? code) (string-append "@" (A-location code) "\n")]
              [(and (C? code) (equal? "null" (C-jump code)))
               (string-append (C-destination code) "=" (C-compute code) "\n")]
              [(and (C? code) (equal? "null" (C-destination code)))
               (string-append (C-compute code) ";" (C-jump code) "\n")]
              [(L? code) (string-append "(" (L-symbol code) ")\n")]
              [else code])))
    (foldr string-append "" (map code->string l))))
;====================================================;
; [List-of [List-of String]] -> [List-of VMCommand]
; parse the list of text line into list of VM Command
(define (parser l)
  (local (; Any -> Boolean
          (define (not-empty-or-comment? x)
            (not (or (empty? x) (equal? "//" (first x)))))
          ; [List-of String] -> VMCommand
          (define (ls->vmcommand ls)
            (local ((define COMMAND (first ls)))
              (cond
                [(equal? "push" COMMAND)
                 (make-push (second ls) (third ls))]
                [(equal? "pop" COMMAND)
                 (make-pop (second ls) (third ls))]
                [else (make-arithlogic COMMAND)]))))
    (map ls->vmcommand (filter not-empty-or-comment? l))))
;====================================================;
; [List-of VMCommand] String -> [List-of AssemblyCode]
; parse list of vm command into list of assembly
; code.
(define (codewriter l filename)
  (local ((define END
            (list (make-L "END")
                  (make-A "END")
                  (make-C "null" "0" "JMP"))))
    (append (foldr (lambda (x rest) (append (vm-to-asm x filename) rest)) '() l) END)))

; VMCommand String -> [List-of AssemblyCode]
; parse the given vm command into a sequence of
; assembly code instructions.
(define (vm-to-asm command filename)
  (local (; VMCommand -> String
          (define (write-comment command)
            (cond
              [(push? command)
               (string-append "// push " (push-segment command) " "
                              (push-address command) "\n")]
              [(pop? command)
               (string-append "// pop " (pop-segment command) " "
                              (pop-address command) "\n")]
              [(arithlogic? command)
               (string-append "// " (arithlogic-command command) "\n")])))
    (cond
      [(push? command)
       (cons (write-comment command) (write-push command filename))]
      [(pop? command)
       (cons (write-comment command) (write-pop command filename))]
      [(arithlogic? command)
       (cons (write-comment command) (write-arithlogic command))])))

; Arithlogic -> [list-of AssemblyCode]
; parse the given arithmetic-logical command into
; its coresponding sequence of assembly codes.
(define (write-arithlogic command)
  (local ((define OPERATION (arithlogic-command command))
          ; String -> [List-of AssemblyCode]
          (define (pop-template comp)
            (list (make-A "SP")
                  (make-C "A" "M-1" "null")
                  (make-C "D" comp "null")
                  (make-A "SP")
                  (make-C "M" "M-1" "null")))
          ; String -> [List-of AssemblyCode]
          (define (push-template comp)
            (list (make-C "D" comp "null")
                  (make-A "SP")
                  (make-C "A" "M" "null")
                  (make-C "M" "D" "null")
                  (make-A "SP")
                  (make-C "M" "M+1" "null")))
          ; String -> [List-of AssemblyCode]
          (define (relational-template jump)
              (append
               (list (make-A "TRUE")
                     (make-C "null" "D" jump))
               (push-template "0")
               (list  (make-A "DONE")
                      (make-C "null" "0" "JMP")
                      (make-L "TRUE"))
               (push-template "-1")
               (list (make-L "DONE")))))
    (cond
      [(equal? "add" OPERATION)
       (append (pop-template "M")
               (pop-template "M+D")
               (rest (push-template "null")))]
      [(equal? "sub" OPERATION)
       (append (pop-template "M")
               (pop-template "M-D")
               (rest (push-template "null")))]
      [(equal? "neg" OPERATION)
       (append (pop-template "-M")
               (rest (push-template "null")))]
      [(equal? "eq" OPERATION)
       (append (pop-template "M")
               (pop-template "M-D")
               (relational-template "JEQ"))]
      [(equal? "gt" OPERATION)
       (append (pop-template "M")
               (pop-template "M-D")
               (relational-template "JGT"))]
      [(equal? "lt" OPERATION)
       (append (pop-template "M")
               (pop-template "M-D")
               (relational-template "JLT"))]
      [(equal? "and" OPERATION)
       (append (pop-template "M")
               (pop-template "M&D")
               (rest (push-template "null")))]
      [(equal? "or" OPERATION)
       (append (pop-template "M")
               (pop-template "M|D")
               (rest (push-template "null")))]
      [(equal? "not" OPERATION)
       (append (pop-template "!M")
               (rest (push-template "null")))])))

; PushPop String -> [List-of AssemblyCode]
; parse the given push command into its coresponding
; sequence of assembly codes.
(define (write-push command filename)
  (local (; Push String -> [List-of AssemblyCode]
          (define (get-value command filename)
            (local ((define SEGMENT (push-segment command))
                    (define ADDRESS (push-address command)))
              (cond
                [(member? SEGMENT '("argument" "local" "this" "that"))
                 (list (make-A ADDRESS)
                       (make-C "D" "A" "null")
                       (make-A
                        (cond [(equal? "argument" SEGMENT) "ARG"]
                              [(equal? "local" SEGMENT) "LCL"]
                              [(equal? "this" SEGMENT) "THIS"]
                              [(equal? "that" SEGMENT) "THAT"]))
                       (make-C "A" "M+D" "null")
                       (make-C "D" "M" "null"))]
                [(equal? "pointer" SEGMENT)
                 (list (make-A (if (equal? "0" ADDRESS) "THIS" "THAT"))
                       (make-C "D" "M" "null"))]
                [else
                 (list (make-A
                        (cond [(equal? "static" SEGMENT) (string-append filename "." ADDRESS)]
                              [(equal? "temp" SEGMENT) (number->string (+ 5 (string->number ADDRESS)))]
                              [else ADDRESS]))
                       (make-C "D" (if (equal? "constant" SEGMENT) "A" "M") "null"))]))))
       (append (get-value command  filename)
            (list (make-A "SP")
                  (make-C "A" "M" "null")
                  (make-C "M" "D" "null")
                  (make-A "SP")
                  (make-C "M" "M+1" "null")))))

; PushPop String -> [List-of AssemblyCode]
; parse the given pop command into its coresponding
; sequence of assembly codes.
(define (write-pop command filename)
  (local (; Pop String -> String
          (define (get-location command filename)
            (local (; String String -> Number
                    (define (get-base-address segment)
                      (cond
                        [(equal? "local" segment) 300]
                        [(equal? "argument" segment) 400]
                        [(equal? "temp" segment) 5])))
              (cond
                [(equal? "pointer" (pop-segment command))
                 (if (equal? "0" (pop-address command)) "3" "4")]
                [(equal? "static" (pop-segment command))
                 (string-append filename "." (pop-address command))]
                [else
                 (number->string
                  (+ (string->number (pop-address command))
                     (get-base-address (pop-segment command))))]))))
    (cond
      [(or (equal? "this" (pop-segment command))
           (equal? "that" (pop-segment command)))
       (list (make-A (pop-address command))
             (make-C "D" "A" "null")
             (make-A (if (equal? "this" (pop-segment command)) "THIS" "THAT"))
             (make-C "D" "M+D" "null")
             (make-A "R13")
             (make-C "M" "D" "null")
             (make-A "SP")
             (make-C "A" "M-1" "null")
             (make-C "D" "M" "null")
             (make-C "M" "D" "null")
             (make-A "R13")
             (make-C "A" "M" "null")
             (make-C "M" "D" "null")
             (make-A "SP")
             (make-C "M" "M-1" "null"))]
      [else
       (list (make-A "SP")
             (make-C "A" "M-1" "null")
             (make-C "D" "M" "null")
             (make-A (get-location command filename))
             (make-C "M" "D" "null")
             (make-A "SP")
             (make-C "M" "M-1" "null"))])))

; [List-of VMCommand] -> [List-of VMCommand]
; generate unique label for relational operations.
(define (generate-label l)
  (local (; [List-of VMCommand] Number Number Number Number -> [List-of VMCommand]
          (define (generate-label/a l at ad lt ld)
            (cond
              [(empty? l) '()]
              [(and (A? (first l)) (equal? "TRUE" (A-location (first l))))
               (cons (make-A (string-append "TRUE" (number->string at)))
                     (generate-label/a (rest l) (add1 at) ad lt ld))]
              [(and (A? (first l)) (equal? "DONE" (A-location (first l))))
               (cons (make-A (string-append "DONE" (number->string ad)))
                     (generate-label/a (rest l) at (add1 ad) lt ld))]
              [(and (L? (first l)) (equal? "TRUE" (L-symbol (first l))))
               (cons (make-L (string-append "TRUE" (number->string lt)))
                     (generate-label/a (rest l) at ad (add1 lt) ld))]
              [(and (L? (first l)) (equal? "DONE" (L-symbol (first l))))
               (cons (make-L (string-append "DONE" (number->string ld)))
                     (generate-label/a (rest l) at ad lt (add1 ld)))]
              [else (cons (first l) (generate-label/a (rest l) at ad lt ld))])))
    (generate-label/a l 0 0 0 0)))
