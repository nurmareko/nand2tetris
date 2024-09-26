;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname CodeWriter) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
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
;====================================================;
;(check-expect
; (codewriter
;  (list (make-push "constant" "5")
;        (make-pop "static" "1")
;        (make-arithlogic "add")
;        (make-arithlogic "not"))
;  "Test")
; (list "// push constant 5\n"
;       (make-A "5")
;       (make-C "D" "A" "null")
;       (make-A "SP")
;       (make-C "A" "M" "null")
;       (make-C "M" "D" "null")
;       (make-A "SP")
;       (make-C "M" "M+1" "null")
;       "// pop static 1\n"
;       (make-A "SP")
;       (make-C "A" "M-1" "null")
;       (make-C "D" "M" "null")
;       (make-A "Test.1")
;       (make-C "M" "D" "null")
;       (make-A "SP")
;       (make-C "M" "M-1" "null")
;       "// add\n"
;       (make-A "SP")
;       (make-C "A" "M-1" "null")
;       (make-C "D" "M" "null")
;       (make-A "SP")
;       (make-C "M" "M-1" "null")
;       (make-A "SP")
;       (make-C "A" "M-1" "null")
;       (make-C "D" "M+D" "null")
;       (make-A "SP")
;       (make-C "M" "M-1" "null")
;       (make-A "SP")
;       (make-C "A" "M" "null")
;       (make-C "M" "D" "null")
;       (make-A "SP")
;       (make-C "M" "M+1" "null")
;       "// not\n"
;       (make-A "SP")
;       (make-C "A" "M-1" "null")
;       (make-C "D" "!M" "null")
;       (make-A "SP")
;       (make-C "M" "M-1" "null")
;       (make-A "SP")
;       (make-C "A" "M" "null")
;       (make-C "M" "D" "null")
;       (make-A "SP")
;       (make-C "M" "M+1" "null")
;       (make-L "END")
;       (make-A "END")
;       (make-C "null" "0" "JMP")))
;
;(check-expect
; (vm-to-asm (make-push "constant" "5") "Test")
; (list "// push constant 5\n"
;       (make-A "5")
;       (make-C "D" "A" "null")
;       (make-A "SP")
;       (make-C "A" "M" "null")
;       (make-C "M" "D" "null")
;       (make-A "SP")
;       (make-C "M" "M+1" "null")))
;(check-expect
; (vm-to-asm (make-pop "static" "1") "Test")
; (list "// pop static 1\n"
;       (make-A "SP")
;       (make-C "A" "M-1" "null")
;       (make-C "D" "M" "null")
;       (make-A "Test.1")
;       (make-C "M" "D" "null")
;       (make-A "SP")
;       (make-C "M" "M-1" "null")))
;(check-expect
; (vm-to-asm (make-arithlogic "add") "Test")
; (list "// add\n"
;       (make-A "SP")
;       (make-C "A" "M-1" "null")
;       (make-C "D" "M" "null")
;       (make-A "SP")
;       (make-C "M" "M-1" "null")
;       (make-A "SP")
;       (make-C "A" "M-1" "null")
;       (make-C "D" "M+D" "null")
;       (make-A "SP")
;       (make-C "M" "M-1" "null")
;       (make-A "SP")
;       (make-C "A" "M" "null")
;       (make-C "M" "D" "null")
;       (make-A "SP")
;       (make-C "M" "M+1" "null")))
;(check-expect
; (vm-to-asm (make-arithlogic "not") "Test")
; (list "// not\n"
;       (make-A "SP")
;       (make-C "A" "M-1" "null")
;       (make-C "D" "!M" "null")
;       (make-A "SP")
;       (make-C "M" "M-1" "null")
;       (make-A "SP")
;       (make-C "A" "M" "null")
;       (make-C "M" "D" "null")
;       (make-A "SP")
;       (make-C "M" "M+1" "null")))
;
;(check-expect
; (write-arithlogic (make-arithlogic "add"))
; (list (make-A "SP")
;       (make-C "A" "M-1" "null")
;       (make-C "D" "M" "null")
;       (make-A "SP")
;       (make-C "M" "M-1" "null")
;       (make-A "SP")
;       (make-C "A" "M-1" "null")
;       (make-C "D" "M+D" "null")
;       (make-A "SP")
;       (make-C "M" "M-1" "null")
;       (make-A "SP")
;       (make-C "A" "M" "null")
;       (make-C "M" "D" "null")
;       (make-A "SP")
;       (make-C "M" "M+1" "null")))
;(check-expect
; (write-arithlogic (make-arithlogic "sub"))
; (list (make-A "SP")
;       (make-C "A" "M-1" "null")
;       (make-C "D" "M" "null")
;       (make-A "SP")
;       (make-C "M" "M-1" "null")
;       (make-A "SP")
;       (make-C "A" "M-1" "null")
;       (make-C "D" "M-D" "null")
;       (make-A "SP")
;       (make-C "M" "M-1" "null")
;       (make-A "SP")
;       (make-C "A" "M" "null")
;       (make-C "M" "D" "null")
;       (make-A "SP")
;       (make-C "M" "M+1" "null")))
;(check-expect
; (write-arithlogic (make-arithlogic "neg"))
; (list (make-A "SP")
;       (make-C "A" "M-1" "null")
;       (make-C "D" "-M" "null")
;       (make-A "SP")
;       (make-C "M" "M-1" "null")
;       (make-A "SP")
;       (make-C "A" "M" "null")
;       (make-C "M" "D" "null")
;       (make-A "SP")
;       (make-C "M" "M+1" "null")))
;(check-expect
; (write-arithlogic (make-arithlogic "eq"))
; (list (make-A "SP")
;       (make-C "A" "M-1" "null")
;       (make-C "D" "M" "null")
;       (make-A "SP")
;       (make-C "M" "M-1" "null")
;       (make-A "SP")
;       (make-C "A" "M-1" "null")
;       (make-C "D" "M-D" "null")
;       (make-A "SP")
;       (make-C "M" "M-1" "null")
;       (make-A "TRUE")
;       (make-C "null" "D" "JEQ")
;       (make-C "D" "0" "null")
;       (make-A "SP")
;       (make-C "A" "M" "null")
;       (make-C "M" "D" "null")
;       (make-A "SP")
;       (make-C "M" "M+1" "null")
;       (make-A "DONE")
;       (make-C "null" "0" "JMP")
;       (make-L "TRUE")
;       (make-C "D" "-1" "null")
;       (make-A "SP")
;       (make-C "A" "M" "null")
;       (make-C "M" "D" "null")
;       (make-A "SP")
;       (make-C "M" "M+1" "null")
;       (make-L "DONE")))
;(check-expect
; (write-arithlogic (make-arithlogic "gt"))
; (list (make-A "SP")
;       (make-C "A" "M-1" "null")
;       (make-C "D" "M" "null")
;       (make-A "SP")
;       (make-C "M" "M-1" "null")
;       (make-A "SP")
;       (make-C "A" "M-1" "null")
;       (make-C "D" "M-D" "null")
;       (make-A "SP")
;       (make-C "M" "M-1" "null")
;       (make-A "TRUE")
;       (make-C "null" "D" "JGT")
;       (make-C "D" "0" "null")
;       (make-A "SP")
;       (make-C "A" "M" "null")
;       (make-C "M" "D" "null")
;       (make-A "SP")
;       (make-C "M" "M+1" "null")
;       (make-A "DONE")
;       (make-C "null" "0" "JMP")
;       (make-L "TRUE")
;       (make-C "D" "-1" "null")
;       (make-A "SP")
;       (make-C "A" "M" "null")
;       (make-C "M" "D" "null")
;       (make-A "SP")
;       (make-C "M" "M+1" "null")
;       (make-L "DONE")))
;(check-expect
; (write-arithlogic (make-arithlogic "lt"))
; (list (make-A "SP")
;       (make-C "A" "M-1" "null")
;       (make-C "D" "M" "null")
;       (make-A "SP")
;       (make-C "M" "M-1" "null")
;       (make-A "SP")
;       (make-C "A" "M-1" "null")
;       (make-C "D" "M-D" "null")
;       (make-A "SP")
;       (make-C "M" "M-1" "null")
;       (make-A "TRUE")
;       (make-C "null" "D" "JLT")
;       (make-C "D" "0" "null")
;       (make-A "SP")
;       (make-C "A" "M" "null")
;       (make-C "M" "D" "null")
;       (make-A "SP")
;       (make-C "M" "M+1" "null")
;       (make-A "DONE")
;       (make-C "null" "0" "JMP")
;       (make-L "TRUE")
;       (make-C "D" "-1" "null")
;       (make-A "SP")
;       (make-C "A" "M" "null")
;       (make-C "M" "D" "null")
;       (make-A "SP")
;       (make-C "M" "M+1" "null")
;       (make-L "DONE")))
;(check-expect
; (write-arithlogic (make-arithlogic "and"))
; (list (make-A "SP")
;       (make-C "A" "M-1" "null")
;       (make-C "D" "M" "null")
;       (make-A "SP")
;       (make-C "M" "M-1" "null")
;       (make-A "SP")
;       (make-C "A" "M-1" "null")
;       (make-C "D" "M&D" "null")
;       (make-A "SP")
;       (make-C "M" "M-1" "null")
;       (make-A "SP")
;       (make-C "A" "M" "null")
;       (make-C "M" "D" "null")
;       (make-A "SP")
;       (make-C "M" "M+1" "null")))
;(check-expect
; (write-arithlogic (make-arithlogic "or"))
; (list (make-A "SP")
;       (make-C "A" "M-1" "null")
;       (make-C "D" "M" "null")
;       (make-A "SP")
;       (make-C "M" "M-1" "null")
;       (make-A "SP")
;       (make-C "A" "M-1" "null")
;       (make-C "D" "M|D" "null")
;       (make-A "SP")
;       (make-C "M" "M-1" "null")
;       (make-A "SP")
;       (make-C "A" "M" "null")
;       (make-C "M" "D" "null")
;       (make-A "SP")
;       (make-C "M" "M+1" "null")))
;(check-expect
; (write-arithlogic (make-arithlogic "not"))
; (list (make-A "SP")
;       (make-C "A" "M-1" "null")
;       (make-C "D" "!M" "null")
;       (make-A "SP")
;       (make-C "M" "M-1" "null")
;       (make-A "SP")
;       (make-C "A" "M" "null")
;       (make-C "M" "D" "null")
;       (make-A "SP")
;       (make-C "M" "M+1" "null")))
;
;(check-expect
; (write-push (make-push "argument" "5") "Test")
; (list (make-A "5")
;       (make-C "D" "A" "null")
;       (make-A "ARG")
;       (make-C "A" "M+D" "null")
;       (make-C "D" "M" "null")
;       (make-A "SP")
;       (make-C "A" "M" "null")
;       (make-C "M" "D" "null")
;       (make-A "SP")
;       (make-C "M" "M+1" "null")))
;(check-expect
; (write-push (make-push "local" "5") "Test")
; (list (make-A "5")
;       (make-C "D" "A" "null")
;       (make-A "LCL")
;       (make-C "A" "M+D" "null")
;       (make-C "D" "M" "null")
;       (make-A "SP")
;       (make-C "A" "M" "null")
;       (make-C "M" "D" "null")
;       (make-A "SP")
;       (make-C "M" "M+1" "null")))
;(check-expect
; (write-push (make-push "static" "5") "Test")
; (list (make-A "Test.5")
;       (make-C "D" "M" "null")
;       (make-A "SP")
;       (make-C "A" "M" "null")
;       (make-C "M" "D" "null")
;       (make-A "SP")
;       (make-C "M" "M+1" "null")))
;(check-expect
; (write-push (make-push "constant" "5") "Test")
; (list (make-A "5")
;       (make-C "D" "A" "null")
;       (make-A "SP")
;       (make-C "A" "M" "null")
;       (make-C "M" "D" "null")
;       (make-A "SP")
;       (make-C "M" "M+1" "null")))
;(check-expect
; (write-push (make-push "this" "5") "Test")
; (list (make-A "5")
;       (make-C "D" "A" "null")
;       (make-A "THIS")
;       (make-C "A" "M+D" "null")
;       (make-C "D" "M" "null")
;       (make-A "SP")
;       (make-C "A" "M" "null")
;       (make-C "M" "D" "null")
;       (make-A "SP")
;       (make-C "M" "M+1" "null")))
;(check-expect
; (write-push (make-push "that" "5") "Test")
; (list (make-A "5")
;       (make-C "D" "A" "null")
;       (make-A "THAT")
;       (make-C "A" "M+D" "null")
;       (make-C "D" "M" "null")
;       (make-A "SP")
;       (make-C "A" "M" "null")
;       (make-C "M" "D" "null")
;       (make-A "SP")
;       (make-C "M" "M+1" "null")))
;(check-expect
; (write-push (make-push "pointer" "0") "Test")
; (list (make-A "THIS")
;       (make-C "D" "M" "null")
;       (make-A "SP")
;       (make-C "A" "M" "null")
;       (make-C "M" "D" "null")
;       (make-A "SP")
;       (make-C "M" "M+1" "null")))
;(check-expect
; (write-push (make-push "pointer" "1") "Test")
; (list (make-A "THAT")
;       (make-C "D" "M" "null")
;       (make-A "SP")
;       (make-C "A" "M" "null")
;       (make-C "M" "D" "null")
;       (make-A "SP")
;       (make-C "M" "M+1" "null")))
;(check-expect
; (write-push (make-push "temp" "0") "Test")
; (list (make-A "5")
;       (make-C "D" "M" "null")
;       (make-A "SP")
;       (make-C "A" "M" "null")
;       (make-C "M" "D" "null")
;       (make-A "SP")
;       (make-C "M" "M+1" "null")))
;(check-expect
; (write-push (make-push "temp" "1") "Test")
; (list (make-A "6")
;       (make-C "D" "M" "null")
;       (make-A "SP")
;       (make-C "A" "M" "null")
;       (make-C "M" "D" "null")
;       (make-A "SP")
;       (make-C "M" "M+1" "null")))
;(check-expect
; (write-push (make-push "temp" "2") "Test")
; (list (make-A "7")
;       (make-C "D" "M" "null")
;       (make-A "SP")
;       (make-C "A" "M" "null")
;       (make-C "M" "D" "null")
;       (make-A "SP")
;       (make-C "M" "M+1" "null")))
;(check-expect
; (write-push (make-push "temp" "3") "Test")
; (list (make-A "8")
;       (make-C "D" "M" "null")
;       (make-A "SP")
;       (make-C "A" "M" "null")
;       (make-C "M" "D" "null")
;       (make-A "SP")
;       (make-C "M" "M+1" "null")))
;(check-expect
; (write-push (make-push "temp" "4") "Test")
; (list (make-A "9")
;       (make-C "D" "M" "null")
;       (make-A "SP")
;       (make-C "A" "M" "null")
;       (make-C "M" "D" "null")
;       (make-A "SP")
;       (make-C "M" "M+1" "null")))
;(check-expect
; (write-push (make-push "temp" "5") "Test")
; (list (make-A "10")
;       (make-C "D" "M" "null")
;       (make-A "SP")
;       (make-C "A" "M" "null")
;       (make-C "M" "D" "null")
;       (make-A "SP")
;       (make-C "M" "M+1" "null")))
;(check-expect
; (write-push (make-push "temp" "6") "Test")
; (list (make-A "11")
;       (make-C "D" "M" "null")
;       (make-A "SP")
;       (make-C "A" "M" "null")
;       (make-C "M" "D" "null")
;       (make-A "SP")
;       (make-C "M" "M+1" "null")))
;(check-expect
; (write-push (make-push "temp" "7") "Test")
; (list (make-A "12")
;       (make-C "D" "M" "null")
;       (make-A "SP")
;       (make-C "A" "M" "null")
;       (make-C "M" "D" "null")
;       (make-A "SP")
;       (make-C "M" "M+1" "null")))
;
;(check-expect
; (write-pop (make-pop "argument" "5") "Test")
; (list (make-A "SP")
;       (make-C "A" "M-1" "null")
;       (make-C "D" "M" "null")
;       (make-A "405")
;       (make-C "M" "D" "null")
;       (make-A "SP")
;       (make-C "M" "M-1" "null")))
;(check-expect
; (write-pop (make-pop "local" "5") "Test")
; (list (make-A "SP")
;       (make-C "A" "M-1" "null")
;       (make-C "D" "M" "null")
;       (make-A "305")
;       (make-C "M" "D" "null")
;       (make-A "SP")
;       (make-C "M" "M-1" "null")))
;(check-expect
; (write-pop (make-pop "static" "5") "Test")
; (list (make-A "SP")
;       (make-C "A" "M-1" "null")
;       (make-C "D" "M" "null")
;       (make-A "Test.5")
;       (make-C "M" "D" "null")
;       (make-A "SP")
;       (make-C "M" "M-1" "null")))
;(check-expect
; (write-pop (make-pop "this" "5") "Test")
; (list (make-A "5")
;       (make-C "D" "A" "null")
;       (make-A "THIS")
;       (make-C "D" "M+D" "null")
;       (make-A "R13")
;       (make-C "M" "D" "null")
;       (make-A "SP")
;       (make-C "A" "M-1" "null")
;       (make-C "D" "M" "null")
;       (make-C "M" "D" "null")
;       (make-A "R13")
;       (make-C "A" "M" "null")
;       (make-C "M" "D" "null")
;       (make-A "SP")
;       (make-C "M" "M-1" "null")))
;(check-expect
; (write-pop (make-pop "that" "5") "Test")
; (list (make-A "5")
;       (make-C "D" "A" "null")
;       (make-A "THAT")
;       (make-C "D" "M+D" "null")
;       (make-A "R13")
;       (make-C "M" "D" "null")
;       (make-A "SP")
;       (make-C "A" "M-1" "null")
;       (make-C "D" "M" "null")
;       (make-C "M" "D" "null")
;       (make-A "R13")
;       (make-C "A" "M" "null")
;       (make-C "M" "D" "null")
;       (make-A "SP")
;       (make-C "M" "M-1" "null")))
;(check-expect
; (write-pop (make-pop "pointer" "1") "Test")
; (list (make-A "SP")
;       (make-C "A" "M-1" "null")
;       (make-C "D" "M" "null")
;       (make-A "4")
;       (make-C "M" "D" "null")
;       (make-A "SP")
;       (make-C "M" "M-1" "null")))
;(check-expect
; (write-pop (make-pop "pointer" "0") "Test")
; (list (make-A "SP")
;       (make-C "A" "M-1" "null")
;       (make-C "D" "M" "null")
;       (make-A "3")
;       (make-C "M" "D" "null")
;       (make-A "SP")
;       (make-C "M" "M-1" "null")))
;(check-expect
; (write-pop (make-pop "temp" "0") "Test")
; (list (make-A "SP")
;       (make-C "A" "M-1" "null")
;       (make-C "D" "M" "null")
;       (make-A "5")
;       (make-C "M" "D" "null")
;       (make-A "SP")
;       (make-C "M" "M-1" "null")))
;(check-expect
; (write-pop (make-pop "temp" "1") "Test")
; (list (make-A "SP")
;       (make-C "A" "M-1" "null")
;       (make-C "D" "M" "null")
;       (make-A "6")
;       (make-C "M" "D" "null")
;       (make-A "SP")
;       (make-C "M" "M-1" "null")))
;(check-expect
; (write-pop (make-pop "temp" "2") "Test")
; (list (make-A "SP")
;       (make-C "A" "M-1" "null")
;       (make-C "D" "M" "null")
;       (make-A "7")
;       (make-C "M" "D" "null")
;       (make-A "SP")
;       (make-C "M" "M-1" "null")))
;(check-expect
; (write-pop (make-pop "temp" "3") "Test")
; (list (make-A "SP")
;       (make-C "A" "M-1" "null")
;       (make-C "D" "M" "null")
;       (make-A "8")
;       (make-C "M" "D" "null")
;       (make-A "SP")
;       (make-C "M" "M-1" "null")))
;(check-expect
; (write-pop (make-pop "temp" "4") "Test")
; (list (make-A "SP")
;       (make-C "A" "M-1" "null")
;       (make-C "D" "M" "null")
;       (make-A "9")
;       (make-C "M" "D" "null")
;       (make-A "SP")
;       (make-C "M" "M-1" "null")))
;(check-expect
; (write-pop (make-pop "temp" "5") "Test")
; (list (make-A "SP")
;       (make-C "A" "M-1" "null")
;       (make-C "D" "M" "null")
;       (make-A "10")
;       (make-C "M" "D" "null")
;       (make-A "SP")
;       (make-C "M" "M-1" "null")))
;(check-expect
; (write-pop (make-pop "temp" "6") "Test")
; (list (make-A "SP")
;       (make-C "A" "M-1" "null")
;       (make-C "D" "M" "null")
;       (make-A "11")
;       (make-C "M" "D" "null")
;       (make-A "SP")
;       (make-C "M" "M-1" "null")))
;(check-expect
; (write-pop (make-pop "temp" "7") "Test")
; (list (make-A "SP")
;       (make-C "A" "M-1" "null")
;       (make-C "D" "M" "null")
;       (make-A "12")
;       (make-C "M" "D" "null")
;       (make-A "SP")
;       (make-C "M" "M-1" "null")))
;
;(check-expect
; (generate-label
;  (list
;   "// eq\n"
;   (make-A "TRUE")
;   (make-A "DONE")
;   (make-L "TRUE")
;   (make-L "DONE")
;
;   "// lt\n"
;   (make-A "TRUE")
;   (make-A "DONE")
;   (make-L "TRUE")
;   (make-L "DONE")
;
;   "// gt\n"
;   (make-A "TRUE")
;   (make-A "DONE")
;   (make-L "TRUE")
;   (make-L "DONE")
;   ))
; (list
;  "// eq\n"
;  (make-A "TRUE0")
;  (make-A "DONE0")
;  (make-L "TRUE0")
;  (make-L "DONE0")
;
;  "// lt\n"
;  (make-A "TRUE1")
;  (make-A "DONE1")
;  (make-L "TRUE1")
;  (make-L "DONE1")
;
;  "// gt\n"
;  (make-A "TRUE2")
;  (make-A "DONE2")
;  (make-L "TRUE2")
;  (make-L "DONE2")
;  ))