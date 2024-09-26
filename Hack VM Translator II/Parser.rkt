;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Parser) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
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

;====================================================;
; [List-of [List-of String]] -> [List-of VMCommand]
; parse the list of text line into list of VM Command
(check-expect
 (parser
  '(("//" "comment")
    ()
    ("push" "local" "0")
    ("pop" "argument" "3")
    ("add")
    ("eq")))
 (list
  (make-push "local" "0")
  (make-pop "argument" "3")
  (make-arithlogic "add")
  (make-arithlogic "eq")))

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