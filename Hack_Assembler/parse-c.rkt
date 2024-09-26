;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname parse-c) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
(define-struct C [destination compute jump])

; String -> C-instruction
; parse string into C-instruction
(check-expect (parse-c "ADM=0") (make-C "ADM" "0" ""))
(check-expect (parse-c "M=M+D") (make-C "M" "M+D" ""))
(check-expect (parse-c "0;JMP") (make-C "" "0" "JMP"))
(check-expect (parse-c "ADM=M+D;JMP") (make-C "ADM" "M+D" "JMP"))
(check-expect (parse-c "M+D") (make-C "" "M+D" ""))
(define (parse-c str)
  (local (; String -> String
          (define (get-dest str)
            (local ((define exploded (explode str))
                    ; [List-of 1String] String -> String
                    (define (get-dest/a l accu)
                      (cond
                        [(equal? "=" (first l)) accu]
                        [else
                         (get-dest/a (rest l)
                                     (string-append accu (first l)))])))
              (if (member? "=" exploded) (get-dest/a exploded "") "")))
          ; String -> String
          (define (get-comp str)
            (local ((define exploded (explode str))
                    ; [List-of 1String] -> String
                    (define (get-comp/a l accu)
                      (cond
                        [(or (empty? l) (equal? ";" (first l))) accu]
                        [else
                         (if (member? "=" l)
                             (get-comp/a (rest l)accu)
                             (get-comp/a (rest l)
                                         (string-append accu (first l))))])))
              (if (or (member? "=" exploded) (member? ";" exploded))
                  (get-comp/a exploded "")
                  str)))
          ; String -> String
          (define (get-jump l)
            (local ((define exploded (explode l))
                    ; [List-of 1String] -> String
                    (define (get-jump l)
                      (cond
                        [(equal? ";" (first l)) (implode (rest l))]
                        [else
                         (get-jump (rest l))])))
              (if (member? ";" exploded) (get-jump exploded) ""))))
    (make-C (get-dest str) (get-comp str) (get-jump str))))

;; String -> String
;; get the destination field.
;(check-expect (get-dest "ADM=0") "ADM")
;(check-expect (get-dest "M=M+D") "M")
;(check-expect (get-dest "0;JMP") "")
;(check-expect (get-dest "ADM=M+D;JMP") "ADM")
;(check-expect (get-dest "M+D") "")
;(define (get-dest str)
;  (local ((define exploded (explode str))
;          ; [List-of 1String] String -> String
;          (define (get-dest/a l accu)
;            (cond
;              [(equal? "=" (first l)) accu]
;              [else
;               (get-dest/a (rest l)
;                           (string-append accu (first l)))])))
;    (if (member? "=" exploded) (get-dest/a exploded "") "")))
;; String -> String
;; get the compute field.
;(check-expect (get-comp "ADM=0") "0")
;(check-expect (get-comp "M=M+D") "M+D")
;(check-expect (get-comp "0;JMP") "0")
;(check-expect (get-comp "ADM=M+D;JMP") "M+D")
;(check-expect (get-comp "M+D") "M+D")
;(define (get-comp str)
;  (local ((define exploded (explode str))
;          ; [List-of 1String] -> String
;          (define (get-comp/a l accu)
;            (cond
;              [(or (empty? l) (equal? ";" (first l))) accu]
;              [else
;               (if (member? "=" l)
;                   (get-comp/a (rest l)accu)
;                   (get-comp/a (rest l)
;                               (string-append accu (first l))))])))
;    (if (or (member? "=" exploded) (member? ";" exploded))
;        (get-comp/a exploded "")
;        str)))
;; String -> String
;; get the jump field.
;(check-expect (get-jump "ADM=0") "")
;(check-expect (get-jump "M=M+D") "")
;(check-expect (get-jump "0;JMP") "JMP")
;(check-expect (get-jump "ADM=M+D;JMP") "JMP")
;(check-expect (get-jump "M+D") "")
;(define (get-jump l)
;  (local ((define exploded (explode l))
;          ; [List-of 1String] -> String
;          (define (get-jump l)
;            (cond
;              [(equal? ";" (first l)) (implode (rest l))]
;              [else
;               (get-jump (rest l))])))
;    (if (member? ";" exploded) (get-jump exploded) "")))