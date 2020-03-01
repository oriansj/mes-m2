;;; -*-scheme-*-

;;; GNU Mes --- Maxwell Equations of Software
;;; Copyright ?? 2016,2017,2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
;;; Copyright ?? 2019 Jeremiah Orians
;;;
;;; This file is part of GNU Mes.
;;;
;;; GNU Mes is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Mes is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Mes.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; A work in progress

;;; Code:
(define mes %version)
(define EXIT_FAILURE 1)
(define EXIT_SUCCESS 0)

(define (cond-expand-expander clauses)
  (if (defined? (car (car clauses)))
      (cdr (car clauses))
      (cond-expand-expander (cdr clauses))))

(define-macro (cond-expand . clauses)
  (cons 'begin (cond-expand-expander clauses)))

(define (newline . rest)
  (display #\newline))

;; String related
(define (string . rest)
  (list->string rest))

;; NUMBERS
(define (zero? x) (= x 0))
(define (even? x) (= 0 (modulo x 2)))
(define (odd? x) (not (even? x)))
(define (positive? x) (> x 0))
(define (negative? x) (< x 0))
(define (abs x) (if (negative? x) (- 0 x) x))
(define (min x y) (if (< x y) x y))
(define (max x y) (if (> x y) x y))
(define (1+ x) (+ x 1))
(define (1- x) (- x 1))

;; Convience c**rs
;; Level 2
(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))
;; Level 3
(define (caaar x) (car (car (car x))))
(define (caadr x) (car (car (cdr x))))
(define (cadar x) (car (cdr (car x))))
(define (caddr x) (car (cdr (cdr x))))
(define (cdaar x) (cdr (car (car x))))
(define (cdadr x) (cdr (car (cdr x))))
(define (cddar x) (cdr (cdr (car x))))
(define (cdddr x) (cdr (cdr (cdr x))))
;; Level 4
(define (caaaar x) (car (car (car (car x)))))
(define (caaadr x) (car (car (car (cdr x)))))
(define (caadar x) (car (car (cdr (car x)))))
(define (caaddr x) (car (car (cdr (cdr x)))))
(define (cadaar x) (car (cdr (car (car x)))))
(define (cadadr x) (car (cdr (car (cdr x)))))
(define (caddar x) (car (cdr (cdr (car x)))))
(define (cadddr x) (car (cdr (cdr (cdr x)))))
(define (cdaaar x) (cdr (car (car (car x)))))
(define (cdaadr x) (cdr (car (car (cdr x)))))
(define (cdadar x) (cdr (car (cdr (car x)))))
(define (cdaddr x) (cdr (car (cdr (cdr x)))))
(define (cddaar x) (cdr (cdr (car (car x)))))
(define (cddadr x) (cdr (cdr (car (cdr x)))))
(define (cdddar x) (cdr (cdr (cdr (car x)))))
(define (cddddr x) (cdr (cdr (cdr (cdr x)))))

;; Add baseline support for record types
(define (record-predicate type)
  (lambda (record)
    (core:record-predicate type record)))

(define (record-accessor type field)
  (lambda (record)
    (core:record-accessor type field record)))

(define (record-modifier type field)
  (lambda (record value)
    (core:record-modifier type field record value)))

(define (record-constructor type fields)
  (lambda (. values)
    (core:record-constructor type fields values)))


;; Common boot primitives
(define (map f lst)
  (if (null? lst) '()
      (cons (f (car lst)) (map f (cdr lst)))))

(define (for-each f l)
  (if (null? l) *unspecified*
      (begin (f (car l)) (for-each f (cdr l)))))

(define (cons* . rest)
  (if (null? (cdr rest)) (car rest)
      (cons (car rest) (apply cons* (cdr rest)))))

;; Some list primitives
(define (memq i l) (cond ((null? l) #f) ((eq? i (car l)) l) (#t (memq i (cdr l)))))
(define (memv i l) (cond ((null? l) #f) ((eqv? i (car l)) l) (#t (memv i (cdr l)))))
(define (member i l) (cond ((null? l) #f) ((equal? i (car l)) l) (#t (member i (cdr l)))))
(define (assq i l) (cond ((null? l) #f) ((eq? i (caar l)) (car l)) (else (assq i (cdr l)))))
(define (assv i l) (cond ((null? l) #f) ((eqv? i (caar l)) (car l)) (else (assv i (cdr l)))))
(define (assoc i l) (cond ((null? l) #f) ((equal? i (caar l)) (car l)) (else (assoc i (cdr l)))))

;; Provide guile primitives
(define (keyword-like-symbol->keyword sym)
  (if (symbol? sym) (string->keyword (list->string (cons* #\# #\: (cdr (string->list (symbol->string sym))))))
      (begin (display "keyword-like-symbol->keyword did not recieve a symbol") (exit 1))))

(define (open-file name mode)
  (cond
   ((or (string=? "w" mode) (string=? "wb" mode)) (open-output-file name))
   ((or (string=? "r" mode) (string=? "rb" mode)) (open-input-file name))
   (#t (begin (display "open-file: unknown mode ") (display mode) (display " recieved\nHalting to prevent harm\n") (exit 1)))))

(define (integer? i) (number? i))
(define (error reason . args)
  (display "ERROR: ")
  (display reason)
  (for-each (lambda (arg) (display " ") (write arg)) args)
  (newline)
  (exit 1))

;; Implement the standard prompt
(define __args (cdr (command-line)))

(define --help "Usage: mes [OPTION]... [FILE]...
Evaluate code with Mes, interactively or from a script.

  [-s] FILE           load source code from FILE, and exit
  --                  stop scanning arguments; run interactively

The above switches stop argument processing, and pass all
remaining arguments as the value of (command-line).

  -e,--main=MAIN      after reading script, apply MAIN to command-line arguments
  -h, --help          display this help and exit
  -L,--load-path=DIR  add DIR to the front of the module load path
  -v, --version       display version information and exit

Ignored for Guile compatibility:
  --auto-compile
  --fresh-auto-compile
  --no-auto-compile
  -C,--compiled-path=DIR

Report bugs to: bug-mes@gnu.org
GNU Mes home page: <http://gnu.org/software/mes/>
General help using GNU software: <http://gnu.org/gethelp/>\n")

(define %main "cons*")

(while (not (null? __args))
       (begin
          (cond
           ((or (string=? "--help" (car __args)) (string=? "-h" (car __args))) (begin (display --help) (exit 0)))
           ((string=? "-s" (car __args)) (begin (primitive-load (cadr __args)) (set! __args (cdr __args))((primitive-eval (string->symbol %main)) __args) (exit 0)))
           ((string=? "-e" (car __args)) (begin (set! %main (cadr __args)) (set! __args (cdr __args))))
           ((string=? "--" (car __args)) (set! __args (cons '() '())))
           (#t (begin (display "error: unrecognized switch ") (display (car __args)) (newline) (display --help) (exit 1))))
          (set! __args (cdr __args))))

;; Give a lovely experience
(display "REPL: ")
(primitive-load "/dev/stdin")
(display "have a nice day!\n")
(exit 0)
