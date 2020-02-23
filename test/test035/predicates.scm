;;; GNU Mes --- Maxwell Equations of Software
;;; Copyright (C) 2008  Kragen Javier Sitaker
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
;;; along with GNU Mes.  If not, see <http://www.gnu.org/licenses/>

;; Setup output file
(set-current-output-port (open-output-file "test/results/test035.answer"))
(define (newline) (display #\newline))
(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (integer? i) (number? i))
(define (for-each f l)
  (if (null? l) *unspecified*
      (begin (f (car l)) (for-each f (cdr l)))))

(define predicates (list (cons "pair?" pair?) (cons "null?" null?)
                         (cons "symbol?" symbol?) (cons "integer?" integer?)
                         (cons "boolean?" boolean?) (cons "string?" string?)
                         (cons "procedure?" procedure?) (cons "char?" char?)))
(define values (cons (list "a lambda" (lambda (foo) foo))
                     '(("()" ()) ("(hello)" (hello)) ("hello" hello)
                       ("39" 39) ("#t" #t) ("#f" #f) ("\"hi\"" "hi")
                       ("#\\x" #\x))))
(define testpreds
  (lambda (preds value)
    (if (not (null? preds))
        (begin
          (display (caar preds)) (display " ")
          (display (car value)) (display ": ")
          (display (if ((cdar preds) (cadr value)) "yes" "no")) (newline)
          (testpreds (cdr preds) value))
        '())))
(define testall (lambda (value) (testpreds predicates value)))
(for-each testall values)

(exit 0)
