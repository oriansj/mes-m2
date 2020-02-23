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
(set-current-output-port (open-output-file "test/results/test036.answer"))
(define (newline) (display #\newline))
(define (cadr x) (car (cdr x)))

;;; Test that quoting works.
;; Covers lists, numbers, booleans, and strings, but not symbols.

(display '"hello, ")
(display (car '("world")))
(newline)

(define add (lambda (x) (+ (car x) (cadr x))))
(display (number->string (add '(3 4)))) (newline)
(display (if (car '(#t #f)) "hi" "bad")) (newline)

(exit 0)
