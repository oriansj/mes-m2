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
(set-current-output-port (open-output-file "test/results/test050.answer"))
(define (newline) (display #\newline))

;;; Test the equal? predicate.

(define (try a b) (display (if (equal? a b) " - equal" " - not equal")) (newline))
(try "hi" "bye")
(try "hi" "hi")
(try "hi" (string-append "h" "i"))
(try 'a 'b)
(try 'a 'a)
(try 1 2)
(try 1 1)
(try "a" 1)
(try '() '())
(try '(a b c) (cons 'a '(b c)))
(try '() '(a))
(try '(a) '())
(try '(a) "a")
(try "a" '(a))
(try '(xixaxa xoxaxa) '(xixaxa quuxaxa))
(try '("this" "is" "a" "test") '("this" "is" "a" "mess"))
(try '("this" "is" "a" "test") '("this" "is" "a" "test"))

(exit 0)
