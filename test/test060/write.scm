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
(set-current-output-port (open-output-file "test/results/test060.answer"))
(define (newline) (display #\newline))
(define (for-each f l)
  (if (null? l) *unspecified*
      (begin (f (car l)) (for-each f (cdr l)))))

;;; Test "write" procedure.

(define (wnl x) (write x) (newline))
(for-each wnl '("hello"
                "Let's go to \"C:\\AGENDA\" for a good semistructured time!"
                ()
                ("hi")
                foo
                (foo)
                (foo bar)
                (foo bar baz)
                (foo bar . baz)
                3
                (1 2 3 5)
                (#t #f)
                (#\x #\y #\newline #\space #\\)))
(newline)
(let ((i 0)) (while (< i 128) (begin (write (integer->char i)) (set! i (+ i 1)) (newline))))

(exit 0)
