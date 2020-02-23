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
(set-current-output-port (open-output-file "test/results/test022.answer"))

(define (newline) (display #\newline))

;;; A simple test program for the compiler.
(define msg "this is a message")
((lambda (hi) (begin (display hi) (newline))) "hi there")

(begin (display (if #t "hello" "goodbye"))
       (display ", world")
       (newline)
       (display "indeed"))
(newline)

((lambda (hello goodbye) (begin (display hello) (newline))) "ok" "NOT OK")

(define fibonacci
  (lambda (x) (if (= x 0) (begin (display "+") 1 )
                  (if (= x 1) (begin (display "+") 1)
                      (+ (fibonacci (- x 1))
                         (fibonacci (- x 2)))))))
(fibonacci 7)
(newline)

(display msg)
(newline)
(exit 0)
