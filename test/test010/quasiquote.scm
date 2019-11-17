;;; GNU Mes --- Maxwell Equations of Software
;;; Copyright © 2016,2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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
(set-current-output-port (open-output-file "test/results/test010.answer"))

;; Demonstrate using 'S-expression
(display "making a quasiquote with ` character and unquote with ,\n")
(write `(foo ,(+ 1 3) you))
(display "\nmake a quasiquote with ` and unquote with , everything\n")
(write `(,(+ 1 2 3) ,(+ 4 5 6) ,(+ 7 8 9)))
(display "\nmake a quasiquote with ` and unquote with unquote\n")
(write `(foo (unquote (+ 1 3)) you))
(display "\nmake a quasiquote with quasiquote and unquote with ,\n")
(write (quasiquote (foo ,(+ 1 3) you)))
(display "\nmake a quasiquote with quasiquote and unquote with unquote\n")
(write (quasiquote (foo (unquote (+ 1 3)) you)))
(exit 0)
