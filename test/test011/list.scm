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
(set-current-output-port (open-output-file "test/results/test011.answer"))

(display (list 1 2 3 4 5 6 7))
(display #\newline)
(display (list "hello" "mes" "how" "are" "you"))
(display #\newline)
(display (list 4 "hello" 2 "you"))
(display #\newline)
(display (vector->list #(8 6 7 5 3 0 9)))
(display #\newline)
(display (vector->list #((1 2) "hello" 8)))
(display #\newline)
(exit 0)
