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
(set-current-output-port (open-output-file "test/results/test006.answer"))

;; Test >
(display "Test >\n")
(display (> 4 2 1))
(display #\newline)
(display (> 1 2 4))
(display #\newline)
(display (> 4 2 4))
(display #\newline)

;; Test <
(display "Test <\n")
(display (< 1 2 4))
(display #\newline)
(display (< 4 2 1))
(display #\newline)
(display (< 4 2 4))
(display #\newline)

;; Test =
(display "Test =\n")
(display (= 4 4 4))
(display #\newline)
(display (= 1 4 4))
(display #\newline)
(display (= 4 4 1))
(display #\newline)
(display (= 4 1 4))
(display #\newline)
(exit 0)
