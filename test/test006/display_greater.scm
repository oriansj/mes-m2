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
(prim:set-current-output-port (prim:open-output-file "test/results/test006.answer"))

;; Test >
(prim:display "Test >\n")
(prim:display (prim:greater_p 4 2 1))
(prim:display #\newline)
(prim:display (prim:greater_p 1 2 4))
(prim:display #\newline)
(prim:display (prim:greater_p 4 2 4))
(prim:display #\newline)

;; Test <
(prim:display "Test <\n")
(prim:display (prim:less_p 1 2 4))
(prim:display #\newline)
(prim:display (prim:less_p 4 2 1))
(prim:display #\newline)
(prim:display (prim:less_p 4 2 4))
(prim:display #\newline)

;; Test =
(prim:display "Test =\n")
(prim:display (prim:is_p 4 4 4))
(prim:display #\newline)
(prim:display (prim:is_p 1 4 4))
(prim:display #\newline)
(prim:display (prim:is_p 4 4 1))
(prim:display #\newline)
(prim:display (prim:is_p 4 1 4))
(prim:display #\newline)
(prim:exit 0)
