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
(prim:set-current-output-port (prim:open-output-file "test/results/test007.answer"))

;; Test -
(prim:display "Test -\n")
(prim:display (prim:minus 4 1))
(prim:display #\newline)
(prim:display (prim:minus 1 4))
(prim:display #\newline)
(prim:display (prim:minus 4 4))
(prim:display #\newline)

;; Test -
(prim:display "Test +\n")
(prim:display (prim:plus 4 1))
(prim:display #\newline)
(prim:display (prim:plus 1 -4))
(prim:display #\newline)
(prim:display (prim:plus 4 4))
(prim:display #\newline)

;; Test /
(prim:display "Test /\n")
(prim:display (prim:divide 4 1))
(prim:display #\newline)
(prim:display (prim:divide 1 4))
(prim:display #\newline)
(prim:display (prim:divide 4 -1))
(prim:display #\newline)

;; Test modulo
(prim:display "Test modulo\n")
(prim:display (prim:modulo 4 1))
(prim:display #\newline)
(prim:display (prim:modulo 1 4))
(prim:display #\newline)
(prim:display (prim:modulo 4 3))
(prim:display #\newline)
(prim:display (prim:modulo 4 -3))
(prim:display #\newline)

;; Test *
(prim:display "Test *\n")
(prim:display (prim:multiply 2 2 2 2))
(prim:display #\newline)
(prim:display (prim:multiply 1 4))
(prim:display #\newline)
(prim:display (prim:multiply 4 -1))
(prim:display #\newline)

;; Test logand
(prim:display "Test logand\nUsing:")
(prim:display 0x123456)
(prim:display #\newline)
(prim:display (prim:logand 0x123456 0xFF000000))
(prim:display #\newline)
(prim:display (prim:logand 0x123456 0xFF0000))
(prim:display #\newline)
(prim:display (prim:logand 0x123456 0xFF00))
(prim:display #\newline)
(prim:display (prim:logand 0x123456 0xFF))
(prim:display #\newline)
(prim:display (prim:logand 0x123456 -1))
(prim:display #\newline)

;; Test logior
(prim:display "Test logior\n")
(prim:display (prim:logior 0x0 0x0))
(prim:display #\newline)
(prim:display (prim:logior 0x0 0xFF))
(prim:display #\newline)
(prim:display (prim:logior 0xFF 0x0))
(prim:display #\newline)
(prim:display (prim:logior 0xFF 0xFF))
(prim:display #\newline)
(prim:display (prim:logior 0xFF00 -1))
(prim:display #\newline)


;; Test not
(prim:display "Test lognot\n")
(prim:display (prim:lognot 1))
(prim:display #\newline)
(prim:display (prim:lognot -1))
(prim:display #\newline)


;; Test logxor
(prim:display "Test logxor\n")
(prim:display (prim:logxor 0xFF00FF 0xFF00))
(prim:display #\newline)
(prim:display (prim:logxor 0xFF00FF 0xF00F0))
(prim:display #\newline)
(prim:display (prim:logxor 0xF0F0F0 0xFF00FF))
(prim:display #\newline)

;; Test ash
(prim:display "Test ash\n")
(prim:display (prim:ash 4 1))
(prim:display #\newline)
(prim:display (prim:ash 1 4))
(prim:display #\newline)
(prim:display (prim:ash 4 -1))
(prim:display #\newline)
(prim:display (prim:ash 0xFF00 -8))
(prim:display #\newline)
(prim:exit 0)
