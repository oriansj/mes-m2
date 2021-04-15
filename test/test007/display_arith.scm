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
(set-current-output-port (open-output-file "test/results/test007.answer"))

;; Test -
(core:display "Test -\n")
(core:display (- 4 1))
(core:display #\newline)
(core:display (- 1 4))
(core:display #\newline)
(core:display (- 4 4))
(core:display #\newline)

;; Test -
(core:display "Test +\n")
(core:display (+ 4 1))
(core:display #\newline)
(core:display (+ 1 -4))
(core:display #\newline)
(core:display (+ 4 4))
(core:display #\newline)

;; Test quotient
(core:display "Test quotient\n")
(core:display (quotient 4 1))
(core:display #\newline)
(core:display (quotient 1 4))
(core:display #\newline)
(core:display (quotient 4 -1))
(core:display #\newline)

;; Test Remainder
(core:display "Test remainder\n")
(core:display (remainder 13 4))
(core:display #\newline)
(core:display (remainder -13 4))
(core:display #\newline)

;; Test modulo
(core:display "Test modulo\n")
(core:display (modulo 13 4))
(core:display #\newline)
(core:display (modulo -13 4))
(core:display #\newline)
(core:display (modulo 13 -4))
(core:display #\newline)
(core:display (modulo -13 -4))
(core:display #\newline)

;; Test *
(core:display "Test *\n")
(core:display (* 2 2 2 2))
(core:display #\newline)
(core:display (* 1 4))
(core:display #\newline)
(core:display (* 4 -1))
(core:display #\newline)

;; Test logand
(core:display "Test logand\nUsing:")
(core:display #x123456)
(core:display #\newline)
(core:display (logand #x123456 #xFF000000))
(core:display #\newline)
(core:display (logand #x123456 #xFF0000))
(core:display #\newline)
(core:display (logand #x123456 #xFF00))
(core:display #\newline)
(core:display (logand #x123456 #xFF))
(core:display #\newline)
(core:display (logand #x123456 -1))
(core:display #\newline)

;; Test logior
(core:display "Test logior\n")
(core:display (logior #x0 #x0))
(core:display #\newline)
(core:display (logior #x0 #xFF))
(core:display #\newline)
(core:display (logior #xFF #x0))
(core:display #\newline)
(core:display (logior #xFF #xFF))
(core:display #\newline)
(core:display (logior #xFF00 -1))
(core:display #\newline)


;; Test not
(core:display "Test lognot\n")
(core:display (lognot 1))
(core:display #\newline)
(core:display (lognot -1))
(core:display #\newline)


;; Test logxor
(core:display "Test logxor\n")
(core:display (logxor #xFF00FF #xFF00))
(core:display #\newline)
(core:display (logxor #xFF00FF #xF00F0))
(core:display #\newline)
(core:display (logxor #xF0F0F0 #xFF00FF))
(core:display #\newline)

;; Test ash
(core:display "Test ash\n")
(core:display (ash 4 1))
(core:display #\newline)
(core:display (ash 1 4))
(core:display #\newline)
(core:display (ash 4 -1))
(core:display #\newline)
(core:display (ash #xFF00 -8))
(core:display #\newline)
(exit 0)
