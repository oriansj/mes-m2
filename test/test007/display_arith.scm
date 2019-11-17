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
(display "Test -\n")
(display (- 4 1))
(display #\newline)
(display (- 1 4))
(display #\newline)
(display (- 4 4))
(display #\newline)

;; Test -
(display "Test +\n")
(display (+ 4 1))
(display #\newline)
(display (+ 1 -4))
(display #\newline)
(display (+ 4 4))
(display #\newline)

;; Test quotient
(display "Test quotient\n")
(display (quotient 4 1))
(display #\newline)
(display (quotient 1 4))
(display #\newline)
(display (quotient 4 -1))
(display #\newline)

;; Test Remainder
(display "Test remainder\n")
(display (remainder 13 4))
(display #\newline)
(display (remainder -13 4))
(display #\newline)

;; Test modulo
(display "Test modulo\n")
(display (modulo 13 4))
(display #\newline)
(display (modulo -13 4))
(display #\newline)
(display (modulo 13 -4))
(display #\newline)
(display (modulo -13 -4))
(display #\newline)

;; Test *
(display "Test *\n")
(display (* 2 2 2 2))
(display #\newline)
(display (* 1 4))
(display #\newline)
(display (* 4 -1))
(display #\newline)

;; Test logand
(display "Test logand\nUsing:")
(display #x123456)
(display #\newline)
(display (logand #x123456 #xFF000000))
(display #\newline)
(display (logand #x123456 #xFF0000))
(display #\newline)
(display (logand #x123456 #xFF00))
(display #\newline)
(display (logand #x123456 #xFF))
(display #\newline)
(display (logand #x123456 -1))
(display #\newline)

;; Test logior
(display "Test logior\n")
(display (logior #x0 #x0))
(display #\newline)
(display (logior #x0 #xFF))
(display #\newline)
(display (logior #xFF #x0))
(display #\newline)
(display (logior #xFF #xFF))
(display #\newline)
(display (logior #xFF00 -1))
(display #\newline)


;; Test not
(display "Test lognot\n")
(display (lognot 1))
(display #\newline)
(display (lognot -1))
(display #\newline)


;; Test logxor
(display "Test logxor\n")
(display (logxor #xFF00FF #xFF00))
(display #\newline)
(display (logxor #xFF00FF #xF00F0))
(display #\newline)
(display (logxor #xF0F0F0 #xFF00FF))
(display #\newline)

;; Test ash
(display "Test ash\n")
(display (ash 4 1))
(display #\newline)
(display (ash 1 4))
(display #\newline)
(display (ash 4 -1))
(display #\newline)
(display (ash #xFF00 -8))
(display #\newline)
(exit 0)
