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
(set-current-output-port (open-output-file "test/results/test009.answer"))

;; Demonstrate using 'S-expression
(display "making a quote with ' character\n")
(write '(display (string-append "Hello " "Guix" "\n")))
(display #\newline)
(write '(a b c (d e)))

;; Demonstrate using (quote S-expression)
(display "\nmake a quote with quote\n")
(write (quote (display (string-append "Hello " "Guix" "\n"))))
(display #\newline)
(write (quote (a b c (d e))))
(exit 0)
