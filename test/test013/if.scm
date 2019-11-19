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
(set-current-output-port (open-output-file "test/results/test013.answer"))

(if #t (display "Hello mes") (display "OOOOOPS!"))
(if #t (if #t (display #\newline) (display "OOOOOPS!!")) (display "OOOOOPS!!!"))
(if #f (exit 2) (if #t (if #f (exit 4) (display "Good job")) (exit 3)))
(if '() (display #\newline) (exit 5))
(if #f (exit 32) (exit 0))
