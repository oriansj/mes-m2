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
(set-current-output-port (open-output-file "test/results/test057.answer"))
(define (newline) (display #\newline))

;;; Tests for integer->char and char->integer.

(define buf (make-string 1))
(define (print-chartable i max)
  (if (= i max) (newline)
      (begin (string-set! buf 0 (integer->char i))
             (display buf)
             (display (if (= i (char->integer (string-ref buf 0))) " " "!"))
             (print-chartable (+ 1 i) max))))
(print-chartable 32 64)
(print-chartable 64 96)
(print-chartable 96 127)

(exit 0)
