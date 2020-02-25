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
(set-current-output-port (open-output-file "test/results/test058.answer"))
(define (newline) (display #\newline))

;;; Tests for char-alphabetic?

(define (x a b)
  (if (= a b) (newline)
      (begin (display (if (char-alphabetic? (integer->char a)) "!" "."))
             (x (+ 1 a) b))))
(x 0 32)
(x 32 64)
(x 64 96)
(x 96 128)
; stick to ASCII!
;(x 128 160)
;(x 160 192)
;(x 192 224)
;(x 224 256)

(exit 0)
