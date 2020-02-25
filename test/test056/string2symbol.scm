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
(set-current-output-port (open-output-file "test/results/test056.answer"))
(define (newline) (display #\newline))
(define (for-each f l)
  (if (null? l) *unspecified*
      (begin (f (car l)) (for-each f (cdr l)))))

;;; Test for string->symbol.

;; Ur-Scheme is case-sensitive, so this test needs to compare it with
;; either a case-sensitive Scheme like MzScheme or a
;; lowercase-preferred Scheme like Elk.

(define (xp a) (lambda (b) (display (if (eq? a (string->symbol b)) "X" "P"))))
(for-each (xp 'ha) '("Ha" "ha" "ho" "hee" "hee" "ha" "haaa."))
(newline)
((xp "who") (string-append "w" "ho"))
(newline)

(exit 0)
