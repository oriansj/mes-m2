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
(set-current-output-port (open-output-file "test/results/test063.answer"))
(define (newline) (display #\newline))
(define (for-each f l)
  (if (null? l) *unspecified*
      (begin (f (car l)) (for-each f (cdr l)))))

;;; Test that + and - are first-class values.
;; At first they weren't!

(define (map2 op a b) ; only necessary because Ur-Scheme map is binary
  (cond ((and (null? a) (null? b)) '())
        ((null? a) (error "mismatched list lengths in map2"))
        (else (cons (op (car a) (car b)) (map2 op (cdr a) (cdr b))))))
(define (pr num) (display (number->string num)) (newline))

(for-each pr (map2 + '(0 1 3 5 70) '(1 2 3 4 5)))
(for-each pr (map2 - '(0 1 3 5 70) '(1 2 3 4 5)))

(exit 0)
