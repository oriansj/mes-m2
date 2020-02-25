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
(set-current-output-port (open-output-file "test/results/test051.answer"))
(define (newline) (display #\newline))

;;; Test to verify parsing and compiling of character literals.

(define message "Lisp is one of the oldest languages.")
(define (subst-loop str str2 idx)
  (if (= idx (string-length str)) #f
      (begin
        (if (char=? (string-ref str idx) #\s)
            (string-set! str2 idx #\f)
            (string-set! str2 idx (string-ref str idx)))
        (subst-loop str str2 (+ 1 idx)))))
(define (subst str)
  (let ((buf (make-string (string-length str))))
    (subst-loop str buf 0)
    buf))
(display (subst message))
(newline)

(exit 0)
