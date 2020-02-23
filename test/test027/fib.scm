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
(set-current-output-port (open-output-file "test/results/test027.answer"))
(define (newline) (display #\newline))

;;; A dumb fibonacci test --- the first recursive code ever compiled.
;; At least, by the Ur-Scheme compiler.

;; This used to display "+" and "*" depending on which case you fell
;; into, which produced an interesting pattern of
;; "+*+*++*+*++*++*+*++*+", which I think can be produced by a simple
;; L-system.  However, the exact pattern depends on the order of
;; argument evaluation, which differs between this compiler and (at
;; least) MzScheme.
(define fibonacci
  (lambda (x) (if (= x 0) (begin (display "+") 1 )
                  (if (= x 1) (begin (display "+") 1)
                      (+ (fibonacci (- x 1))
                         (fibonacci (- x 2)))))))
(fibonacci 7)
(newline)

(exit 0)
