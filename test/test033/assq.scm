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
(set-current-output-port (open-output-file "test/results/test033.answer"))
(define (newline) (display #\newline))
(define (cadr x) (car (cdr x)))
(define (caar x) (car (car x)))
(define (assq i l) (cond ((null? l) #f) ((eq? i (caar l)) (car l)) (else (assq i (cdr l)))))

(define msglist '((father "The President ")
                  (is "is ")
                  (eating "smoking ")
                  (apples "a reefer.")))
(define printmsg
  (lambda (msg) (if (null? msg) (newline)
                    (begin (display (cadr (assq (car msg) msglist)))
                           (printmsg (cdr msg))))))
(printmsg '(father is eating apples))

(exit 0)
