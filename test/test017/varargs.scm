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
(set-current-output-port (open-output-file "test/results/test017.answer"))

;; Flatten nested list example
(define (flatten x)
	(cond
		((null? x) '())
		((not (list? x)) (list x))
		(#t (append (flatten (car x)) (flatten (cdr x))))))

(display (flatten '((1) 2 ((3 4) 5) ((())) (((6))) 7 8 ())))
(display #\newline)

;; Simple Vararg example
(define (foo1 a)
	(cond
		((null? a) 0)
		((not (list? a)) (* a a))
		(#t (+ (foo1 (car a)) (foo1 (cdr a))))))

(define (foo . a)
	(cond
		((null? a) 0)
		(#t (foo1 a))))

(display (foo))
(display #\newline)
(display (foo 4))
(display #\newline)
(display (foo 1 2 3 4 5))
(display #\newline)
(display (foo (flatten '((1) 2 ((3 4) 5) ((())) (((6))) 7 8 ()))))
(display #\newline)
(exit 0)
