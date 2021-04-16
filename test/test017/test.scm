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

(define (display x) (core:display x))
(define (write x) (core:write x))

(define (not a) (if a #f #t))

(define (append a b) (append2 a b))

(define (list? x)
  (if (null? x) #t
	  (if (pair? x) (list? (cdr x))
		  #f)))

(define-macro (cond . clauses)
  (list 'if (pair? clauses)
        (list (cons
               'lambda
               (cons
                '(test)
                (list (list 'if 'test
                            (if (pair? (cdr (car clauses)))
                                (if (eq? (car (cdr (car clauses))) '=>)
                                    (append2 (cdr (cdr (car clauses))) '(test))
                                    (list (cons 'lambda (cons '() (cons 'test (cdr (car clauses)))))))
                                (list (cons 'lambda (cons '() (cons 'test (cdr (car clauses)))))))
                            (if (pair? (cdr clauses))
                                (cons 'cond (cdr clauses)))))))
              (car (car clauses)))))

(primitive-load "test/test017/varargs.scm")
(exit 69)
