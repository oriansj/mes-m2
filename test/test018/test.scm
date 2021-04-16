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

(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

(define (caaar x) (car (car (car x))))
(define (caadr x) (car (car (cdr x))))
(define (cadar x) (car (cdr (car x))))
(define (caddr x) (car (cdr (cdr x))))
(define (cdaar x) (cdr (car (car x))))
(define (cdadr x) (cdr (car (cdr x))))
(define (cddar x) (cdr (cdr (car x))))
(define (cdddr x) (cdr (cdr (cdr x))))

(define (not a) (if a #f #t))

(define-macro (and . x)
  (if (null? x) #t
      (if (null? (cdr x)) (car x)
          (list 'if (car x) (cons 'and (cdr x))
                #f))))

(define-macro (or . x)
  (if (null? x) #f
      (if (null? (cdr x)) (car x)
          (list (list 'lambda (list 'r)
                      (list 'if 'r 'r
                            (cons 'or (cdr x))))
                (car x)))))

(define (append a b) (append2 a b))

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

(define else #t)
(define (cons* . a)
  (begin
    (define (_cons* a) (if (null? (cdr a)) (car a) (cons (car a) (_cons* (cdr a)))))
    (_cons* a)))

(define (quasiquote-expand x)
  (cond ((vector? x) (list 'list->vector (quasiquote-expand (vector->list x))))
        ((not (pair? x)) (cons 'quote (cons x '())))
        ((eq? (car x) 'quasiquote) (quasiquote-expand (quasiquote-expand
                                             (if (null? (cddr x)) (cadr x)
                                                 (cons 'list (cdr x))))))
        ((eq? (car x) 'unquote) (if (null? (cddr x)) (cadr x)
                                    (cons 'list (cdr x))))
        ((and (pair? (car x)) (eq? (caar x) 'unquote-splicing))
         ((lambda (d)
            (if (null? (cddar x)) (list 'append (cadar x) d)
                (list 'quote (append (cdar x) d))))
          (quasiquote-expand (cdr x))))
        (else ((lambda (a d)
                 (if (pair? d)
                     (if (eq? (car d) 'quote)
                         (if (and (pair? a) (eq? (car a) 'quote))
                             (list 'quote (cons (cadr a) (cadr d)))
                             (if (null? (cadr d))
                                 (list 'list a)
                                 (list 'cons* a d)))
                         (if (memq (car d) '(list cons*))
                             (cons (car d) (cons a (cdr d)))
                             (list 'cons* a d)))
                     (list 'cons* a d)))
               (quasiquote-expand (car x))
               (quasiquote-expand (cdr x))))))

(define-macro (quasiquote x)
  (quasiquote-expand x))

(define (map1 f lst)
  (if (null? lst) (list)
      (cons (f (car lst)) (map1 f (cdr lst)))))

(define-macro (simple-let bindings . rest)
  (cons (cons 'lambda (cons (map1 car bindings) rest))
        (map1 cadr bindings)))

(define-macro (xsimple-let bindings rest)
  `(,`(lambda ,(map1 car bindings) ,@rest)
    ,@(map1 cadr bindings)))

(define-macro (xnamed-let name bindings rest)
  `(simple-let ((,name *unspecified*))
     (set! ,name (lambda ,(map1 car bindings) ,@rest))
     (,name ,@(map1 cadr bindings))))

(define-macro (let bindings-or-name . rest)
  (if (symbol? bindings-or-name)
      `(xnamed-let ,bindings-or-name ,(car rest) ,(cdr rest))
      `(xsimple-let ,bindings-or-name ,rest)))

(define (equal? . x)
  (if (or (null? x) (null? (cdr x))) #t
      (if (null? (cddr x)) (equal2? (car x) (cadr x))
          (and (equal2? (car x) (cadr x))
               (apply equal? (cdr x))))))

(primitive-load "test/test018/equal.scm")
(exit 69)
