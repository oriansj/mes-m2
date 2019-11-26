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
(set-current-output-port (open-output-file "test/results/test018.answer"))

; Test integer functionality
(display "= tests:\n")
(display (= 0 0 0 0 0 0 0 0 0))
(display #\newline)
(display (= -1 -1))
(display #\newline)
(display (= 1 1))
(display #\newline)
(display (= -1 1))
(display #\newline)
(display (= 1 -1))
(display #\newline)

(display "> tests:\n")
(display (> 4 3 2 1 0 -1 -2 -3 -4))
(display #\newline)
(display (> -1 -1))
(display #\newline)
(display (> 1 1))
(display #\newline)
(display (> -1 1))
(display #\newline)
(display (> 1 -1))
(display #\newline)

(display ">= tests:\n")
(display (> 4 3 2 1 0 0 0 -1 -2 -3 -4))
(display #\newline)
(display (> -1 -1))
(display #\newline)
(display (> 1 1))
(display #\newline)
(display (> -1 1))
(display #\newline)
(display (> 1 -1))
(display #\newline)

(display "< tests:\n")
(display (< -4 -3 -2 -1 0 1 2 3 4))
(display #\newline)
(display (< -1 -1))
(display #\newline)
(display (< 1 1))
(display #\newline)
(display (< -1 1))
(display #\newline)
(display (< 1 -1))
(display #\newline)

(display "<= tests:\n")
(display (<= -4 -3 -2 -1 0 0 0 1 2 3 4))
(display #\newline)
(display (<= -1 -1))
(display #\newline)
(display (<= 1 1))
(display #\newline)
(display (<= -1 1))
(display #\newline)
(display (<= 1 -1))
(display #\newline)


; Test char functionality
(display "char=? tests:\n")
(display (char=? #\A #\A #\A #\A #\A))
(display #\newline)
(display (char=? #\B #\B))
(display #\newline)
(display (char=? #\C #\C))
(display #\newline)
(display (char=? #\B #\C))
(display #\newline)
(display (char=? #\C #\B))
(display #\newline)


; Test string functionality
(display "string=? tests:\n")
(display (string=? "" "" "" "" ""))
(display #\newline)
(display (string=? "foo" "foo"))
(display #\newline)
(display (string=? "bar" "bar"))
(display #\newline)
(display (string=? "foo" "bar"))
(display #\newline)
(display (string=? "bar" "foo"))
(display #\newline)


; Test eq? functionality per r7rs
(display "\neq? tests:\n")
(display (eq? 'a 'a))
(display #\newline)

;; WTF guile shell shows #t and put it in file and it is #f??
(display (eq? '(a) '(a)))
(display #\newline)

(display (eq? (list 'a) (list 'a)))
(display #\newline)

;; WTF guile shell shows #t and put it in file and it is #f??
(display (eq? "a" "a"))
(display #\newline)
(display (eq? "" ""))
(display #\newline)

(display (eq? '() '()))
(display #\newline)
(display (eq? 2 2))
(display #\newline)
(display (eq? #\A #\A))
(display #\newline)
(display (eq? car car))
(display #\newline)
(display (let ((n (+ 2 3))) (eq? n n)))
(display #\newline)
(display (let ((n '(a))) (eq? n n)))
(display #\newline)
(display (let ((n '#())) (eq? n n)))
(display #\newline)
(display (let ((p (lambda (x) x))) (eq? p p)))
(display #\newline)

; Test equal? functionality per r7rs
(display "\nequal? tests:\n")
(display (equal? 'a 'a))
(display #\newline)
(display (equal? '(a) '(a)))
(display #\newline)
(display (equal? '(a (b) c) '(a (b) c)))
(display #\newline)
(display (equal? "abc" "abc"))
(display #\newline)
(display (equal? 2 2))
(display #\newline)
(display (equal? (make-vector 5 'a) (make-vector 5 'a)))
(display #\newline)
(display (equal? (lambda (x) x) (lambda (y) y)))
(display #\newline)

;; Now to test further
(display (equal? (cons 1 2) (cons 1 2)))
(display #\newline)
(display (equal? (cons 1 2) (cons 1 '())))
(display #\newline)
(display (equal? #((cons 1 2) (cons 3 '())) #((cons 1 2) (cons 3 '()))))
(display #\newline)
(display (equal? #((cons 1 2) (cons 3 '())) #((cons 1 2) (cons 4 '()))))
(display #\newline)
(display (equal? (list #(1 2) #(3 4)) (list #(1 2) #(3 4))))
(display #\newline)
(display (equal? (list #(1 2) #(3 4)) (list #(1 2) #(3 5))))
(display #\newline)
(exit 0)
