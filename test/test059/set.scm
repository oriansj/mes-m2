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
(set-current-output-port (open-output-file "test/results/test059.answer"))
(define (newline) (display #\newline))

;;; Tests of set!
;; There are three ways variables are stored at the moment: globally,
;; on the stack, and in the heap.

(define global "global")
(define (printglobal) (display global) (newline))
(printglobal)
;; We'd like to ensure that the stack effect of set! (all three kinds)
;; is correct.  It should return exactly one value, not zero or two.
;; But we can't care about what its actual value is, because it's
;; probably something different in the reference Scheme
;; implementation.

;; Executing it in what you'd call void context in C doesn't tell us
;; anything; it's unlikely we'll crash the program by popping off too
;; many stack items, and leaving extra stack items there is safe.  So
;; we have to pass it as an argument to something.  But it has to be
;; in a position where we're using the stuff underneath it; at the
;; moment, that means it needs to be the first argument.

(display (cdr (cons (set! global "global variable") "and ")))
(printglobal)
(set! global "global variable gets set!")
(printglobal)

(define (printlocal local)
  (display (cdr (cons (set! local "local variable always the same")
                      "the right stuff: ")))
  (display local)
  (newline))
(printlocal "a")
(printlocal "b")

(define heap-printer
  (let ((val "original heap var"))
    (lambda (cmd arg)
      (case cmd
        ((set!) (display (cdr (cons (set! val arg) "set-"))))
        ((print) (begin (display val) (newline)))
        (else (error "bad cmd" cmd))))))

(heap-printer 'print 0)
(heap-printer 'set! "heap var")
(heap-printer 'print 0)
(heap-printer 'set! "heap var changes!")
(heap-printer 'print 0)

(exit 0)
