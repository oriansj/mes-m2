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
(set-current-output-port (open-output-file "test/results/test039.answer"))
(define (newline) (display #\newline))

;;; Test to ensure that tail-calls don't use stack.
(define do-n (lambda (n) (if (= n 0) #t (do-n (- n 1)))))
(do-n 100000)                           ; 50 000 fit in 1024k of stack
(define do-n-with-begin 
  (lambda (n) (if (= n 0) #t (begin (do-n-with-begin (- n 1))))))
(do-n-with-begin 100000)
(define do-n-with-else 
  (lambda (n) (if (not (= n 0)) (do-n-with-else (- n 1))
                  #t)))
(do-n-with-else 100000)
(display "Ok")
(newline)
;; Incidentally, SBCL can count down from a hundred million in 
;; * (time (do-n 100000000))
;; ... 6.172386 seconds of user run time

;; So that's 62ns per iteration, or 16 million iterations per second
;; (on my 700MHz laptop).  This Scheme compiler is getting about 6.5
;; million, which is like 110 cycles per call.  Even Python 2.4 gets
;; 2.2 million for-loop iterations on this machine.  (But it can only
;; do around half a million function call/returns in a second.)  Lua
;; 5.0.3 gets 1.15 million iterations of do-n per second, and 2.3
;; million for-loop iterations per second. So don't get cocky.

(exit 0)
