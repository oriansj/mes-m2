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
(set-current-output-port (open-output-file "test/results/test008.answer"))

;; Demonstrate making a vector
(display "making vector with make-vector\n")
(define v (make-vector 4))
(display v)

;; Test vector-length
(display "\nusing vector-length\n")
(display (vector-length v))

;; Test vector-set!
(display "\nusing vector-set!\n")
(vector-set! v 0 3)
(vector-set! v 1 9)
(vector-set! v 2 7)
(vector-set! v 3 8)
(display v)

;; Validate vector->list
(display "\nusing vector->list\n")
(display (vector->list v))

;; Validate list->vector
(display "\nusing list->vector\n")
(set! v (list->vector (list 1 2 3 4)))
(display v)

;; Validate vector-ref
(display "\nUsing vector-ref\n")
(display (vector-ref v 3))
(display #\newline)
(display (vector-ref v 2))
(display #\newline)
(display (vector-ref v 1))
(display #\newline)
(display (vector-ref v 0))
(display #\newline)
(exit 0)
