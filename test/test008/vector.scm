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
(core:display "making vector with make-vector\n")
(define v (make-vector 4))
(core:display v)

;; Test vector-length
(core:display "\nusing vector-length\n")
(core:display (vector-length v))

;; Test vector-set!
(core:display "\nusing vector-set!\n")
(vector-set! v 0 3)
(vector-set! v 1 9)
(vector-set! v 2 7)
(vector-set! v 3 8)
(core:display v)

;; Validate vector->list
(core:display "\nusing vector->list\n")
(core:display (vector->list v))

;; Validate list->vector
(core:display "\nusing list->vector\n")
(set! v (list->vector (list 1 2 3 4)))
(core:display v)

;; Validate vector-ref
(core:display "\nUsing vector-ref\n")
(core:display (vector-ref v 3))
(core:display #\newline)
(core:display (vector-ref v 2))
(core:display #\newline)
(core:display (vector-ref v 1))
(core:display #\newline)
(core:display (vector-ref v 0))
(core:display #\newline)
(exit 0)
