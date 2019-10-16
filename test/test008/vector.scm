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
(prim:set-current-output-port (prim:open-output-file "test/results/test008.answer"))

;; Demonstrate making a vector
(prim:display "making vector with make-vector\n")
(define v (prim:make-vector 4))
(prim:display v)

;; Test vector-length
(prim:display "\nusing vector-length\n")
(prim:display (prim:vector-length v))

;; Test vector-set!
(prim:display "\nusing vector-set!\n")
(prim:vector-set! v 0 3)
(prim:vector-set! v 1 9)
(prim:vector-set! v 2 7)
(prim:vector-set! v 3 8)
(prim:display v)

;; Validate vector->list
(prim:display "\nusing vector->list\n")
(prim:display (prim:vector->list v))

;; Validate list->vector
(prim:display "\nusing list->vector\n")
(set! v (prim:list->vector (prim:list 1 2 3 4)))
(prim:display v)

;; Validate vector-ref
(prim:display "\nUsing vector-ref\n")
(prim:display (prim:vector-ref v 3))
(prim:display #\newline)
(prim:display (prim:vector-ref v 2))
(prim:display #\newline)
(prim:display (prim:vector-ref v 1))
(prim:display #\newline)
(prim:display (prim:vector-ref v 0))
(prim:display #\newline)
(prim:exit 0)
