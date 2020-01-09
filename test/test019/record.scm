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
(set-current-output-port (open-output-file "test/results/test019.answer"))

; Make our employee record-type
(define employee-type (make-record-type "employee" '(name age salary)))
(display employee-type)
(display #\newline)
(display (record-type-name employee-type))
(display #\newline)
(record-type-fields employee-type)
(display #\newline)

; Something to build our records
(define new-employee (record-constructor employee-type '(name age salary)))

; Alternate record-type
(define intern-type (make-record-type "slave" '(name age)))
(display intern-type)
(display #\newline)

; Make a record
(define new-hire (new-employee "Paul" 42 80000))
(display new-hire)
(display #\newline)

; Check for types
(display ((record-predicate employee-type) new-hire))
(display #\newline)
(display ((record-predicate intern-type) new-hire))
(display #\newline)

; Extract fields
(define get-name (record-accessor employee-type 'name))
(define get-age (record-accessor employee-type 'age))
(define get-salary (record-accessor employee-type 'salary))
(display (get-name new-hire))
(display #\newline)
(display (get-age new-hire))
(display #\newline)
(display (get-salary new-hire))
(display #\newline)

; Set fields
(define set-name! (record-modifier employee-type 'name))
(define set-age! (record-modifier employee-type 'age))
(define set-salary! (record-modifier employee-type 'salary))
(display (set-name! new-hire "Paula"))
(display #\newline)
(display new-hire)
(display #\newline)
(display (set-age! new-hire 38))
(display #\newline)
(display new-hire)
(display #\newline)
(display (set-salary! new-hire 60000))
(display #\newline)
(display new-hire)
(display #\newline)

; Extract record-type from record
(display (record-type-descriptor new-hire))
(display #\newline)

; Ensure we can tell the difference between records and record types
(display (record? new-hire))
(display #\newline)
(display (record? new-employee))
(display #\newline)
(display (record-type? new-hire))
(display #\newline)
(display (record-type? new-employee))
(display #\newline)

(exit 0)
