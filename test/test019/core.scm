;;; GNU Mes --- Maxwell Equations of Software
;;; Copyright © 2019 Jeremiah Orians
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

(define (record-predicate type)
	(lambda (record)
		(core:record-predicate type record)))

(define (record-accessor type field)
	(lambda (record)
		(core:record-accessor type field record)))

(define (record-modifier type field)
	(lambda (record value)
		(core:record-modifier type field record value)))

(define (record-constructor type fields)
	(lambda (. values)
		(core:record-constructor type fields values)))
