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
(set-current-output-port (open-output-file "test/results/test015.answer"))

(define win #t)
(define lose #f)
(define (game result post1 post2) (if result (display post1) (display post2)))
(define msg1 "We won!\n")
(define msg2 "Grumble!\n")
(define answer 42)
(define success 0)
(game win msg1 msg2)
(game lose msg1 msg2)
(game win msg1 msg2)
(game win msg1 msg2)
(game lose msg1 msg2)
(game lose msg1 msg2)
(game win msg1 msg2)
(game win msg1 msg2)
(game win msg1 msg2)
(game lose msg1 msg2)
(game lose msg1 msg2)
(game lose msg1 msg2)
(exit success)
