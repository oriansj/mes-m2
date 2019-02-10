#! /gnu/store/d3jd0rrvg9cwlfsrqxf6mqpgqp85fkcf-profile/bin/guile \
--no-auto-compile -e main -L /usr/local/share/guile/site/2.2 -C /usr/local/lib/guile/2.2/site-ccache -s
!#
;;; GNU Mes --- Maxwell Equations of Software
;;; Copyright ?? 2016,2017,2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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
;;; along with GNU Mes.  If not, see <http://www.gnu.org/licenses/>.

(setenv "%prefix" (or (getenv "MES_PREFIX")
                      (if (string-prefix? "@prefix" "/usr/local")
                          ""
                          "/usr/local/share/mes")))

(setenv "%version" (if (string-prefix? "@VERSION" "0.19") "git"
                       "0.19"))

(cond-expand
 (mes
  (mes-use-module (mescc))
  (mescc:main (command-line)))
 (guile
  (use-modules (mescc))))

(define (main args)
  (mescc:main args))
