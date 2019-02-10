;;; GNU Mes --- Maxwell Equations of Software
;;; Copyright © 2016,2017,2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

(define-module (mescc)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 getopt-long)
  #:use-module (mes misc)
  #:use-module (mescc mescc)
  #:export (mescc:main))

(cond-expand
 (mes
  (define (set-port-encoding! port encoding) #t)
  (mes-use-module (mes guile))
  (mes-use-module (mes misc))
  (mes-use-module (mes getopt-long))
  (mes-use-module (mes display))
  (mes-use-module (mescc mescc))
  )
 (guile
  (define-macro (mes-use-module . rest) #t)))

(define %prefix (getenv "%prefix"))
(define %version (getenv "%version"))

(when (and=> (getenv "V") (lambda (v) (> (string->number v) 1)))
  (format (current-error-port) "mescc[~a]...\n" %scheme))

(define (unclump-single o)
  (cond ((string-prefix? "--" o) (list o))
        ((and (string-prefix? "-" o)
              (> (string-length o) 2)
              (not (eq? (string-ref o 2) #\space)))
         (list (substring o 0 2)
               (substring o 2)))
        (else (list o))))

(define (parse-opts args)
  (let* ((option-spec
          '((align)
            (assemble (single-char #\c))
            (base-address (value #t))
            (compile (single-char #\S))
            (define (single-char #\D) (value #t))
            (debug-info (single-char #\g))
            (dumpmachine (single-char #\d))
            (help (single-char #\h))
            (include (single-char #\I) (value #t))
            (library-dir (single-char #\L) (value #t))
            (library (single-char #\l) (value #t))
            (machine (single-char #\m) (value #t))
            (preprocess (single-char #\E))
            (std (value #t))
            (output (single-char #\o) (value #t))
            (optimize (single-char #\O) (value #t))
            (version (single-char #\V))
            (verbose (single-char #\v))
            (write (single-char #\w) (value #t))
            (language (single-char #\x) (value #t))))
         (options (getopt-long args option-spec))
         (help? (option-ref options 'help #f))
         (files (option-ref options '() '()))
         (usage? (and (not help?) (null? files)))
         (version? (option-ref options 'version #f)))
    (cond ((option-ref options 'dumpmachine #f)
           (display "x86-mes")
           (exit 0))
          (version? (format #t "mescc (GNU Mes) ~a\n" %version) (exit 0))
          (else
           (and (or help? usage?)
                (format (or (and usage? (current-error-port)) (current-output-port)) "\
Usage: mescc [OPTION]... FILE...
  --align             align globals
  -dumpmachine        display the compiler's target processor
  --base-address=ADRRESS
                      use BaseAddress ADDRESS [0x1000000]
  -D DEFINE[=VALUE]   define DEFINE [VALUE=1]
  -E                  preprocess only; do not compile, assemble or link
  -g                  add debug info [GDB, objdump] TODO: hex2 footer
  -h, --help          display this help and exit
  -I DIR              append DIR to include path
  -L DIR              append DIR to library path
  -l LIBNAME          link with LIBNAME
  -m BITS             compile for BITS bits [32]
  -o FILE             write output to FILE
  -O LEVEL            use optimizing LEVEL
  -S                  preprocess and compile only; do not assemble or link
  --std=STANDARD      assume that the input sources are for STANDARD
  -v, --version       display version and exit
  -w,--write=TYPE     dump Nyacc AST using TYPE {pretty-print,write}
  -x LANGUAGE         specify LANGUAGE of the following input files

Environment variables:

  MES=BINARY          run on mes-executable BINARY {mes,guile}
  MES_DEBUG=LEVEL     show debug output with verbosity LEVEL {0..5}
  NYACC_TRACE=1       show Nyacc progress

Report bugs to: bug-mes@gnu.org
GNU Mes home page: <http://gnu.org/software/mes/>
General help using GNU software: <http://gnu.org/gethelp/>
")
                (exit (or (and usage? 2) 0)))
           options))))

(define (mescc:main args)
  (let* ((single-dash-options '("-dumpmachine" "-std"))
         (args (map (lambda (o)
                      (if (member o single-dash-options) (string-append "-" o)
                          o))
                    args))
         (args (append-map unclump-single args))
         (options (parse-opts args))
         (options (acons 'prefix %prefix options))
         (preprocess? (option-ref options 'preprocess #f))
         (compile? (option-ref options 'compile #f))
         (assemble? (option-ref options 'assemble #f))
         (verbose? (option-ref options 'verbose (getenv "MES_DEBUG"))))
    (when verbose?
      (setenv "NYACC_TRACE" "yes")
      (format (current-error-port) "options=~s\n" options))
    (cond (preprocess? (mescc:preprocess options))
          (compile? (mescc:compile options))
          (assemble? (mescc:assemble options))
          (else (mescc:link options)))))
