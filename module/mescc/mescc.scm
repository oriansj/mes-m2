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

(define-module (mescc mescc)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 getopt-long)
  #:use-module (mes guile)
  #:use-module (mes misc)

  #:use-module (mescc i386 info)
  #:use-module (mescc x86_64 info)
  #:use-module (mescc preprocess)
  #:use-module (mescc compile)
  #:use-module (mescc M1)
  #:export (mescc:preprocess
            mescc:compile
            mescc:assemble
            mescc:link))

(define GUILE-with-output-to-file with-output-to-file)
(define (with-output-to-file file-name thunk)
  (if (equal? file-name "-") (thunk)
      (GUILE-with-output-to-file file-name thunk)))

(define (mescc:preprocess options)
  (let* ((pretty-print/write (string->symbol (option-ref options 'write (if guile? "pretty-print" "write"))))
         (pretty-print/write (if (eq? pretty-print/write 'pretty-print) pretty-print write))
         (files (option-ref options '() '("a.c")))
         (input-file-name (car files))
         (ast-file-name (cond ((and (option-ref options 'preprocess #f)
                                    (option-ref options 'output #f)))
                              (else (replace-suffix input-file-name ".E"))))
         (dir (dirname input-file-name))
         (defines (reverse (filter-map (multi-opt 'define) options)))
         (includes (reverse (filter-map (multi-opt 'include) options)))
         (includes (cons dir includes))
         (prefix (option-ref options 'prefix ""))
         (machine (option-ref options 'machine "32"))
         (arch (if (equal? machine "32") "__i386__=1" "__x86_64__=1"))
         (defines (cons arch defines)))
    (with-output-to-file ast-file-name
      (lambda _ (for-each (cut c->ast prefix defines includes write <>) files)))))

(define (c->ast prefix defines includes write file-name)
  (with-input-from-file file-name
    (cut write (c99-input->ast #:prefix prefix #:defines defines #:includes includes))))

(define (mescc:compile options)
  (let* ((files (option-ref options '() '("a.c")))
         (input-file-name (car files))
         (M1-file-name (cond ((and (option-ref options 'compile #f)
                                   (option-ref options 'output #f)))
                             (else (replace-suffix input-file-name ".S"))))
         (infos (map (cut file->info options <>) files))
         (verbose? (option-ref options 'verbose #f))
         (align? (option-ref options 'align #f)))
    (when verbose?
      (stderr "dumping: ~a\n" M1-file-name))
    (with-output-to-file M1-file-name
      (cut infos->M1 M1-file-name infos #:align? align?))
    M1-file-name))

(define (file->info options file-name)
  (cond ((.c? file-name) (c->info options file-name))
        ((.E? file-name) (E->info options file-name))))

(define (c->info options file-name)
  (let* ((defines (reverse (filter-map (multi-opt 'define) options)))
         (includes (reverse (filter-map (multi-opt 'include) options)))
         (dir (dirname file-name))
         (includes (cons dir includes))
         (prefix (option-ref options 'prefix ""))
         (machine (option-ref options 'machine "32"))
         (info (if (equal? machine "32") (x86-info)  (x86_64-info)))
         (arch (if (equal? machine "32") "__i386__=1" "__x86_64__=1"))
         (defines (cons arch defines)))
    (with-input-from-file file-name
      (cut c99-input->info info #:prefix prefix #:defines defines #:includes includes))))

(define (E->info options file-name)
  (let* ((ast (with-input-from-file file-name read))
         (machine (option-ref options 'machine "32"))
         (info (if (equal? machine "32") (x86-info)  (x86_64-info))))
    (c99-ast->info info ast)))

(define (mescc:assemble options)
  (let* ((files (option-ref options '() '("a.c")))
         (input-file-name (car files))
         (hex2-file-name (cond ((and (option-ref options 'assemble #f)
                                     (option-ref options 'output #f)))
                               (else (replace-suffix input-file-name ".o"))))
         (S-files (filter .S? files))
         (hex2-files  M1->hex2 ) ;; FIXME
         (source-files (filter (disjoin .c? .E?) files))
         (infos (map (cut file->info options <>) source-files)))
    (if (and (pair? S-files) (pair? infos))
        (error "mixing source and object not supported:" source-files S-files))
    (when (pair? S-files)
      (M1->hex2 options S-files))
    (when (pair? infos)
      (infos->hex2 options hex2-file-name infos))
    hex2-file-name))

(define (mescc:link options)
  (let* ((files (option-ref options '() '("a.c")))
         (source-files (filter (disjoin .c? .E?) files))
         (S-files (filter .S? files))
         (o-files (filter .o? files))
         (input-file-name (car files))
         (hex2-file-name (if (or (string-suffix? ".hex2" input-file-name)
                                 (string-suffix? ".o" input-file-name)) input-file-name
                                 (replace-suffix input-file-name ".o")))
         (infos (map (cut file->info options <>) source-files))
         (S-files (filter .S? files))
         (hex2-files (filter .o? files))
         (hex2-files (if (null? S-files) hex2-files
                         (append hex2-files (list (M1->hex2 options S-files)))))
         (hex2-files (if (null? infos) hex2-files
                         (append hex2-files
                                 (list (infos->hex2 options hex2-file-name infos)))))
         (default-libraries (if (or (option-ref options 'nodefaultlibs #f)
                                    (option-ref options 'nostdlib #f)) '()
                                    '("c")))
         (libraries (filter-map (multi-opt 'library) options))
         (libraries (delete-duplicates (append libraries default-libraries)))
         (hex2-libraries (map (cut find-library options ".o" <>) libraries))
         (hex2-files (append hex2-files hex2-libraries))
         (S-files (append S-files (map (cut find-library options ".S" <>)  libraries)))
         (debug-info? (option-ref options 'debug-info #f))
         (S-files (cons (replace-suffix input-file-name ".S") S-files))
         (elf-footer (and debug-info?
                          (or (M1->blood-elf options S-files)
                              (exit 1)))))
    (or (hex2->elf options hex2-files #:elf-footer elf-footer)
        (exit 1))))

(define (infos->hex2 options hex2-file-name infos)
  (let* ((input-file-name (car (option-ref options '() '("a.c"))))
         (M1-file-name (replace-suffix hex2-file-name ".S"))
         (options (acons 'compile #t options)) ; ugh
         (options (acons 'output hex2-file-name options))
         (verbose? (option-ref options 'verbose #f))
         (align? (option-ref options 'align #f)))
    (when verbose?
      (stderr "dumping: ~a\n" M1-file-name))
    (with-output-to-file M1-file-name
      (cut infos->M1 M1-file-name infos #:align? align?))
    (or (M1->hex2 options (list M1-file-name))
        (exit 1))))

(define (M1->hex2 options M1-files)
  (let* ((input-file-name (car (option-ref options '() '("a.c"))))
         (M1-file-name (car M1-files))
         (hex2-file-name (cond ((and (option-ref options 'assemble #f)
                                     (option-ref options 'output #f)))
                               ((option-ref options 'assemble #f)
                                (replace-suffix input-file-name ".o"))
                               (else (replace-suffix M1-file-name ".o"))))
         (machine (option-ref options 'machine "32"))
         (architecture (cond
                        ((equal? machine "32") "x86")
                        ((equal? machine "64") "amd64")
                        (else "1")))
         (m1-macros (cond
                     ((equal? machine "32") "x86.M1")
                     ((equal? machine "64") "x86_64.M1")
                     (else "x86.M1")))
         (verbose? (option-ref options 'verbose #f))
         (M1 (or (getenv "M1") "M1"))
         (command `(,M1
                    "--LittleEndian"
                    "--architecture" ,architecture
                    "-f" ,(arch-find options m1-macros)
                    ,@(append-map (cut list "-f" <>) M1-files)
                    "-o" ,hex2-file-name)))
    (when verbose?
      (stderr "~a\n" (string-join command)))
    (and (zero? (apply assert-system* command))
         hex2-file-name)))

(define* (hex2->elf options hex2-files #:key elf-footer)
  (let* ((input-file-name (car (option-ref options '() '("a.c"))))
         (elf-file-name (cond ((option-ref options 'output #f))
                              (else (replace-suffix input-file-name ""))))
         (verbose? (option-ref options 'verbose #f))
         (hex2 (or (getenv "HEX2") "hex2"))
         (machine (option-ref options 'machine "32"))
         (architecture (cond
                         ((equal? machine "32") "x86")
                         ((equal? machine "64") "amd64")
                         (else "1")))
         (base-address (option-ref options 'base-address "0x1000000"))
         (elf-footer (or elf-footer
                         (arch-find options (string-append
                                             "elf" machine "-footer-single-main.hex2"))))
         (start-files (if (or (option-ref options 'nostartfiles #f)
                              (option-ref options 'nostdlib #f)) '()
                              `("-f" ,(arch-find options "crt1.o"))))
         (command `(,hex2
                    "--LittleEndian"
                    "--architecture" ,architecture
                    "--BaseAddress" ,base-address
                    "-f" ,(arch-find options (string-append "elf" machine "-header.hex2"))
                    ,@start-files
                    ,@(append-map (cut list "-f" <>) hex2-files)
                    "-f" ,elf-footer
                    "--exec_enable"
                    "-o" ,elf-file-name)))
    (when verbose?
      (stderr "~a\n" (string-join command)))
    (and (zero? (apply assert-system* command))
         elf-file-name)))

(define (M1->blood-elf options M1-files)
  (let* ((M1-file-name (car M1-files))
         (M1-blood-elf-footer (string-append M1-file-name ".blood-elf"))
         (hex2-file-name (replace-suffix M1-file-name ".o"))
         (blood-elf-footer (string-append hex2-file-name ".blood-elf"))
         (verbose? (option-ref options 'verbose #f))
         (blood-elf (or (getenv "BLOOD_ELF") "blood-elf"))
         (machine (option-ref options 'machine "32"))
         (m1-macros (cond
                     ((equal? machine "32") "x86.M1")
                     ((equal? machine "64") "x86_64.M1")
                     (else "x86.M1")))
         (command `(,blood-elf
                      "-f" ,(arch-find options m1-macros)
                      ,@(append-map (cut list "-f" <>) M1-files)
                      "-o" ,M1-blood-elf-footer)))
    (when verbose?
        (format (current-error-port) "~a\n" (string-join command)))
    (and (zero? (apply assert-system* command))
         (let* ((options (acons 'compile #t options)) ; ugh
                (options (acons 'output blood-elf-footer options)))
           (M1->hex2 options (list M1-blood-elf-footer))))))

(define (replace-suffix file-name suffix)
  (let* ((parts (string-split file-name #\.))
         (base (if (pair? (cdr parts)) (drop-right parts 1)))
         (old-suffix (last parts))
         (program-prefix (cond ((string-prefix? "x86-mes-" old-suffix) ".x86-mes-")
                               ((string-prefix? "x86_64-mes-" old-suffix) ".x86_64-mes-")
                               (else "."))))
    (if (string-null? suffix)
        (if (string-null? program-prefix) (string-join base ".")
            (string-append (string-drop program-prefix 1) (string-join base ".")))
        (string-append (string-join base ".") program-prefix (string-drop suffix 1)))))

(define (find-library options ext o)
  (arch-find options (string-append "lib" o ext)))

(define* (arch-find options file-name)
  (let* ((srcdest (or (getenv "srcdest") ""))
         (srcdir-lib (string-append srcdest "lib"))
         (machine (option-ref options 'machine "32"))
         (arch (cond
                ((equal? machine "32") "x86-mes")
                ((equal? machine "64") "x86_64-mes")
                (else "x86-mes")))
         (path (cons* srcdir-lib
                      (prefix-file options "lib")
                      (filter-map (multi-opt 'library-dir) options)))
         (arch-file-name (string-append arch "/" file-name))
         (verbose? (option-ref options 'verbose #f)))
    (let ((file (search-path path arch-file-name)))
      (when verbose?
        (stderr "arch-find=~s\n" arch-file-name)
        (stderr "     path=~s\n" path)
        (stderr "  => ~s\n" file))
      (or file
          (error (format #f "mescc: file not found: ~s" arch-file-name))))))

(define (prefix-file options file-name)
  (let ((prefix (option-ref options 'prefix "")))
    (define (prefix-file o)
      (if (string-null? prefix) o (string-append prefix "/" o)))
    (prefix-file file-name)))

(define (assert-system* . args)
  (let ((status (apply system* args)))
    (when (not (zero? status))
      (stderr "mescc: failed: ~a\n" (string-join args))
      (exit (status:exit-val status)))
    status))

(define (multi-opt option-name) (lambda (o) (and (eq? (car o) option-name) (cdr o))))

(define (.c? o) (or (string-suffix? ".c" o)
                    (string-suffix? ".M2" o)))
(define (.E? o) (or (string-suffix? ".E" o)
                    (string-suffix? ".mes-E" o)
                    (string-suffix? ".x86-mes-E" o)
                    (string-suffix? ".x86_64-mes-E" o)))
(define (.S? o) (or (string-suffix? ".S" o)
                    (string-suffix? ".mes-S" o)
                    (string-suffix? ".x86-mes-S" o)
                    (string-suffix? ".x86_64-mes-S" o)
                    (string-suffix? "S" o)
                    (string-suffix? ".M1" o)))
(define (.o? o) (or (string-suffix? ".o" o)
                    (string-suffix? ".mes-o" o)
                    (string-suffix? ".x86-mes-o" o)
                    (string-suffix? ".x86_64-mes-o" o)
                    (string-suffix? ".hex2" o)))
