;;; GNU Mes --- Maxwell Equations of Software
;;; Copyright © 2016,2017,2018,2019,2020,2021 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
;;;
;;; This file is part of GNU Mes.
;;;
;;; Also borrowing code from:
;;; guile-sdl2 --- FFI bindings for SDL2
;;; Copyright © 2015 David Thompson <davet@gnu.org>
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

(define-module (mes)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages cross-base)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages man)
  #:use-module (gnu packages mes)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages texinfo)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define %source-dir (getcwd))

(define-public mescc-tools
  (package
    (name "mescc-tools")
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://git.savannah.nongnu.org/cgit/mescc-tools.git/snapshot/"
             name "-Release_" version
             ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "12cjryqfd6m6j807pvhk7i4vr2q0jiibpfrpnq5s67iq9l4rrc6b"))))
    (build-system gnu-build-system)
    (supported-systems
     '("aarch64-linux" "armhf-linux" "i686-linux" "x86_64-linux"))
    (arguments
     `(#:make-flags (list (string-append "PREFIX=" (assoc-ref %outputs "out"))
                          (string-append "CC=" ,(cc-for-target)))
       #:test-target "test"
       #:phases (modify-phases %standard-phases
                  (delete 'configure)
                  (add-after 'unpack 'patch-prefix
                    (lambda _
                      (substitute* "sha256.sh"
                        (("\\$\\(which sha256sum\\)") (which "sha256sum")))
                      #t)))))
    (synopsis "Tools for the full source bootstrapping process")
    (description
     "Mescc-tools is a collection of tools for use in a full source
bootstrapping process.  It consists of the M1 macro assembler, the hex2
linker, the blood-elf symbol table generator, the kaem shell, exec_enable and
get_machine.")
    (home-page "https://savannah.nongnu.org/projects/mescc-tools")
    (license gpl3+)))

(define-public m2-planet
  (let ((commit "46cf81af8303119236cdab5536204d3da3b487de")
        (revision "0"))
    (package
      (name "m2-planet")
      (version (git-version "1.7.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/oriansj/m2-planet.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1qa4qnwgflc4h0z0wyvqx9idr67r4r0yn595b0x6nigp4sij4b32"))))
      (native-inputs
       `(("mescc-tools" ,mescc-tools)))
      (build-system gnu-build-system)
      (arguments
       `(#:make-flags (list (string-append "PREFIX=" (assoc-ref %outputs "out"))
                            (string-append "CC=" ,(cc-for-target)))
         #:tests? #f
         #:phases (modify-phases %standard-phases
                    (delete 'bootstrap)
                    (delete 'configure)
                    (add-after 'unpack 'patch-prefix
                      (lambda _
                        (substitute* "sha256.sh"
                          (("\\$\\(which sha256sum\\)") (which "sha256sum")))
                        #t)))))
      (synopsis "The PLAtform NEutral Transpiler")
      (description
       "M2-Planet, The PLAtform NEutral Transpiler, when combined with
mescc-tools compiles a subset of the C language into working binaries
with introspective steps inbetween.")
      (home-page "https://github.com/oriansj/m2-planet")
      (license gpl3+))))

(define-public mes-m2
  (let ((commit "fd1a1755a1adc65abbada9e8a3273821cb55bcfc")
        (revision "1"))
    (package
      (name "mes-m2")
      (version (git-version "0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://gitlab.com/janneke/mes-m2.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1d70lcw2mdxp0yx3ksswacc80yilwzkrzkjk9z43b9z5gbylwfyr"))))
      (build-system gnu-build-system)
      (propagated-inputs
       `(("mescc-tools" ,mescc-tools)
         ("nyacc" ,nyacc)))
      (native-inputs
       `(("guile" ,guile-3.0-latest)
         ("m2-planet" ,m2-planet)))
      (arguments
       `(#:strip-binaries? #f ; binutil's strip b0rkes MesCC/M1/hex2 binaries
         #:tests? #f
         #:make-flags (list (string-append "PREFIX=" (assoc-ref %outputs "out"))
                            (string-append "CC=" ,(cc-for-target))
                            "mes-m2-boot")
         #:phases (modify-phases %standard-phases
                    (delete 'bootstrap)
                    (delete 'configure))))
      (synopsis "Scheme interpreter and C compiler for full source bootstrapping")
      (description
       "GNU Mes--Maxwell Equations of Software--brings the Reduced Binary Seed
bootstrap to Guix and aims to help create full source bootstrapping for
GNU/Linux distributions.  It consists of a mutual self-hosting Scheme
interpreter in C and a Nyacc-based C compiler in Scheme and is compatible with
Guile.")
      (home-page "https://www.gnu.org/software/mes")
      (license gpl3+))))

(define-public mes-m2.git
  (let ((version "0")
        (revision "0")
        (commit (read-string
                 (open-pipe "git show HEAD | head -1 | cut -d ' ' -f 2" OPEN_READ))))
    (package
      (inherit mes-m2)
      (name "mes-m2.git")
      (version (string-append version "-" revision "." (string-take commit 7)))
      (source (local-file %source-dir
                          #:recursive? #t
                          #:select? (git-predicate %source-dir))))))
