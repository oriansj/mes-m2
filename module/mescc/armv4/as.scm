;;; GNU Mes --- Maxwell Equations of Software
;;; Copyright © 2016,2017,2018,2020 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2019 Danny Milosavljevic <dannym@scratchpost.org>
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

;;; Commentary:

;;; define armv4 assembly

;;; Code:

(define-module (mescc armv4 as)
  #:use-module (mes guile)
  #:use-module (mescc as)
  #:use-module (mescc info)
  #:export (
            armv4:instructions
            ))
;; If 0 <= exp < #x100, use positive-body.
;; If #x-100 < exp < 0, use negative-body.
;; Otherwise, use general-body.
(define-macro (optimize-immediate exp positive-body negative-body
                                  general-body)
 `(let ((exp ,exp))
    (if (>= exp 0)
        (if (< exp #x100)
            ,positive-body
            ,general-body)
        (if (> exp #x-100)
            ,negative-body
            ,general-body))))

(define (armv4:function-preamble . rest)
  "Note: Pretends to be on x86 a lot"
  '(("push___%lr")
    ("push___%ebp")
    ("mov____%esp,%ebp")))

(define (armv4:function-locals . rest)
  `(("allocate_stack_4180"))) ; 4*1024 buf, 20 local vars

(define (armv4:r->local info n)
  (or n (error "invalid value: armv4:r->local: " n))
  (let ((r (get-r info))
        (n (- 0 (* 4 n))))
    `(,`(,(string-append "mov____%" r ",0x32(%ebp)") (#:immediate ,n)))))

(define (immediate->r0 v)
  (optimize-immediate v
   `(((#:immediate1 ,v) "mov____$i8,%r0"))
   `(((#:immediate1 ,(- -1 v)) "mvn____%r0,$i8"))
   `(("mov____$i32,%r0" (#:immediate ,v)))))

(define (armv4:value->r info v)
  (let ((r (get-r info)))
    (optimize-immediate v
     `(((#:immediate1 ,v) ,(string-append "mov____$i8,%" r)))
     `(((#:immediate1 ,(- -1 v)) ,(string-append "mvn____%" r ",$i8")))
     `((,(string-append "mov____$i32,%" r) (#:immediate ,v))))))

(define (armv4:ret . rest)
  "Note: Pretends to be on x86 a lot"
  '(("mov____%ebp,%esp")
    ("pop____%ebp")
    ("ret")))

(define (armv4:r-zero? info)
  (let ((r (get-r info)))
    `(((#:immediate1 #x00) ,(string-append "cmp____$i8,%" r)))))

(define (armv4:local->r info n)
  (let ((r (get-r info))
        (n (- 0 (* 4 n))))
    (optimize-immediate n
     `(((#:immediate1 ,n)
        ,(string-append "ldr____%" r ",(%fp,+#$i8)")))
     `(((#:immediate1 ,(- n))
        ,(string-append "ldr____%" r ",(%fp,-#$i8)")))
     `((,(string-append "mov____0x32(%ebp),%" r)
        (#:immediate ,n))))))

(define (armv4:r0+r1 info)
  (let ((r0 (get-r0 info))
        (r1 (get-r1 info)))
    `((,(string-append "add____%" r1 ",%" r0)))))

(define (armv4:call-label info label n)
  `(((#:offset3 ,label) bl)
    ((#:immediate1 ,(* n 4)) "add____$i8,%esp")))

(define (armv4:r->arg info i)
  (let ((r (get-r info)))
    `((,(string-append "push___%" r)))))

(define (armv4:label->arg info label i)
  `(("push___$i32" (#:address ,label))))

;; Register--register value subtraction
(define (armv4:r0-r1 info)
  (let ((r0 (get-r0 info))
        (r1 (get-r1 info)))
    `((,(string-append "sub____%" r1 ",%" r0)))))

;; Zero flag to register.
(define (armv4:zf->r info)
  (let* ((r (get-r info)))
   `(((#:immediate1 #x00) ,(string-append "mov____$i8,%" r))
     ((#:immediate1 #x01) ,(string-append "moveq__%" r ",$i8")))))

;; C NOT Register value.
(define (armv4:r-negate info)
  (armv4:zf->r info))

(define (armv4:xor-zf info)
  '(((#:immediate1 #x00) "mov____$i8,%r0")
    ((#:immediate1 #x01) "moveq__%r0,$i8")
    ((#:immediate1 #x00) "cmp____$i8,%r0")))

(define (armv4:r->local+n info id n)
  (let ((n (+ (- 0 (* 4 id)) n))
        (r (get-r info)))
    (optimize-immediate n
     `(((#:immediate1 ,n)
        ,(string-append "str____%" r ",(%fp,+#$i8)")))
     `(((#:immediate1 ,(- n))
        ,(string-append "str____%" r ",(%fp,-#$i8)")))
     `((,(string-append "mov____%" r ",0x32(%ebp)")
        (#:immediate ,n))))))

(define (armv4:r-mem-add info v)
  (let ((r (get-r info)))
   `((,(string-append "add____$i32,(%" r ")") (#:immediate ,v)))))

(define (armv4:r-byte-mem-add info v)
  (let ((r (get-r info)))
    `((,(string-append "push___%r0"))
      (,(string-append "ldrsb__%r0,(%" r ")"))
      ,(optimize-immediate v
        `((#:immediate1 ,v) ,(string-append "add____$i8,%r0"))
        `((#:immediate1 ,(- v)) ,(string-append "sub____$i8,%r0"))
        (error "armv4:r-byte-mem-add got immediate that doesn't fit into 8 bits."))
      (,(string-append "strb___%r0,(%" r ")"))
      (,(string-append "pop____%r0")))))

(define (armv4:r-word-mem-add info v)
  (let ((r (get-r info)))
    `((,(string-append "push___%r0"))
      (,(string-append "ldrsh__%r0,(%" r ")"))
      ,(optimize-immediate v
        `((#:immediate1 ,v) ,(string-append "add____$i8,%r0"))
        `((#:immediate1 ,(- v)) ,(string-append "sub____$i8,%r0"))
        `(("add____$i32,(%r0)" (#:immediate ,v))))
      (,(string-append "strh___%r0,(%" r ")"))
      (,(string-append "pop____%r0")))))

(define (armv4:local-ptr->r info n)
  (let ((r (get-r info)))
    (let ((n (- 0 (* 4 n))))
      `((,(string-append "mov____%ebp,%" r))
        ,(optimize-immediate n
          `((#:immediate1 ,n) ,(string-append "add____$i8,%" r))
          `((#:immediate1 ,(- n)) ,(string-append "sub____$i8,%" r))
          `(,(string-append "add____$i32,%" r) (#:immediate ,n)))))))

(define (armv4:label->r info label)
  (let ((r (get-r info)))
    `((,(string-append "mov____$i32,%" r) (#:address ,label)))))

(define (armv4:r0->r1 info)
  (let ((r0 (get-r0 info))
        (r1 (get-r1 info)))
    `((,(string-append  "mov____%" r0 ",%" r1)))))

(define (armv4:byte-mem->r info)
  (let ((r (get-r info)))
    `((,(string-append "ldrsb__%" r ",(%" r ")"))
      ((#:immediate1 #xFF) ,(string-append "and____$i8,%" r)))))

(define (armv4:byte-r info)
  (let* ((r (get-r info)))
    `((,(string-append "uxtb__%" r ",%" r)))))

(define (armv4:byte-signed-r info)
  (let* ((r (get-r info)))
    `((,(string-append "sxtb__%" r ",%" r)))))

(define (armv4:word-r info)
  (let* ((r (get-r info)))
    `((,(string-append "uxth__%" r ",%" r)))))

(define (armv4:word-signed-r info)
  (let* ((r (get-r info)))
    `((,(string-append "sxth__%" r ",%" r)))))

(define (armv4:jump info label)
  `(((#:offset3 ,label) "b")))

(define (armv4:jump-z info label)
  `(((#:offset3 ,label) "je")))

(define (armv4:jump-nz info label)
  `(((#:offset3 ,label) "jne")))

(define (armv4:jump-byte-z info label)
  `(("test___%r0,%r0") ; TODO: 1 Byte ?
    ((#:offset3 ,label) "je")))

;; signed
(define (armv4:jump-g info label)
  `(((#:offset3 ,label) "jg")))

(define (armv4:jump-ge info label)
  `(((#:offset3 ,label) "jge")))

(define (armv4:jump-l info label)
  `(((#:offset3 ,label) "jl" )))

(define (armv4:jump-le info label)
  `(((#:offset3 ,label) "jle")))

;; unsigned
(define (armv4:jump-a info label)
  `(((#:offset3 ,label) "ja")))

(define (armv4:jump-ae info label)
  `(((#:offset3 ,label) "jae")))

(define (armv4:jump-b info label)
  `(((#:offset3 ,label) "jb")))

(define (armv4:jump-be info label)
  `(((#:offset3 ,label) "jbe")))

(define (armv4:byte-r0->r1-mem info)
  (let* ((r0 (get-r0 info))
         (r1 (get-r1 info)))
    `((,(string-append "strb___%" r0 ",(%" r1 ")")))))

(define (armv4:label-mem->r info label)
  (let ((r (get-r info)))
    `((,(string-append "mov____0x32,%" r) (#:address ,label)))))

(define (armv4:word-mem->r info)
  (let ((r (get-r info)))
    `((,(string-append "ldrsh__%" r ",(%" r ")")))))

(define (armv4:mem->r info)
  (let ((r (get-r info)))
    `((,(string-append "mov____(%" r "),%" r)))))

(define (armv4:local-add info n v)
  (let ((n (- 0 (* 4 n))))
    (append (immediate->r0 v)
           `(("mov____0x32(%ebp),%r1" (#:immediate ,n))
             ("add____%r0,%r1")
             ("mov____%r1,0x32(%ebp)" (#:immediate ,n))))))

(define (armv4:label-mem-add info label v)
  (append (immediate->r0 v)
         `(("add____%r0,0x32" (#:address ,label)))))

(define (armv4:nop info)
  '(("nop")))

(define (armv4:swap-r0-r1 info)
  (let ((r0 (get-r0 info))
        (r1 (get-r1 info)))
    `((,(string-append "xchg___%" r0 ",%" r1)))))

(define (armv4:flag->r branchspec info)
  "Find out whether a flag or set of flag has a given set of values and set the value of the register R to 1 if it is so, and to 0 otherwise.
  Possible values for branchspec are one of (\"cs\", \"cc\", \"ge\", \"gt\", \"hi\", \"lt\", \"le\")"
  (let* ((r (get-r info)))
    `(((#:immediate1 #x00) ,(string-append "mov____$i8,%" r))
      ((#:immediate1 #x01) ,(string-append "mov" branchspec "__%" r ",$i8")))))

;; signed
(define (armv4:g?->r info)
  (armv4:flag->r "gt" info))

(define (armv4:ge?->r info)
  (armv4:flag->r "ge" info))

(define (armv4:l?->r info)
  (armv4:flag->r "lt" info))

(define (armv4:le?->r info)
  (armv4:flag->r "le" info))

;; unsigned
(define (armv4:a?->r info)
  (armv4:flag->r "hi" info))

(define (armv4:ae?->r info)
  (armv4:flag->r "cs" info))

(define (armv4:b?->r info)
  (armv4:flag->r "cc" info))

(define (armv4:be?->r info)
  (let* ((r (get-r info)))
    `(((#:immediate1 #x01) ,(string-append "mov____$i8,%" r))
      ((#:immediate1 #x00) ,(string-append "movhi__%" r ",$i8")))))

(define (armv4:test-r info)
  (let ((r (get-r info)))
    `((,(string-append "test___%" r ",%" r)))))

(define (armv4:r->label info label)
  (let ((r (get-r info)))
    `((,(string-append "mov____%" r ",0x32") (#:address ,label)))))

(define (armv4:r->byte-label info label)
  (let* ((r (get-r info))) ; r: byte
    `((,(string-append "movb___%" r ",0x32") (#:address ,label)))))

(define (armv4:r->word-label info label)
  (let* ((r (get-r info))) ; r: halfword
    `((,(string-append "movw___%" r ",0x32") (#:address ,label)))))

(define (armv4:call-r info n)
  (let ((r (get-r info)))
    `((,(string-append "call___*%" r))
      ;; Note: Assumes n > 0.
      ((#:immediate1  ,(* n 4)) "add____$i8,%esp"))))

(define (armv4:r0*r1 info)
  ;; FIXME: Signedness.
  (let ((r0 (get-r0 info))
        (r1 (get-r1 info)))
    `((,(string-append "mul____%" r0 ",%" r1)))))

(define (armv4:r0<<r1 info)
  (let ((r0 (get-r0 info))
        (r1 (get-r1 info)))
    `((,(string-append "lsl____%" r0 ",%" r0 ",%" r1)))))

;; FIXME: lsr??! Signed or unsigned r0?
(define (armv4:r0>>r1 info)
  (let ((r0 (get-r0 info))
        (r1 (get-r1 info)))
    `((,(string-append "lsr____%" r0 ",%" r0 ",%" r1)))))

(define (armv4:r0-and-r1 info)
  (let ((r0 (get-r0 info))
        (r1 (get-r1 info)))
    `((,(string-append "and____%" r1 ",%" r0)))))

(define (armv4:r0/r1 info signed?)
  (let ((r0 (get-r0 info))
        (r1 (get-r1 info)))
    (if signed?
        ;; __mesabi_idiv(a, b)
        (cons* `(,(string-append "push___%" r1))
               `(,(string-append "push___%" r0))
               (armv4:call-label #f "__mesabi_idiv" 2))
        ;; __mesabi_uldiv(a, b, remainderp)
        (cons* `(,(string-append "push___0"))
               `(,(string-append "push___%" r1))
               `(,(string-append "push___%" r0))
               (armv4:call-label #f "__mesabi_uldiv" 3)))))

(define (armv4:r0%r1 info signed?)
  (let ((r0 (get-r0 info))
        (r1 (get-r1 info)))
    (if signed?
        ;; __mesabi_imod(a,b)
        (cons* `(,(string-append "push___%" r1))
               `(,(string-append "push___%" r0))
               (armv4:call-label #f "__mesabi_imod" 2))
        ;; __mesabi_uldiv(a, b, remainderp)
        (append `(("push___%r0") ; slot for remainder
                  ("mov____%esp,%r0")
                  ("push___%r0") ; pointer to remainder
                  (,(string-append "push___%" r1))
                  (,(string-append "push___%" r0)))
                (armv4:call-label #f "__mesabi_uldiv" 3)
                `(("pop____%r0"))))))

(define (armv4:r+value info v)
  (let ((r (get-r info)))
    (optimize-immediate v
     `(((#:immediate1 ,v) ,(string-append "add____$i8,%" r)))
     `(((#:immediate1 ,(- v)) ,(string-append "sub____$i8,%" r)))
     `((,(string-append "add____$i32,%" r) (#:immediate ,v))))))

(define (armv4:r0->r1-mem info)
  (let ((r0 (get-r0 info))
         (r1 (get-r1 info)))
    `((,(string-append "mov____%" r0 ",(%" r1 ")")))))

(define (armv4:byte-r0->r1-mem info)
  (let* ((r0 (get-r0 info))
         (r1 (get-r1 info)))
    `((,(string-append "strb___%" r0 ",(%" r1 ")")))))

(define (armv4:word-r0->r1-mem info)
  (let* ((r0 (get-r0 info))
         (r1 (get-r1 info)))
    `((,(string-append "strh___%" r0 ",(%" r1 ")")))))

(define (armv4:r-cmp-value info v)
  (let ((r (get-r info)))
    (optimize-immediate v
     `(((#:immediate1 ,v) ,(string-append "cmp____$i8,%" r)))
     `(((#:immediate1 ,(- v)) ,(string-append "cmn____$i8,%" r)))
     `((,(string-append "cmp____$i32,%" r) (#:immediate ,v))))))

(define (armv4:push-register info r)
  `((,(string-append "push___%" r))))

(define (armv4:pop-register info r)
  `((,(string-append "pop____%" r))))

(define (armv4:return->r info)
  (let ((r (get-r info)))
    (if (equal? r "r0") '()
        `((,(string-append "mov____%r0,%" r))))))

(define (armv4:r0-or-r1 info)
  (let ((r0 (get-r0 info))
        (r1 (get-r1 info)))
    `((,(string-append "or_____%" r1 ",%" r0)))))

(define (armv4:shl-r info n)
  (let ((r (get-r info)))
    `(((#:immediate1 ,n) ,(string-append "lsl____%" r ",%" r ",$i8")))))

(define (armv4:r+r info)
  (let ((r (get-r info)))
    `((,(string-append "add____%" r ",%" r)))))

(define (armv4:not-r info)
  (let ((r (get-r info)))
    `((,(string-append "not____%" r)))))

(define (armv4:r0-xor-r1 info)
  (let ((r0 (get-r0 info))
        (r1 (get-r1 info)))
    `((,(string-append "xor____%" r1 ",%" r0)))))

(define (armv4:r0-mem->r1-mem info)
  (let* ((registers (.registers info))
         (r0 (get-r0 info))
         (r1 (get-r1 info))
         (r2 (car registers)))
    `((,(string-append "mov____(%" r0 "),%" r2))
      (,(string-append "mov____%" r2 ",(%" r1 ")")))))

(define (armv4:byte-r0-mem->r1-mem info)
  (let* ((registers (.registers info))
         (r0 (get-r0 info))
         (r1 (get-r1 info))
         (r2 (car registers)))
    `((,(string-append "ldrsb__%" r2 ",(%" r0 ")"))
      (,(string-append "strb___%" r2 ",(%" r1 ")")))))

(define (armv4:word-r0-mem->r1-mem info)
  (let* ((registers (.registers info))
         (r0 (get-r0 info))
         (r1 (get-r1 info))
         (r2 (car registers)))
    `((,(string-append "mov____(%" r0 "),%" r2))
      (,(string-append "strh___%" r2 ",(%" r1 ")")))))

(define (armv4:r0+value info v)
  (let ((r0 (get-r0 info)))
    (optimize-immediate v
     `(((#:immediate1 ,v) ,(string-append "add____$i8,%" r0)))
     `(((#:immediate1 ,(- v)) ,(string-append "sub____$i8,%" r0)))
     `((,(string-append "add____$i32,%" r0) (#:immediate ,v))))))

(define (armv4:value->r0 info v)
  (let ((r0 (get-r0 info)))
    `((,(string-append "mov____$i32,%" r0) (#:immediate ,v)))))

(define (armv4:byte-r->local+n info id n)
  (let* ((n (+ (- 0 (* 4 id)) n))
         (r (get-r info)))
    `(,(optimize-immediate n
        `((#:immediate1 ,n)
          ,(string-append "strb___%" r ",(%fp,+#$i8)"))
        `((#:immediate1 ,(- n))
          ,(string-append "strb___%" r ",(%fp,-#$i8)"))
        `(,(string-append "strb___%" r ",0x32(%ebp)")
          (#:immediate ,n))))))

(define (armv4:word-r->local+n info id n)
  (let* ((n (+ (- 0 (* 4 id)) n))
         (r (get-r info)))
    (optimize-immediate n
     `(((#:immediate1 ,n)
        ,(string-append "strh___%" r ",(%fp,+#$i8)")))
     `(((#:immediate1 ,(- n))
        ,(string-append "strh___%" r ",(%fp,-#$i8)")))
     `((,(string-append "strh___%" r ",0x32(%ebp)"))))))

(define (armv4:r-and info v)
  (let ((r (get-r info)))
    `(((#:immediate1 ,v) ,(string-append "and____$i8,%" r)))))

(define (armv4:push-r0 info)
  (let ((r0 (get-r0 info)))
    `((,(string-append "push___%" r0)))))

(define (armv4:r1->r0 info)
  (let ((r0 (get-r0 info))
        (r1 (get-r1 info)))
    `((,(string-append  "mov____%" r1 ",%" r0)))))

(define (armv4:pop-r0 info)
  (let ((r0 (get-r0 info)))
    `((,(string-append "pop____%" r0)))))

(define (armv4:swap-r-stack info)
  (let ((r (get-r info)))
    `((,(string-append "xchg___%" r ",(%esp)")))))

(define (armv4:swap-r1-stack info)
  (let ((r0 (get-r0 info)))
    `((,(string-append "xchg___%" r0 ",(%esp)")))))

(define (armv4:r2->r0 info)
  (let ((r0 (get-r0 info))
        (r1 (get-r1 info))
        (allocated (.allocated info)))
    (if (> (length allocated) 2)
        (let ((r2 (cadddr allocated)))
          `((,(string-append  "mov____%" r2 ",%" r1))))
        `((,(string-append  "pop____%" r0))
          (,(string-append  "push___%" r0))))))

(define armv4:instructions
  `(
    (a?->r . ,armv4:a?->r)
    (ae?->r . ,armv4:ae?->r)
    (b?->r . ,armv4:b?->r)
    (be?->r . ,armv4:be?->r)
    (byte-mem->r . ,armv4:byte-mem->r)
    (byte-r . ,armv4:byte-r)
    (byte-r->local+n . ,armv4:byte-r->local+n)
    (byte-r0->r1-mem . ,armv4:byte-r0->r1-mem)
    (byte-r0->r1-mem . ,armv4:byte-r0->r1-mem)
    (byte-r0-mem->r1-mem . ,armv4:byte-r0-mem->r1-mem)
    (byte-signed-r . ,armv4:byte-signed-r)
    (call-label . ,armv4:call-label)
    (call-r . ,armv4:call-r)
    (function-locals . ,armv4:function-locals)
    (function-preamble . ,armv4:function-preamble)
    (g?->r . ,armv4:g?->r)
    (ge?->r . ,armv4:ge?->r)
    (jump . ,armv4:jump)
    (jump-a . ,armv4:jump-a)
    (jump-ae . ,armv4:jump-ae)
    (jump-b . ,armv4:jump-b)
    (jump-be . ,armv4:jump-be)
    (jump-byte-z . ,armv4:jump-byte-z)
    (jump-g . , armv4:jump-g)
    (jump-ge . , armv4:jump-ge)
    (jump-l . ,armv4:jump-l)
    (jump-le . ,armv4:jump-le)
    (jump-nz . ,armv4:jump-nz)
    (jump-z . ,armv4:jump-z)
    (l?->r . ,armv4:l?->r)
    (label->arg . ,armv4:label->arg)
    (label->r . ,armv4:label->r)
    (label-mem->r . ,armv4:label-mem->r)
    (label-mem-add . ,armv4:label-mem-add)
    (le?->r . ,armv4:le?->r)
    (local->r . ,armv4:local->r)
    (local-add . ,armv4:local-add)
    (local-ptr->r . ,armv4:local-ptr->r)
    (long-r0->r1-mem . ,armv4:r0->r1-mem)
    (long-r0-mem->r1-mem . ,armv4:r0-mem->r1-mem)
    (mem->r . ,armv4:mem->r)
    (nop . ,armv4:nop)
    (not-r . ,armv4:not-r)
    (pop-r0 . ,armv4:pop-r0)
    (pop-register . ,armv4:pop-register)
    (push-r0 . ,armv4:push-r0)
    (push-register . ,armv4:push-register)
    (r+r . ,armv4:r+r)
    (r+value . ,armv4:r+value)
    (r->arg . ,armv4:r->arg)
    (r->byte-label . ,armv4:r->byte-label)
    (r->label . ,armv4:r->label)
    (r->local . ,armv4:r->local)
    (r->local+n . ,armv4:r->local+n)
    (r->word-label . ,armv4:r->word-label)
    (r-and . ,armv4:r-and)
    (r-byte-mem-add . ,armv4:r-byte-mem-add)
    (r-cmp-value . ,armv4:r-cmp-value)
    (r-mem-add . ,armv4:r-mem-add)
    (r-negate . ,armv4:r-negate)
    (r-word-mem-add . ,armv4:r-word-mem-add)
    (r-zero? . ,armv4:r-zero?)
    (r0%r1 . ,armv4:r0%r1)
    (r0*r1 . ,armv4:r0*r1)
    (r0+r1 . ,armv4:r0+r1)
    (r0+value . ,armv4:r0+value)
    (r0->r1 . ,armv4:r0->r1)
    (r0->r1-mem . ,armv4:r0->r1-mem)
    (r0-and-r1 . ,armv4:r0-and-r1)
    (r0-mem->r1-mem . ,armv4:r0-mem->r1-mem)
    (r0-or-r1 . ,armv4:r0-or-r1)
    (r0-r1 . ,armv4:r0-r1)
    (r0-xor-r1 . ,armv4:r0-xor-r1)
    (r0/r1 . ,armv4:r0/r1)
    (r0<<r1 . ,armv4:r0<<r1)
    (r0>>r1 . ,armv4:r0>>r1)
    (r1->r0 . ,armv4:r1->r0)
    (r2->r0 . ,armv4:r2->r0)
    (ret . ,armv4:ret)
    (return->r . ,armv4:return->r)
    (shl-r . ,armv4:shl-r)
    (swap-r-stack . ,armv4:swap-r-stack)
    (swap-r0-r1 . ,armv4:swap-r0-r1)
    (swap-r1-stack . ,armv4:swap-r1-stack)
    (test-r . ,armv4:test-r)
    (value->r . ,armv4:value->r)
    (value->r0 . ,armv4:value->r0)
    (word-mem->r . ,armv4:word-mem->r)
    (word-r . ,armv4:word-r)
    (word-r->local+n . ,armv4:word-r->local+n)
    (word-r0->r1-mem . ,armv4:word-r0->r1-mem)
    (word-r0-mem->r1-mem . ,armv4:word-r0-mem->r1-mem)
    (word-signed-r . ,armv4:word-signed-r)
    (xor-zf . ,armv4:xor-zf)
    (zf->r . ,armv4:zf->r)
    ))
