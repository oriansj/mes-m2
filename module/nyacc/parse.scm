;;; nyacc/parse.scm

;; Copyright (C) 2014-2018 Matthew R. Wette
;;
;; This library is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.
;;
;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public License
;; along with this library; if not, see <http://www.gnu.org/licenses/>

;;; Description:

;; procedures to generate parsers, given a lexical analyzer
;; one for files; one for interactive use: newline is possible end of input

;;; Code:

(define-module (nyacc parse)
  #:export (make-lalr-parser
	    make-lalr-parser/sym
	    make-lalr-parser/num))

(use-modules (ice-9 pretty-print))
(define pp pretty-print)

(define $default 1)			; sync w/ lalr.scm
(define $error 2)			; sync w/ lalr.scm

(define (vector-map proc vec)		; see (srfi srfi-43)
  (let* ((ln (vector-length vec)) (res (make-vector ln)))
    (let iter ((ix 0))
      (unless (= ix ln)
	(vector-set! res ix (proc ix (vector-ref vec ix)))
	(iter (1+ ix))))
    res))

(define (wrap-action actn)		; see util.scm
  (define (mkarg i) (string->symbol (string-append "$" (number->string i))))
  (define (make-arg-list n) (let iter ((r '(. $rest)) (i 1))
			      (if (> i n) r (iter (cons (mkarg i) r) (1+ i)))))
  (cons* 'lambda (make-arg-list (car actn)) (cdr actn)))

(define (make-xct av)
  (if (procedure? (vector-ref av 0))
      av
      (vector-map (lambda (ix f) (eval f (current-module)))
		  (vector-map (lambda (ix actn) (wrap-action actn)) av))))

(define (sferr fmt . args)
  (apply simple-format (current-error-port) fmt args))

(define (dmsg/n s t a)
  (cond
   ((not a) (sferr "state ~S, token ~S\t=> parse error\n" s t))
   ((positive? a) (sferr "state ~S, token ~S\t=> shift, goto ~S\n" s t a))
   ((negative? a) (sferr "state ~S, token ~S\t=> reduce ~S\n" s t (- a)))
   ((zero? a) (sferr "state ~S, token ~S\t=> accept\n" s t))
   (else (error "coding error in (nyacc parse)"))))

(define (dmsg/s s t a)
  (case (car a)
    ((error) (sferr "state ~S, token ~S\t=> parse error\n" s t))
    ((shift) (sferr "state ~S, token ~S\t=> shift, goto ~S\n" s t (cdr a)))
    ((reduce) (sferr "state ~S, token ~S\t=> reduce ~S\n" s t (cdr a)))
    ((accept) (sferr "state ~S, token ~S\t=> accept\n" s t))
    (else (error "coding error in (nyacc parse)"))))

(define (parse-error state laval)
  (let ((fn (or (port-filename (current-input-port)) "(unknown)"))
	(ln (1+ (port-line (current-input-port)))))
    (throw 'nyacc-error
	   "~A:~A: parse failed at state ~A, on input ~S\n"
	   fn ln (car state) (cdr laval))))

(define* (make-lalr-parser/sym mach #:key (skip-if-unexp '()) interactive)
  (let* ((len-v (assq-ref mach 'len-v))
	 (rto-v (assq-ref mach 'rto-v))
	 (pat-v (assq-ref mach 'pat-v))
	 (xct-v (make-xct (assq-ref mach 'act-v)))
	 (start (assq-ref (assq-ref mach 'mtab) '$start)))
    (lambda* (lexr #:key debug)
      (let iter ((state (list 0))	; state stack
		 (stack (list '$@))	; semantic value stack
		 (nval #f)		; non-terminal from prev reduction
		 (lval #f))		; lexical value (from lex'er)
	(cond
	 ((and interactive nval 
	       (eqv? (car nval) start)
	       (zero? (car state)))     ; done
	  (cdr nval))
	 ((not (or nval lval))
	  (if (eqv? '$default (caar (vector-ref pat-v (car state))))
	      (iter state stack (cons '$default #f) lval) ; default reduction
	      (iter state stack nval (lexr))))		  ; reload
	 (else
	  (let* ((laval (or nval lval))
		 (tval (car laval))
		 (sval (cdr laval))
		 (stxl (vector-ref pat-v (car state)))
		 (stx (or (assq-ref stxl tval) (assq-ref stxl '$default)
			  (cons '$error #f))))
	    (if debug (dmsg/s (car state) (if nval tval sval) stx))
	    (cond
	     ((eq? '$error (car stx))	; error ???
	      (if (memq tval skip-if-unexp)
		  (iter state stack #f #f)
		  (parse-error state laval)))
	     ((eq? 'reduce (car stx))	; reduce
	      (let* ((gx (cdr stx))
		     (gl (vector-ref len-v gx))
		     ($$ (apply (vector-ref xct-v gx) stack)))
		(iter (list-tail state gl)
		      (list-tail stack gl)
		      (cons (vector-ref rto-v gx) $$)
		      lval)))
	     ((eq? 'shift (car stx))	; shift
	      (iter (cons (cdr stx) state) (cons sval stack)
		    #f (if nval lval #f)))
	     (else			; accept
	      (car stack))))))))))

(define* (make-lalr-parser/num mach #:key (skip-if-unexp '()) interactive)
  (let* ((len-v (assq-ref mach 'len-v))
	 (rto-v (assq-ref mach 'rto-v))
	 (pat-v (assq-ref mach 'pat-v))
	 (xct-v (make-xct (assq-ref mach 'act-v)))
	 (start (assq-ref (assq-ref mach 'mtab) '$start)))
    (lambda* (lexr #:key debug)
      (let iter ((state (list 0))	; state stack
		 (stack (list '$@))	; semantic value stack
		 (nval #f)		; non-terminal from prev reduction
		 (lval #f))		; lexical value (from lex'r)
	(cond
	 ((and interactive nval
	       (eqv? (car nval) start)
	       (zero? (car state)))     ; done
	  (cdr nval))
	 ((not (or nval lval))
	  (if (eqv? $default (caar (vector-ref pat-v (car state))))
	      (iter state stack (cons $default #f) lval) ; default reduction
	      (iter state stack nval (lexr))))		 ; reload
	 (else
	  (let* ((laval (or nval lval))
		 (tval (car laval))
		 (sval (cdr laval))
		 (stxl (vector-ref pat-v (car state)))
		 (stx (or (assq-ref stxl tval)
			  (and (not (memq tval skip-if-unexp))
			       (assq-ref stxl $default)))))
	    (if debug (dmsg/n (car state) (if nval tval sval) stx))
	    (cond
	     ((eq? #f stx)		; error
	      (if (memq tval skip-if-unexp)
		  (iter state stack #f #f)
		  (parse-error state laval)))
	     ((negative? stx)		; reduce
	      (let* ((gx (abs stx))
		     (gl (vector-ref len-v gx))
		     ($$ (apply (vector-ref xct-v gx) stack)))
		(iter (list-tail state gl)
		      (list-tail stack gl)
		      (cons (vector-ref rto-v gx) $$)
		      lval)))
	     ((positive? stx)		; shift
	      (iter (cons stx state) (cons sval stack) #f (if nval lval #f)))
	     (else			; accept
	      (car stack))))))))))

;; @deffn {Procedure} make-lalr-parser mach [options] => parser
;; Generate a procedure for parsing a language, where @var{mach} is
;; a machine generated by @code{make-lalr-machine}.
;; This generates a procedure that takes one argument, a lexical analyzer:
;; @example
;; (parser lexical-analyzer #:debug #t)
;; @end example
;; @noindent
;; and is used as
;; @example
;; (define xyz-parse (make-lalr-parser xyz-mach))
;; (with-input-from-file "sourcefile.xyz"
;;   (lambda () (xyz-parse (gen-lexer))))
;; @end example
;; @noindent
;; The generated parser is reentrant.  Options are:
;; @table @code
;; @item #:skip-if-unexp 
;; This is a list of tokens to skip if not expected.  It is used
;; to allow comments to be skipped.  The default is @code{'()}.
;; @item #:interactive
;; If @code{#t}, this tells the parserthat this is being called
;; interactively, so that the token @code{$end} is not expected.
;; The default value is @code{#f}.
;; @end table
;; @noindent
;; @end deffn
(define* (make-lalr-parser mach #:key (skip-if-unexp '()) interactive)
  "- Procedure: make-lalr-parser mach [options] => parser
     Generate a procedure for parsing a language, where MACH is a
     machine generated by 'make-lalr-machine'.  This generates a
     procedure that takes one argument, a lexical analyzer:
          (parser lexical-analyzer #:debug #t)
     and is used as
          (define xyz-parse (make-lalr-parser xyz-mach))
          (with-input-from-file \"sourcefile.xyz\"
            (lambda () (xyz-parse (gen-lexer))))
     The generated parser is reentrant.  Options are:
     '#:skip-if-unexp'
          This is a list of tokens to skip if not expected.  It is used
          to allow comments to be skipped.  The default is ''()'.
     '#:interactive'
          If '#t', this tells the parserthat this is being called
          interactively, so that the token '$end' is not expected.  The
          default value is '#f'."
  (let* ((mtab (assq-ref mach 'mtab))
	 (siu (map (lambda (n) (assoc-ref mtab n)) skip-if-unexp))
	 (iact interactive))
    (if (number? (caar (vector-ref (assq-ref mach 'pat-v) 0)))
	;; hashed:
	(make-lalr-parser/num mach #:skip-if-unexp siu #:interactive iact)
	;; not hashed:
	(make-lalr-parser/sym mach #:skip-if-unexp siu #:interactive iact))))


;; == deprecated =============================================================

(define* (old-make-lalr-parser mach)
  (define (dmsg s t a) (sferr "state ~S, token ~S\t=> ~S\n" s t a))
  (let* ((len-v (assq-ref mach 'len-v))	 ; production RHS length
	 (rto-v (assq-ref mach 'rto-v))	 ; reduce to
	 (pat-v (assq-ref mach 'pat-v))	 ; parse action (shift, reduce) table
	 (actn-v (assq-ref mach 'act-v)) ; symbolic actions
	 (mtab (assq-ref mach 'mtab))
	 (xact-v (if (procedure? (vector-ref actn-v 0)) actn-v
		     (vector-map
		      ;; Turn symbolic action into executable procedures:
		      (lambda (ix f) (eval f (current-module)))
		      (vector-map
		       (lambda (ix actn) (wrap-action actn))
		       actn-v))))
	 ;;
	 (hashed (number? (caar (vector-ref pat-v 0)))) ; been hashified?
	 (def (if hashed $default '$default))
	 (end (assq-ref mtab '$end))
	 (err (assq-ref mtab '$error))
	 (comm (list (assq-ref mtab '$lone-comm) (assq-ref mtab '$code-comm)))
	 ;; predicate to test for shift action:
	 (shift? (if hashed
		     (lambda (a) (positive? a))
		     (lambda (a) (eq? 'shift (car a)))))
	 ;; On shift, transition to this state:
	 (shift-to (if hashed (lambda (x) x) (lambda (x) (cdr x))))
	 ;; Predicate to test for reduce action:
	 (reduce? (if hashed
		      (lambda (a) (negative? a))
		      (lambda (a) (eq? 'reduce (car a)))))
	 ;; On reduce, reduce this production-rule:
	 (reduce-pr (if hashed abs cdr))
	 ;; If error, make the right packet.
	 (other (if hashed 0 '(other . 0)))
	 )

    (lambda* (lexr #:key debug)
      (let iter ((state (list 0))	; state stack
		 (stack (list '$@))	; sval stack
		 (nval #f)		; prev reduce to non-term val
		 (lval (lexr)))		; lexical value (from lex'er)

	(let* ((tval (car (if nval nval lval))) ; token (syntax value)
	       (sval (cdr (if nval nval lval))) ; semantic value
	       (stxl (vector-ref pat-v (car state))) ; state transition xtra
	       (oact #f) ;; if not shift/reduce, then accept, error or skip
	       (stx (cond ;; state transition
		     ((assq-ref stxl tval)) ; shift/reduce in table
		     ((memq tval comm) (set! oact 'skip) other)
		     ((assq-ref stxl err)) ; error recovery
		     ((assq-ref stxl def))  ; default action
		     (else (set! oact 'error) other))))

	  (if debug (dmsg (car state) (if nval tval sval) stx))
	  (cond
	   ((shift? stx)
	    ;; We could check here to determine if next transition only has a
	    ;; default reduction and, if so, go ahead and process the reduction
	    ;; without reading another input token.  Needed for interactive.
	    (iter (cons (shift-to stx) state) (cons sval stack)
		  #f (if nval lval (lexr))))
	   ((reduce? stx)
	    (let* ((gx (reduce-pr stx)) (gl (vector-ref len-v gx))
		   ($$ (apply (vector-ref xact-v gx) stack)))
	      (iter (list-tail state gl) 
		    (list-tail stack gl)
		    (cons (vector-ref rto-v gx) $$)
		    lval)))
	   (else ;; other action: skip, error, or accept
	    (case oact
	      ((skip) (iter state stack nval (lexr)))
	      ((error) (throw 'nyacc-error
			      "parse failed at state ~A, on input ~S"
			      (car state) sval))
	      (else ;; accept
	       (car stack))))))))))

;;; --- last line ---
