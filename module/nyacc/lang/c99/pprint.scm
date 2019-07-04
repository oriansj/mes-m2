;;; nyacc/lang/c99/pprint.scm - C pretty-printer

;; Copyright (C) 2015-2018 Matthew R. Wette
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

;;; Code:

(define-module (nyacc lang c99 pprint)
  #:export (pretty-print-c99)
  #:use-module ((srfi srfi-1) #:select (pair-for-each))
  #:use-module (nyacc lang util)
  #:use-module (nyacc lang sx-util)
  #:use-module (ice-9 pretty-print)
  )
(cond-expand ;; for MES
 (guile-2 #t)
 (else
  (use-modules (ice-9 optargs))))

(define op-sym
  (let ((ot '(("=" . eq) ("+=" . pl-eq) ("-=" . mi-eq) ("*=" . ti-eq)
	      ("/=" . di-eq) ("%=" . mo-eq) ("<<=" . ls-eq) (">>=" . rs-eq)
	      ("&=" . ba-eq) ("^=" . bx-eq) ("|=" bo-eq))))
    (lambda (name)
      (assoc-ref ot name))))

(define op-prec
  ;; in order of decreasing precedence
  '((p-expr ident fixed float string)
    (comp-lit post-inc post-dec i-sel d-sel fctn-call array-ref)
    (de-ref ref-to neg pos not bitwise-not sizeof pre-inc pre-dec)
    (cast)
    (mul div mod)
    (add sub)
    (lshift rshift)
    (lt gt le ge)
    (eq ne)
    (bitwise-and)
    (bitwise-xor)
    (bitwise-or)
    (and)
    (or)
    (cond-expr)
    (assn-expr)
    (comma)))

(define op-assc
  '((left array-ref d-sel i-sel post-inc post-dec comp-lit mul div mod add sub
	  lshift rshift lt gt le ge bitwise-and bitwise-xor bitwise-or and or)
    (right pre-inc pre-dec sizeof bitwise-not not pos neg ref-to de-ref cast
	   cond assn-expr)
    (nonassoc)))

;; @deffn {Procedure} scmchs->c scm-chr-str => c-chr-str
;; Convert 1-char scheme string into 1-char C string constant as typed by user.
;; That is, exscaped.
;; @example
;; (scmchstr->c "#x00") => "\\0"
;; @end example
;; @end deffn
(define (scmchs->c scm-chr-str)
  (let ((ch (string-ref scm-chr-str 0)))
    (case ch
      ((#\nul) "\\0") ((#\bel) "\\a") ((#\bs) "\\b") ((#\ht) "\\t")
      ((#\nl) "\\n") ((#\vt) "\\v") ((#\np) "\\f") ((#\cr) "\\r") ((#\\) "\\")
      (else scm-chr-str))))

;; @deffn {Procedure} char->hex-list ch seed
;; to be documented
;; @end deffn
(define (char->hex-list ch seed)
  (define (itox ival) (string-ref "0123456789ABCDEF" ival))
  (let iter ((res seed) (ix 8) (val (char->integer ch)))
    (cond
     ((zero? ix) (cons* #\\ #\U res))
     ((and (zero? val) (= ix 4)) (cons* #\\ #\u res))
     (else
      (iter (cons (itox (remainder val 16)) res) (1- ix) (quotient val 16))))))

(define (esc->ch ch)
  (case ch ((#\nul) #\0) ((#\bel) #\a) ((#\bs) #\b) ((#\ht) #\t)
	((#\nl) #\n) ((#\vt) #\v) ((#\np) #\f) ((#\cr) #\r)))
   
;; @deffn {Procedure} scmstr->c str
;; to be documented
;; @end deffn
(define (scmstr->c str)
  (list->string
   (string-fold-right
    (lambda (ch chl)
      (cond
       ((char-set-contains? char-set:printing ch) (cons ch chl))
       ((char=? ch #\space) (cons #\space chl))
       ((memq ch '(#\nul #\bel #\bs #\ht #\nl #\vt #\np #\cr))
	(cons* #\\ (esc->ch ch) chl))
       (else (char->hex-list ch chl))))
    '() str)))
    
;;(define protect-expr? (make-protect-expr op-prec op-assc))

;; @deffn {Procedure} pretty-print-c99 tree [port] [options]
;; Convert and print a C99 sxml tree to the current output port.
;; The optional keyword argument @code{#:basic-offset} provides the
;; indent level, with default of 2.
;; @table code
;; @item #:basic-offset <n>
;; indent level for C code
;; @item #:per-line-prefix <string>
;; string
;; @item #:ugly #t|#f
;; pring ugly
;; @end table
;; @end deffn
(define* (pretty-print-c99 tree
			   #:optional (port (current-output-port))
			   #:key ugly per-line-prefix (basic-offset 2))

  (define fmtr
    ((if ugly make-pp-formatter/ugly make-pp-formatter)
     port #:per-line-prefix per-line-prefix #:basic-offset basic-offset))
  (define (push-il)(fmtr 'push))
  (define (pop-il) (fmtr 'pop))

  (define (sf . args) (apply fmtr args))

  (define (cpp-ppx tree)
    (fmtr 'nlin)
    (sx-match tree
      ((define (name ,name) (args . ,args) (repl ,repl))
       (sf "#define ~A(" name)
       (pair-for-each
	(lambda (pair) (sf "~A" (car pair)) (if (pair? (cdr pair)) (sf ",")))
	args)
       (sf ") ~A\n" repl))
      ((define (name ,name) (repl ,repl))
       (sf "#define ~A ~A\n" name repl))
      ((if ,text) (sf "#if ~A\n" text))
      ((elif ,text) (sf "#elif ~A\n" text))
      ((else ,text) (sf "#else ~A\n" text))
      ((else) (sf "#else\n"))
      ((endif ,text) (sf "#endif ~A\n" text))
      ((endif) (sf "#endif\n"))
      ((include ,file . *) (sf "#include ~A\n" file))
      ((error ,text) (sf "#error ~A\n" text))
      ((pragma ,text) (sf "#pragma ~A\n" text))
      (else (simple-format #t "\n*** pprint/cpp-ppx: NO MATCH: ~S\n" tree)))
    (fmtr 'nlin))

  (define protect-expr? (make-protect-expr op-prec op-assc))
  
  (define (unary/l op rep rval)
    (sf rep)
    (if (protect-expr? 'rt op rval)
	(ppx/p rval)
	(ppx rval)))
  
  (define (unary/r op rep lval)
    (if (protect-expr? 'lt op lval)
	(ppx/p lval)
	(ppx lval))
    (sf rep))
  
  (define (binary op rep lval rval)
    (if (protect-expr? 'lt op lval)
	(ppx/p lval)
	(ppx lval))
    (sf rep)
    (if (protect-expr? 'rt op rval)
	(ppx/p rval)
	(ppx rval)))

  (define (pp-attr attr) ;; attributes
    (string-join
     (map
      (lambda (val)
	;;(simple-format #t "a: ~S\n" val)
	(cond
	 ((eq? (car val) 'packed) "__packed__")
	 (else (symbol->string (car val)))))
      attr)
     " "))

  (define (struct-union-def struct-or-union attr name fields)
    (if name
	(if (pair? attr)
	    (sf "~A ~A ~A {\n" struct-or-union (pp-attr attr) name)
	    (sf "~A ~A {\n" struct-or-union name))
	(if (pair? attr)
	    (sf "~A ~A {\n" struct-or-union (pp-attr attr))
	    (sf "~A {\n" struct-or-union)))
    (push-il)
    (for-each ppx fields)
    (pop-il)
    (sf "}"))

  (define (ppx/p tree) (sf "(") (ppx tree) (sf ")"))

  ;; TODO: comp-lit
  (define (ppx-1 tree)
    (sx-match tree

      ((p-expr ,expr) (ppx expr))
      ((ident ,name) (sf "~A" name))
      ((char (@ . ,al) ,value)
       (let ((type (sx-attr-ref al 'type)))
	 (cond
	  ((not type) (sf "'~A'" (scmchs->c value)))
	  ((string=? type "wchar_t") (sf "L'~A'" (scmchs->c value)))
	  ((string=? type "char16_t") (sf "u'~A'" (scmchs->c value)))
	  ((string=? type "char32_t") (sf "U'~A'" (scmchs->c value)))
	  (else (throw 'c99-error "bad type")))))
      ((fixed ,value) (sf "~A" value))
      ((float ,value) (sf "~A" value))

      ((string . ,value-l)
       (pair-for-each
	(lambda (pair)
	  (sf "\"~A\"" (scmstr->c (car pair)))
	  (if (pair? (cdr pair)) (sf " ")))
	value-l))

      ((comment ,text)
       ;; Comments will look funny when indent for input and output
       ;; are different since multi-line comments will get hosed.
       (for-each (lambda (l) (sf (scmstr->c l)) (sf "\n"))
		 (string-split (string-append "/*" text "*/") #\newline))
       ;; TODO: Since parser now removes prefix, I need to add it back in here.
       ;; needed:
       ;; 1) (get-col) to get column
       ;; 2) (indent-to-col col) to do indents for each line
       #!
       (let ((col (get-col))
	     (lines (string-split text #\newline))
	     (ind (mk-ind-to-col col)))
	 (sf "/*") (sf (car lines))
	 (pair-for-each
	  (lambda (pair)
	    (sf "\n")
	    (if (pair? (cdr pair)) (sf ind))
	    (sf (cdr pair)))
	  (cdr lines))
	 (sf "*/"))
       !#
       )
      
      ((scope ,expr) (sf "(") (ppx expr) (sf ")"))
      
      ((array-ref ,dim ,expr)
       (ppx expr) (sf "[") (ppx dim) (sf "]"))

      ((d-sel ,id ,ex) (binary 'd-sel "." ex id))
      ((i-sel ,id ,ex) (binary 'i-sel "->" ex id))

      ((pre-inc ,expr) (unary/l 'pre-inc "++" expr))
      ((pre-dec ,expr) (unary/l 'pre-dec "--" expr))
      ((ref-to ,expr) (unary/l 'ref-to "&" expr))
      ((de-ref ,expr) (unary/l 'de-ref "*" expr))
      ((pos ,expr) (unary/l 'pos "+" expr))
      ((neg ,expr) (unary/l 'neg "-" expr))
      ((bitwise-not ,expr) (unary/l 'bitwise-not "~" expr))
      ((not ,expr) (unary/l 'not "!" expr))
      ((sizeof-expr ,expr) (sf "sizeof(") (ppx expr) (sf ")"))
      ((sizeof-type ,type) (sf "sizeof(") (ppx type) (sf ")"))

      ((cast ,tn ,ex)
       (sf "(") (ppx tn) (sf ")")
       (if (protect-expr? 'rt 'cast ex)
	   (ppx/p ex)
	   (ppx ex)))

      ((add ,lval ,rval) (binary 'add " + " lval rval))
      ((sub ,lval ,rval) (binary 'sub " - " lval rval))
      ((mul ,lval ,rval) (binary 'mul "*" lval rval))
      ((div ,lval ,rval) (binary 'div "/" lval rval))
      ((mod ,lval ,rval) (binary 'mod "%" lval rval))

      ((lshift ,lval ,rval) (binary 'lshift "<<" lval rval))
      ((rshift ,lval ,rval) (binary 'lshift "<<" lval rval))

      ((lt ,lval ,rval) (binary 'lt " < " lval rval))
      ((gt ,lval ,rval) (binary 'gt " > " lval rval))

      ((le ,lval ,rval) (binary 'le " <= " lval rval))
      ((ge ,lval ,rval) (binary 'ge " >= " lval rval))
      ((eq ,lval ,rval) (binary 'eq " == " lval rval))
      ((ne ,lval ,rval) (binary 'ne " != " lval rval))
      
      ((bitwise-and ,lval ,rval) (binary 'bitwise-and " & " lval rval))
      ((bitwise-or ,lval ,rval) (binary 'bitwise-and " | " lval rval))
      ((bitwise-xor ,lval ,rval) (binary 'bitwise-xor " ^ " lval rval))

      ((and ,lval ,rval) (binary 'and " && " lval rval))
      ((or ,lval ,rval) (binary 'and " || " lval rval))

      ;; CHECK THIS
      ((cond-expr ,cond ,tval ,fval)
       (ppx cond) (sf "? ") (ppx tval) (sf ": ") (ppx fval))

      ((post-inc ,expr) (unary/r 'post-inc "++" expr))
      ((post-dec ,expr) (unary/r 'post-dec "--" expr))

      ;; TODO: check protection 
      ((fctn-call ,expr ,arg-list)
       (if (protect-expr? 'rt 'fctn-call expr)
	   (ppx/p expr)
	   (ppx expr))
       (sf "(")
       (ppx arg-list)
       (sf ")"))

      ((expr-list . ,expr-l)
       (pair-for-each
	(lambda (pair)
	  (ppx (car pair))
	  (if (pair? (cdr pair)) (sf ", ")))
	expr-l))
      
      ((assn-expr ,lval ,op ,rval)
       (binary (car op) (simple-format #f " ~A " (cadr op)) lval rval))

      ;; TODO: check protection
      ((comma-expr . ,expr-list)
       (pair-for-each
	(lambda (pair)
	  (cond
	   ((pair? (cdr pair))
	    (if (protect-expr? 'rt 'comma-expr (car pair))
		(ppx/p (car pair))
		(ppx (car pair)))
	    (sf ", "))
	   (else (ppx (car pair)))))
	expr-list))

      ((udecl . ,rest)
       (ppx `(decl . ,rest)))
      ((decl ,decl-spec-list)
       (ppx decl-spec-list) (sf ";\n"))
      ((decl ,decl-spec-list ,init-declr-list)
       (ppx decl-spec-list) (sf " ") (ppx init-declr-list) (sf ";\n"))
      ((decl ,decl-spec-list ,init-declr-list ,comment)
       (ppx decl-spec-list) (sf " ")
       (ppx init-declr-list) (sf "; ") (ppx comment))
      ((decl-no-newline ,decl-spec-list ,init-declr-list) ; for (int i = 0;
       (ppx decl-spec-list) (sf " ") (ppx init-declr-list) (sf ";"))

      ((comp-decl ,spec-qual-list (comp-declr-list . ,rest2))
       (ppx spec-qual-list) (sf " ") (ppx (sx-ref tree 2)) (sf ";\n"))
      ((comp-decl ,spec-qual-list ,declr-list (comment ,comment))
       (ppx spec-qual-list) (sf " ") (ppx declr-list) (sf "; ")
       (ppx (sx-ref tree 3)))
      ;; anon struct or union
      ((comp-decl ,spec-qual-list) (ppx spec-qual-list) (sf ";\n"))
      ((comp-decl ,spec-qual-list (comment ,comment))
       (ppx spec-qual-list) (sf "; ") (ppx (sx-ref tree 2)))

      ((decl-spec-list . ,dsl)
       (pair-for-each
	(lambda (dsl)
	  (case (sx-tag (car dsl))
	    ((stor-spec) (sf "~A" (car (sx-ref (car dsl) 1))))
	    ((type-qual) (sf "~A" (sx-ref (car dsl) 1)))
	    ((fctn-spec) (sf "~A" (sx-ref (car dsl) 1)))
	    ((type-spec) (ppx (car dsl)))
	    (else (sf "[?:~S]" (car dsl))))
	  (if (pair? (cdr dsl)) (sf " ")))
	dsl))

      ((init-declr-list . ,rest)
       (pair-for-each
	(lambda (pair)
	  (ppx (car pair))
	  (if (pair? (cdr pair)) (sf ", ")))
	rest))
      ((comp-declr-list . ,rest)
       (pair-for-each
	(lambda (pair)
	  (ppx (car pair))
	  (if (pair? (cdr pair)) (sf ", ")))
	rest))

      ((init-declr ,declr ,initr) (ppx declr) (ppx initr))
      ((init-declr ,declr) (ppx declr))
      ((comp-declr ,declr) (ppx declr))
      ((param-declr ,declr) (ppx declr))
      ;; This should work with sx-match, to replace above three.
      ;;(((init-declr comp-declr param-declr) ,declr) (ppx declr))

      ((bit-field ,ident ,expr)
       (ppx ident) (sf " : ") (ppx expr))

      ;;((type-spec ,arg)
      ((type-spec (@ . ,aattr) ,arg)
       (case (sx-tag arg)
	 ((fixed-type) (sf "~A" (sx-ref arg 1)))
	 ((float-type) (sf "~A" (sx-ref arg 1)))
	 ((struct-ref) (ppx arg))
	 ((struct-def) (if (pair? aattr) (sf " ~S" (pp-attr aattr))) (ppx arg))
	 ((union-ref) (ppx arg))
	 ((union-def) (if (pair? aattr) (sf " ~S" (pp-attr aattr))) (ppx arg))
	 ((enum-ref) (ppx arg))
	 ((enum-def) (ppx arg))
	 ((typename) (sf "~A" (sx-ref arg 1)))
	 ((void) (sf "void"))
	 (else (error "missing " arg))))

      ((struct-ref (ident ,name)) (sf "struct ~A" name))
      ((union-ref (ident ,name)) (sf "union ~A" name))
      
      ((struct-def (@ . ,aattr) (ident ,name) (field-list . ,fields))
       (struct-union-def 'struct aattr name fields))
      ((struct-def (@ . ,aattr) (field-list . ,fields))
       (struct-union-def 'struct aattr #f fields))
      ((union-def (@ . ,aattr) (ident ,name) (field-list . ,fields))
       (struct-union-def 'union aattr name fields))
      ((union-def (@ . ,aattr) (field-list . ,fields))
       (struct-union-def 'union aattr #f fields))

      ((enum-ref (ident ,name))
       (sf "enum ~A" name))

      ((enum-def (ident ,name) (enum-def-list . ,edl))
       (sf "enum ~A " name) (ppx `(enum-def-list . ,edl))) ; SPACE ???

      ((enum-def (enum-def-list . ,edl))
       (sf "enum ") (ppx `(enum-def-list . ,edl))) ; SPACE ???

      ((enum-def-list . ,defns)
       (sf "{\n") (push-il)
       (for-each ppx defns)
       (pop-il) (sf "}"))

      ((enum-defn (ident ,name) (comment ,comment))
       (sf "~A, " name) (ppx `(comment ,comment)) (sf "\n"))
      ((enum-defn (ident ,name) ,expr ,comment)
       (sf "~A = " name) (ppx expr) (sf ", ") (ppx comment) (sf "\n"))
      ((enum-defn (ident ,name) ,expr)
       (sf "~A = " name) (ppx expr) (sf ",\n"))
      ((enum-defn (ident ,name))
       (sf "~A,\n" name))

      ;;((fctn-spec "inline")
      ((fctn-spec ,spec)
       (sf "~S " spec))			; SPACE ???

      ((ptr-declr ,ptr ,dir-declr)
       (ppx ptr) (ppx dir-declr))

      ((pointer) (sf "*"))
      ((pointer ,one) (sf "*") (ppx one))
      ((pointer ,one ,two) (sf "*") (ppx one) (ppx two))

      ((type-qual-list . ,tql)		; see decl-spec-list
       (pair-for-each
	(lambda (dsl)
	  (case (sx-tag (car dsl))
	    ((type-qual) (sf "~A" (sx-ref (car dsl) 1)))
	    (else (sf "[?:~S]" (car dsl))))
	  (if (pair? (cdr dsl)) (sf " ")))
	tql))

      ((array-of ,dir-declr ,arg)
       (ppx dir-declr) (sf "[") (ppx arg) (sf "]"))
      ((array-of ,dir-declr)
       (ppx dir-declr) (sf "[]"))
      ;; MORE TO GO
      
      ((ftn-declr ,dir-declr ,param-list)
       (ppx dir-declr) (sf "(") (ppx param-list) (sf ")"))

      ((type-name ,spec-qual-list ,abs-declr)
       (ppx spec-qual-list) (ppx abs-declr))
      ((type-name ,decl-spec-list)
       (ppx decl-spec-list))

      ((abs-declr ,pointer ,dir-abs-declr) (ppx pointer) (ppx dir-abs-declr))
      ((abs-declr ,one-of-above) (ppx one-of-above))

      ;; declr-scope
      ((declr-scope ,abs-declr)
       (sf "(") (ppx abs-declr) (sf ")"))
	
      ;; declr-array dir-abs-declr
      ;; declr-array dir-abs-declr assn-expr
      ;; declr-array dir-abs-declr type-qual-list
      ;; declr-array dir-abs-declr type-qual-list assn-expr
      ((declr-array ,dir-abs-declr)
       (ppx dir-abs-declr) (sf "[]"))
      ((declr-array ,dir-abs-declr ,arg2)
       (ppx dir-abs-declr) (sf "[") (ppx arg2) (sf "]"))
      ((declr-array ,dir-abs-declr ,arg2 ,arg3)
       (ppx dir-abs-declr) (sf "[") (ppx arg2) (sf " ") (ppx arg3) (sf "]"))
	
      ;; declr-anon-array
      ;; declr-STAR

      ;; abs-ftn-declr
      ((abs-ftn-declr ,dir-abs-declr ,param-type-list)
       (ppx dir-abs-declr) (sf "(") (ppx param-type-list) (sf ")"))
      ;; anon-ftn-declr

      ;; initializer
      ((initzer ,expr)
       (sf " = ") (ppx expr))
      
      ;; initializer-list
      ((initzer-list . ,items)
       (sf "{") ;; or "{ "
       (pair-for-each
	(lambda (pair)
	  (ppx (sx-ref (car pair) 1))
	  (if (pair? (cdr pair)) (sf ", ")))
	items)
       (sf "}")) ;; or " }"

      ((compd-stmt (block-item-list . ,items))
       (sf "{\n") (push-il) (for-each ppx items) (pop-il) (sf "}\n"))
      ((compd-stmt-no-newline (block-item-list . ,items))
       (sf "{\n") (push-il) (for-each ppx items) (pop-il) (sf "} "))
      
      ;; expression-statement
      ((expr-stmt) (sf ";\n"))
      ((expr-stmt ,expr) (ppx expr) (sf ";\n"))
      ((expr-stmt ,expr ,comm) (ppx expr) (sf "; ") (ppx comm))
      
      ((expr) (sf ""))		; for lone expr-stmt and return-stmt

      ;; selection-statement
      ((if . ,rest)
       (let ((cond-part (sx-ref tree 1))
	     (then-part (sx-ref tree 2)))
	 (sf "if (") (ppx cond-part) (sf ") ")
	 (ppx then-part)
	 (let iter ((else-l (sx-tail tree 3)))
	   (cond
	    ((null? else-l) #t)
	    ((eqv? 'else-if (caar else-l))
	     (sf "else if (") (ppx (sx-ref (car else-l) 1)) (sf ") ")
	     (ppx (sx-ref (car else-l) 2))
	     (iter (cdr else-l)))
	    (else
	     (sf "else ")
	     (ppx (car else-l)))))))

      ((switch ,expr (compd-stmt (block-item-list . ,items)))
       (sf "switch (") (ppx expr) (sf ") {\n")
       (for-each
	(lambda (item)
	  (unless (memq (car item) '(case default)) (push-il))
	  (ppx item)
	  (unless (memq (car item) '(case default)) (pop-il)))
	items)
       (sf "}\n"))

      ;; labeled-statement
      ((case ,expr ,stmt)
       (sf "case ") (ppx expr) (sf ":\n")
       (push-il) (ppx stmt) (pop-il))

      ((default ,stmt)
       (sf "default:\n")
       (push-il) (ppx stmt) (pop-il))

      ;; CHECK THIS
      ((while ,expr ,stmt)
       (sf "while (") (ppx expr) (sf ") ") (ppx stmt)
       )

      ;; This does not meet the convention of "} while" on same line. 
      ((do-while ,stmt ,expr)
       (sf "do ")
       (if (eqv? 'compd-stmt (sx-tag stmt)) 
	   (ppx `(compd-stmt-no-newline ,(sx-ref stmt 1)))
	   (ppx stmt))
       (sf "while (") (ppx expr) (sf ");\n"))
      
      ;; for
      ((for (decl . ,rest) ,test ,iter ,stmt)
       (sf "for (") (ppx `(decl-no-newline . ,rest))
       (sf " ") (ppx test) (sf "; ") (ppx iter) (sf ") ")
       (ppx stmt))

      ((for (decl . ,rest) ,expr2 ,expr3 ,stmt)
       (sf "for (")
       (ppx `(decl . ,rest)) (sf " ") (ppx expr2) (sf "; ") (ppx expr3)
       (sf ") ") (ppx stmt))
      ((for ,expr1 ,expr2 ,expr3 ,stmt)
       (sf "for (")
       (ppx expr1) (sf "; ") (ppx expr2) (sf "; ") (ppx expr3)
       (sf ") ") (ppx stmt))

      ;; jump-statement
      ((goto ,where)
       (pop-il)			; unindent
       (sf "goto ~A;" (sx-ref where 1))
       ;; comment?
       (sf "\n")
       (push-il))			; re-indent

      ((continue) (sf "continue;\n"))
      ((break) (sf "break;\n"))
      ((return ,expr) (sf "return ") (ppx expr) (sf ";\n"))
      ((return) (sf "return;\n"))

      ((trans-unit . ,items)
       (pair-for-each
	(lambda (pair)
	  (let ((this (car pair))
		(next (and (pair? (cdr pair)) (cadr pair))))
	    (ppx this)
	    (cond ;; add blank line if next is different or fctn defn
	     ((not next))
	     ((not (eqv? (sx-tag this) (sx-tag next))) (sf "\n"))
	     ((eqv? (sx-tag next) 'fctn-defn) (sf "\n")))))
	items))

      ((fctn-defn . ,rest) ;; but not yet (knr-fctn-defn)
       (let* ((decl-spec-list (sx-ref tree 1))
	      (declr (sx-ref tree 2))
	      (compd-stmt (sx-ref tree 3)))
	 (ppx decl-spec-list)
	 (sf " ")
	 (ppx declr)
	 (sf " ")
	 (ppx compd-stmt)))

      ((ptr-declr . ,rest)
       (ppx (sx-ref tree 1)) (ppx (sx-ref tree 2)))
      
      ((ftn-declr . ,rest)
       (ppx (sx-ref tree 1))	; direct-declarator
       (sf "(") (ppx (sx-ref tree 2)) (sf ")"))

      ((param-list . ,params)
       (pair-for-each
	(lambda (pair) (ppx (car pair)) (if (pair? (cdr pair)) (sf ", ")))
	params))

      ((ellipsis)	;; should work
       (sf "..."))

      ((param-decl ,decl-spec-list ,param-declr)
       (ppx decl-spec-list) (sf " ") (ppx param-declr))
      ((param-decl ,decl-spec-list)
       (ppx decl-spec-list))
      
      ((cpp-stmt . ,rest)
       (cpp-ppx (sx-ref tree 1)))

      ((extern-block ,begin ,guts ,end) (ppx begin) (ppx guts) (ppx end))
      ((extern-begin ,lang) (sf "extern \"~A\" {\n" lang))
      ((extern-end) (sf "}\n"))

      (else (simple-format #t "\n*** pprint/ppx: NO MATCH: ~S\n" (car tree)))))

  (define ppx ppx-1)

  (if (not (pair? tree)) (error "expecing sxml tree"))
  (ppx tree)
  (if ugly (newline)))

;; --- last line ---
