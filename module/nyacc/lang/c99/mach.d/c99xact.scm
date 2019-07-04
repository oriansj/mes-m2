;; ./mach.d/c99xact.scm

;; Copyright (C) 2016-2018 Matthew R. Wette
;; 
;; This library is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.
;; See the file COPYING.LESSER included with the this distribution.

(define c99x-act-v
  (vector
   ;; $start => expression
   (lambda ($1 . $rest) $1)
   ;; primary-expression => identifier
   (lambda ($1 . $rest) `(p-expr ,$1))
   ;; primary-expression => constant
   (lambda ($1 . $rest) `(p-expr ,$1))
   ;; primary-expression => string-literal
   (lambda ($1 . $rest) `(p-expr ,(tl->list $1)))
   ;; primary-expression => "(" expression ")"
   (lambda ($3 $2 $1 . $rest) $2)
   ;; primary-expression => "(" "{" block-item-list "}" ")"
   (lambda ($5 $4 $3 $2 $1 . $rest)
     `(stmt-expr (@ (extension "GNUC")) ,$3))
   ;; postfix-expression => primary-expression
   (lambda ($1 . $rest) $1)
   ;; postfix-expression => postfix-expression "[" expression "]"
   (lambda ($4 $3 $2 $1 . $rest)
     `(array-ref ,$3 ,$1))
   ;; postfix-expression => postfix-expression "(" argument-expression-list...
   (lambda ($4 $3 $2 $1 . $rest)
     `(fctn-call ,$1 ,(tl->list $3)))
   ;; postfix-expression => postfix-expression "(" ")"
   (lambda ($3 $2 $1 . $rest)
     `(fctn-call ,$1 (expr-list)))
   ;; postfix-expression => postfix-expression "." identifier
   (lambda ($3 $2 $1 . $rest) `(d-sel ,$3 ,$1))
   ;; postfix-expression => postfix-expression "->" identifier
   (lambda ($3 $2 $1 . $rest) `(i-sel ,$3 ,$1))
   ;; postfix-expression => postfix-expression "++"
   (lambda ($2 $1 . $rest) `(post-inc ,$1))
   ;; postfix-expression => postfix-expression "--"
   (lambda ($2 $1 . $rest) `(post-dec ,$1))
   ;; postfix-expression => "(" type-name ")" "{" initializer-list "}"
   (lambda ($6 $5 $4 $3 $2 $1 . $rest)
     `(comp-lit ,$2 ,(tl->list $5)))
   ;; postfix-expression => "(" type-name ")" "{" initializer-list "," "}"
   (lambda ($7 $6 $5 $4 $3 $2 $1 . $rest)
     `(comp-lit ,$2 ,(tl->list $5)))
   ;; argument-expression-list => assignment-expression
   (lambda ($1 . $rest) (make-tl 'expr-list $1))
   ;; argument-expression-list => argument-expression-list "," assignment-e...
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $3))
   ;; argument-expression-list => arg-expr-hack
   (lambda ($1 . $rest) (make-tl 'expr-list $1))
   ;; argument-expression-list => argument-expression-list "," arg-expr-hack
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $3))
   ;; arg-expr-hack => declaration-specifiers abstract-declarator
   (lambda ($2 $1 . $rest)
     `(param-decl ,(tl->list $1) $2))
   ;; arg-expr-hack => declaration-specifiers
   (lambda ($1 . $rest)
     `(param-decl ,(tl->list $1)))
   ;; unary-expression => postfix-expression
   (lambda ($1 . $rest) $1)
   ;; unary-expression => "++" unary-expression
   (lambda ($2 $1 . $rest) `(pre-inc ,$2))
   ;; unary-expression => "--" unary-expression
   (lambda ($2 $1 . $rest) `(pre-dec ,$2))
   ;; unary-expression => unary-operator cast-expression
   (lambda ($2 $1 . $rest) (list $1 $2))
   ;; unary-expression => "sizeof" unary-expression
   (lambda ($2 $1 . $rest) `(sizeof-expr ,$2))
   ;; unary-expression => "sizeof" "(" type-name ")"
   (lambda ($4 $3 $2 $1 . $rest) `(sizeof-type ,$3))
   ;; unary-operator => "&"
   (lambda ($1 . $rest) 'ref-to)
   ;; unary-operator => "*"
   (lambda ($1 . $rest) 'de-ref)
   ;; unary-operator => "+"
   (lambda ($1 . $rest) 'pos)
   ;; unary-operator => "-"
   (lambda ($1 . $rest) 'neg)
   ;; unary-operator => "~"
   (lambda ($1 . $rest) 'bitwise-not)
   ;; unary-operator => "!"
   (lambda ($1 . $rest) 'not)
   ;; cast-expression => unary-expression
   (lambda ($1 . $rest) $1)
   ;; cast-expression => "(" type-name ")" cast-expression
   (lambda ($4 $3 $2 $1 . $rest) `(cast ,$2 ,$4))
   ;; multiplicative-expression => cast-expression
   (lambda ($1 . $rest) $1)
   ;; multiplicative-expression => multiplicative-expression "*" cast-expre...
   (lambda ($3 $2 $1 . $rest) `(mul ,$1 ,$3))
   ;; multiplicative-expression => multiplicative-expression "/" cast-expre...
   (lambda ($3 $2 $1 . $rest) `(div ,$1 ,$3))
   ;; multiplicative-expression => multiplicative-expression "%" cast-expre...
   (lambda ($3 $2 $1 . $rest) `(mod ,$1 ,$3))
   ;; additive-expression => multiplicative-expression
   (lambda ($1 . $rest) $1)
   ;; additive-expression => additive-expression "+" multiplicative-expression
   (lambda ($3 $2 $1 . $rest) `(add ,$1 ,$3))
   ;; additive-expression => additive-expression "-" multiplicative-expression
   (lambda ($3 $2 $1 . $rest) `(sub ,$1 ,$3))
   ;; shift-expression => additive-expression
   (lambda ($1 . $rest) $1)
   ;; shift-expression => shift-expression "<<" additive-expression
   (lambda ($3 $2 $1 . $rest) `(lshift ,$1 ,$3))
   ;; shift-expression => shift-expression ">>" additive-expression
   (lambda ($3 $2 $1 . $rest) `(rshift ,$1 ,$3))
   ;; relational-expression => shift-expression
   (lambda ($1 . $rest) $1)
   ;; relational-expression => relational-expression "<" shift-expression
   (lambda ($3 $2 $1 . $rest) `(lt ,$1 ,$3))
   ;; relational-expression => relational-expression ">" shift-expression
   (lambda ($3 $2 $1 . $rest) `(gt ,$1 ,$3))
   ;; relational-expression => relational-expression "<=" shift-expression
   (lambda ($3 $2 $1 . $rest) `(le ,$1 ,$3))
   ;; relational-expression => relational-expression ">=" shift-expression
   (lambda ($3 $2 $1 . $rest) `(ge ,$1 ,$3))
   ;; equality-expression => relational-expression
   (lambda ($1 . $rest) $1)
   ;; equality-expression => equality-expression "==" relational-expression
   (lambda ($3 $2 $1 . $rest) `(eq ,$1 ,$3))
   ;; equality-expression => equality-expression "!=" relational-expression
   (lambda ($3 $2 $1 . $rest) `(ne ,$1 ,$3))
   ;; bitwise-and-expression => equality-expression
   (lambda ($1 . $rest) $1)
   ;; bitwise-and-expression => bitwise-and-expression "&" equality-expression
   (lambda ($3 $2 $1 . $rest)
     `(bitwise-and ,$1 ,$3))
   ;; bitwise-xor-expression => bitwise-and-expression
   (lambda ($1 . $rest) $1)
   ;; bitwise-xor-expression => bitwise-xor-expression "^" bitwise-and-expr...
   (lambda ($3 $2 $1 . $rest)
     `(bitwise-xor ,$1 ,$3))
   ;; bitwise-or-expression => bitwise-xor-expression
   (lambda ($1 . $rest) $1)
   ;; bitwise-or-expression => bitwise-or-expression "|" bitwise-xor-expres...
   (lambda ($3 $2 $1 . $rest) `(bitwise-or ,$1 ,$3))
   ;; logical-and-expression => bitwise-or-expression
   (lambda ($1 . $rest) $1)
   ;; logical-and-expression => logical-and-expression "&&" bitwise-or-expr...
   (lambda ($3 $2 $1 . $rest) `(and ,$1 ,$3))
   ;; logical-or-expression => logical-and-expression
   (lambda ($1 . $rest) $1)
   ;; logical-or-expression => logical-or-expression "||" logical-and-expre...
   (lambda ($3 $2 $1 . $rest) `(or ,$1 ,$3))
   ;; conditional-expression => logical-or-expression
   (lambda ($1 . $rest) $1)
   ;; conditional-expression => logical-or-expression "?" expression ":" co...
   (lambda ($5 $4 $3 $2 $1 . $rest)
     `(cond-expr ,$1 ,$3 ,$5))
   ;; assignment-expression => conditional-expression
   (lambda ($1 . $rest) $1)
   ;; assignment-expression => unary-expression assignment-operator assignm...
   (lambda ($3 $2 $1 . $rest)
     `(assn-expr ,$1 (op ,$2) ,$3))
   ;; assignment-operator => "="
   (lambda ($1 . $rest) $1)
   ;; assignment-operator => "+="
   (lambda ($1 . $rest) $1)
   ;; assignment-operator => "-="
   (lambda ($1 . $rest) $1)
   ;; assignment-operator => "*="
   (lambda ($1 . $rest) $1)
   ;; assignment-operator => "/="
   (lambda ($1 . $rest) $1)
   ;; assignment-operator => "%="
   (lambda ($1 . $rest) $1)
   ;; assignment-operator => "<<="
   (lambda ($1 . $rest) $1)
   ;; assignment-operator => ">>="
   (lambda ($1 . $rest) $1)
   ;; assignment-operator => "&="
   (lambda ($1 . $rest) $1)
   ;; assignment-operator => "^="
   (lambda ($1 . $rest) $1)
   ;; assignment-operator => "|="
   (lambda ($1 . $rest) $1)
   ;; expression => assignment-expression
   (lambda ($1 . $rest) $1)
   ;; expression => expression "," assignment-expression
   (lambda ($3 $2 $1 . $rest)
     (if (eqv? 'comma-expr (sx-tag $1))
       (append $1 (list $3))
       `(comma-expr ,$1 ,$3)))
   ;; constant-expression => conditional-expression
   (lambda ($1 . $rest) $1)
   ;; declaration => declaration-specifiers init-declarator-list opt-attrib...
   (lambda ($6 $5 $4 $3 $2 $1 . $rest)
     (if (pair? $3)
       (if (pair? $6)
         `(decl ,$3 ,(sx-ref $4 1) ,(sx-ref $4 2) ,$6)
         `(decl ,$3 ,(sx-ref $4 1) ,(sx-ref $4 2)))
       (if (pair? $6)
         `(decl ,(sx-ref $4 1) ,(sx-ref $4 2) ,$6)
         $4)))
   ;; declaration => declaration-specifiers ";" opt-code-comment
   (lambda ($3 $2 $1 . $rest)
     (if (pair? $3)
       `(decl ,(tl->list $1) ,(list $3))
       `(decl ,(tl->list $1))))
   ;; $P1 => 
   (lambda ($3 $2 $1 . $rest)
     (save-typenames
       `(decl ,(tl->list $1) ,(tl->list $2))))
   ;; declaration-specifiers => storage-class-specifier
   (lambda ($1 . $rest)
     (make-tl 'decl-spec-list $1))
   ;; declaration-specifiers => storage-class-specifier declaration-specifiers
   (lambda ($2 $1 . $rest) (tl-insert $2 $1))
   ;; declaration-specifiers => type-specifier
   (lambda ($1 . $rest)
     (make-tl 'decl-spec-list $1))
   ;; declaration-specifiers => type-specifier declaration-specifiers
   (lambda ($2 $1 . $rest) (tl-insert $2 $1))
   ;; declaration-specifiers => type-qualifier
   (lambda ($1 . $rest)
     (make-tl 'decl-spec-list $1))
   ;; declaration-specifiers => type-qualifier declaration-specifiers
   (lambda ($2 $1 . $rest) (tl-insert $2 $1))
   ;; declaration-specifiers => function-specifier
   (lambda ($1 . $rest)
     (make-tl 'decl-spec-list $1))
   ;; declaration-specifiers => function-specifier declaration-specifiers
   (lambda ($2 $1 . $rest) (tl-insert $2 $1))
   ;; init-declarator-list => init-declarator
   (lambda ($1 . $rest)
     (make-tl 'init-declr-list $1))
   ;; init-declarator-list => init-declarator-list "," init-declarator
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $3))
   ;; init-declarator => declarator
   (lambda ($1 . $rest) `(init-declr ,$1))
   ;; init-declarator => declarator asm-expression
   (lambda ($2 $1 . $rest) `(init-declr ,$1))
   ;; init-declarator => declarator "=" initializer
   (lambda ($3 $2 $1 . $rest) `(init-declr ,$1 ,$3))
   ;; init-declarator => declarator asm-expression "=" initializer
   (lambda ($4 $3 $2 $1 . $rest)
     `(init-declr ,$1 ,$4))
   ;; storage-class-specifier => "auto"
   (lambda ($1 . $rest) '(stor-spec (auto)))
   ;; storage-class-specifier => "extern"
   (lambda ($1 . $rest) '(stor-spec (extern)))
   ;; storage-class-specifier => "register"
   (lambda ($1 . $rest) '(stor-spec (register)))
   ;; storage-class-specifier => "static"
   (lambda ($1 . $rest) '(stor-spec (static)))
   ;; storage-class-specifier => "typedef"
   (lambda ($1 . $rest) '(stor-spec (typedef)))
   ;; type-specifier => "void"
   (lambda ($1 . $rest) '(type-spec (void)))
   ;; type-specifier => fixed-type-specifier
   (lambda ($1 . $rest) `(type-spec ,$1))
   ;; type-specifier => float-type-specifier
   (lambda ($1 . $rest) `(type-spec ,$1))
   ;; type-specifier => "_Bool"
   (lambda ($1 . $rest)
     '(type-spec (fixed-type "_Bool")))
   ;; type-specifier => complex-type-specifier
   (lambda ($1 . $rest) `(type-spec ,$1))
   ;; type-specifier => struct-or-union-specifier
   (lambda ($1 . $rest) `(type-spec ,$1))
   ;; type-specifier => enum-specifier
   (lambda ($1 . $rest) `(type-spec ,$1))
   ;; type-specifier => typedef-name
   (lambda ($1 . $rest) `(type-spec ,$1))
   ;; fixed-type-specifier => "short"
   (lambda ($1 . $rest) '(fixed-type "short"))
   ;; fixed-type-specifier => "short" "int"
   (lambda ($2 $1 . $rest)
     '(fixed-type "short int"))
   ;; fixed-type-specifier => "signed" "short"
   (lambda ($2 $1 . $rest)
     '(fixed-type "signed short"))
   ;; fixed-type-specifier => "signed" "short" "int"
   (lambda ($3 $2 $1 . $rest)
     '(fixed-type "signed short int"))
   ;; fixed-type-specifier => "int"
   (lambda ($1 . $rest) '(fixed-type "int"))
   ;; fixed-type-specifier => "signed"
   (lambda ($1 . $rest) '(fixed-type "signed"))
   ;; fixed-type-specifier => "signed" "int"
   (lambda ($2 $1 . $rest)
     '(fixed-type "signed int"))
   ;; fixed-type-specifier => "long"
   (lambda ($1 . $rest) '(fixed-type "long"))
   ;; fixed-type-specifier => "long" "int"
   (lambda ($2 $1 . $rest) '(fixed-type "long int"))
   ;; fixed-type-specifier => "signed" "long"
   (lambda ($2 $1 . $rest)
     '(fixed-type "signed long"))
   ;; fixed-type-specifier => "signed" "long" "int"
   (lambda ($3 $2 $1 . $rest)
     '(fixed-type "signed long int"))
   ;; fixed-type-specifier => "long" "long"
   (lambda ($2 $1 . $rest)
     '(fixed-type "long long"))
   ;; fixed-type-specifier => "long" "long" "int"
   (lambda ($3 $2 $1 . $rest)
     '(fixed-type "long long int"))
   ;; fixed-type-specifier => "signed" "long" "long"
   (lambda ($3 $2 $1 . $rest)
     '(fixed-type "signed long long"))
   ;; fixed-type-specifier => "signed" "long" "long" "int"
   (lambda ($4 $3 $2 $1 . $rest)
     '(fixed-type "signed long long int"))
   ;; fixed-type-specifier => "unsigned" "short" "int"
   (lambda ($3 $2 $1 . $rest)
     '(fixed-type "unsigned short int"))
   ;; fixed-type-specifier => "unsigned" "short"
   (lambda ($2 $1 . $rest)
     '(fixed-type "unsigned short"))
   ;; fixed-type-specifier => "unsigned" "int"
   (lambda ($2 $1 . $rest)
     '(fixed-type "unsigned int"))
   ;; fixed-type-specifier => "unsigned"
   (lambda ($1 . $rest) '(fixed-type "unsigned"))
   ;; fixed-type-specifier => "unsigned" "long" "int"
   (lambda ($3 $2 $1 . $rest)
     '(fixed-type "unsigned long"))
   ;; fixed-type-specifier => "unsigned" "long"
   (lambda ($2 $1 . $rest)
     '(fixed-type "unsigned long"))
   ;; fixed-type-specifier => "unsigned" "long" "long" "int"
   (lambda ($4 $3 $2 $1 . $rest)
     '(fixed-type "unsigned long long int"))
   ;; fixed-type-specifier => "unsigned" "long" "long"
   (lambda ($3 $2 $1 . $rest)
     '(fixed-type "unsigned long long"))
   ;; fixed-type-specifier => "char"
   (lambda ($1 . $rest) '(fixed-type "char"))
   ;; fixed-type-specifier => "signed" "char"
   (lambda ($2 $1 . $rest)
     '(fixed-type "signed char"))
   ;; fixed-type-specifier => "unsigned" "char"
   (lambda ($2 $1 . $rest)
     '(fixed-type "unsigned char"))
   ;; float-type-specifier => "float"
   (lambda ($1 . $rest) '(float-type "float"))
   ;; float-type-specifier => "double"
   (lambda ($1 . $rest) '(float-type "double"))
   ;; float-type-specifier => "long" "double"
   (lambda ($2 $1 . $rest)
     '(float-type "long double"))
   ;; complex-type-specifier => "_Complex"
   (lambda ($1 . $rest) '(complex-type "_Complex"))
   ;; complex-type-specifier => "float" "_Complex"
   (lambda ($2 $1 . $rest)
     '(complex-type "float _Complex"))
   ;; complex-type-specifier => "double" "_Complex"
   (lambda ($2 $1 . $rest)
     '(complex-type "double _Complex"))
   ;; complex-type-specifier => "long" "double" "_Complex"
   (lambda ($3 $2 $1 . $rest)
     '(complex-type "long double _Complex"))
   ;; attribute-specifier => attribute-specifier-single
   (lambda ($1 . $rest) $1)
   ;; attribute-specifier => attribute-specifier attribute-specifier-single
   (lambda ($2 $1 . $rest) (append $1 (cdr $2)))
   ;; attribute-specifier-single => "__attribute__" "(" "(" attribute-list ...
   (lambda ($6 $5 $4 $3 $2 $1 . $rest)
     (tl->list $4))
   ;; attribute-specifier-single => attr-name
   (lambda ($1 . $rest) `(attributes $1))
   ;; attr-name => "__packed__"
   (lambda ($1 . $rest) $1)
   ;; attr-name => "__aligned__"
   (lambda ($1 . $rest) $1)
   ;; attr-name => "__alignof__"
   (lambda ($1 . $rest) $1)
   ;; attr-ident => attr-name
   (lambda ($1 . $rest) $1)
   ;; attr-ident => '$ident
   (lambda ($1 . $rest) $1)
   ;; attribute-list => 
   (lambda $rest '(@))
   ;; attribute-list => attribute
   (lambda ($1 . $rest) (make-tl '@ $1))
   ;; attribute-list => attribute-list ","
   (lambda ($2 $1 . $rest) $1)
   ;; attribute-list => attribute-list "," attribute
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $3))
   ;; attribute => attr-ident
   (lambda ($1 . $rest)
     (list (string->symbol $1) ""))
   ;; attribute => attr-ident "(" attr-expr-list ")"
   (lambda ($4 $3 $2 $1 . $rest)
     (list (string->symbol $1)
           (attr-expr-list->string (tl->list $3))))
   ;; attr-expr-list => attribute-expr
   (lambda ($1 . $rest)
     (make-tl 'attr-expr-list $1))
   ;; attr-expr-list => attr-expr-list "," attribute-expr
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $3))
   ;; attr-call-name => '$ident
   (lambda ($1 . $rest) $1)
   ;; attr-call-name => attr-name
   (lambda ($1 . $rest) $1)
   ;; attribute-expr => type-name
   (lambda ($1 . $rest) (sx-ref* $1 1 1 1 1))
   ;; attribute-expr => '$fixed
   (lambda ($1 . $rest) $1)
   ;; attribute-expr => '$string
   (lambda ($1 . $rest) $1)
   ;; attribute-expr => attr-call-name
   (lambda ($1 . $rest) $1)
   ;; attribute-expr => attr-call-name "(" attr-expr-list ")"
   (lambda ($4 $3 $2 $1 . $rest)
     (string-append
       $1
       (attr-expr-list->string (tl->list $3))))
   ;; opt-attribute-specifier => 
   (lambda $rest (list))
   ;; opt-attribute-specifier => attribute-specifier
   (lambda ($1 . $rest) $1)
   ;; struct-or-union-specifier => "struct" ident-like "{" struct-declarati...
   (lambda ($6 $5 $4 $3 $2 $1 . $rest)
     (if (pair? $6)
       `(struct-def ,$6 ,$2 ,(tl->list $4))
       `(struct-def ,$2 ,(tl->list $4))))
   ;; struct-or-union-specifier => "struct" "{" struct-declaration-list "}"...
   (lambda ($5 $4 $3 $2 $1 . $rest)
     (if (pair? $5)
       `(struct-def ,$5 ,(tl->list $3))
       `(struct-def ,(tl->list $3))))
   ;; struct-or-union-specifier => "struct" ident-like
   (lambda ($2 $1 . $rest) `(struct-ref ,$2))
   ;; struct-or-union-specifier => "union" ident-like "{" struct-declaratio...
   (lambda ($6 $5 $4 $3 $2 $1 . $rest)
     (if (pair? $6)
       `(union-def ,$6 ,$2 ,(tl->list $4))
       `(union-def ,$2 ,(tl->list $4))))
   ;; struct-or-union-specifier => "union" "{" struct-declaration-list "}" ...
   (lambda ($5 $4 $3 $2 $1 . $rest)
     (if (pair? $5)
       `(union-def ,$5 ,(tl->list $3))
       `(union-def ,(tl->list $3))))
   ;; struct-or-union-specifier => "union" ident-like
   (lambda ($2 $1 . $rest) `(union-ref ,$2))
   ;; struct-or-union-specifier => "struct" attribute-specifier ident-like ...
   (lambda ($6 $5 $4 $3 $2 $1 . $rest)
     `(struct-def ,$2 ,(tl->list $4)))
   ;; struct-or-union-specifier => "struct" attribute-specifier "{" struct-...
   (lambda ($5 $4 $3 $2 $1 . $rest)
     `(struct-def ,(tl->list $3)))
   ;; struct-or-union-specifier => "union" attribute-specifier ident-like "...
   (lambda ($6 $5 $4 $3 $2 $1 . $rest)
     `(union-def ,$2 ,(tl->list $4)))
   ;; struct-or-union-specifier => "union" attribute-specifier "{" struct-d...
   (lambda ($5 $4 $3 $2 $1 . $rest)
     `(union-def ,(tl->list $3)))
   ;; ident-like => identifier
   (lambda ($1 . $rest) $1)
   ;; ident-like => typedef-name
   (lambda ($1 . $rest) `(ident ,(sx-ref $1 1)))
   ;; struct-declaration-list => struct-declaration
   (lambda ($1 . $rest) (make-tl 'field-list $1))
   ;; struct-declaration-list => lone-comment
   (lambda ($1 . $rest) (make-tl 'field-list $1))
   ;; struct-declaration-list => struct-declaration-list struct-declaration
   (lambda ($2 $1 . $rest) (tl-append $1 $2))
   ;; struct-declaration-list => struct-declaration-list lone-comment
   (lambda ($2 $1 . $rest) (tl-append $1 $2))
   ;; struct-declaration-list => ";"
   (lambda ($1 . $rest) (make-tl 'field-list))
   ;; struct-declaration-list => struct-declaration-list ";"
   (lambda ($2 $1 . $rest) $1)
   ;; struct-declaration => specifier-qualifier-list struct-declarator-list...
   (lambda ($5 $4 $3 $2 $1 . $rest)
     (if (pair? $3)
       (if (pair? $5)
         `(comp-decl ,$3 ,(tl->list $1) ,(tl->list $2) ,$5)
         `(comp-decl ,$3 ,(tl->list $1) ,(tl->list $2)))
       (if (pair? $5)
         `(comp-decl ,(tl->list $1) ,(tl->list $2) ,$5)
         `(comp-decl ,(tl->list $1) ,(tl->list $2)))))
   ;; struct-declaration => specifier-qualifier-list ";" opt-code-comment
   (lambda ($3 $2 $1 . $rest)
     (if (pair? $3)
       `(comp-decl ,(tl->list $1) ,$3)
       `(comp-decl ,(tl->list $1))))
   ;; specifier-qualifier-list => type-specifier specifier-qualifier-list
   (lambda ($2 $1 . $rest) (tl-insert $2 $1))
   ;; specifier-qualifier-list => type-specifier
   (lambda ($1 . $rest)
     (make-tl 'decl-spec-list $1))
   ;; specifier-qualifier-list => type-qualifier specifier-qualifier-list
   (lambda ($2 $1 . $rest) (tl-insert $2 $1))
   ;; specifier-qualifier-list => type-qualifier
   (lambda ($1 . $rest)
     (make-tl 'decl-spec-list $1))
   ;; struct-declarator-list => struct-declarator
   (lambda ($1 . $rest)
     (make-tl 'comp-declr-list $1))
   ;; struct-declarator-list => struct-declarator-list "," struct-declarator
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $3))
   ;; struct-declarator => declarator
   (lambda ($1 . $rest) `(comp-declr ,$1))
   ;; struct-declarator => declarator ":" constant-expression
   (lambda ($3 $2 $1 . $rest)
     `(comp-declr (bit-field ,$1 ,$3)))
   ;; struct-declarator => ":" constant-expression
   (lambda ($2 $1 . $rest)
     `(comp-declr (bit-field ,$2)))
   ;; enum-specifier => "enum" ident-like "{" enumerator-list "}"
   (lambda ($5 $4 $3 $2 $1 . $rest)
     `(enum-def ,$2 ,(tl->list $4)))
   ;; enum-specifier => "enum" ident-like "{" enumerator-list "," "}"
   (lambda ($6 $5 $4 $3 $2 $1 . $rest)
     `(enum-def ,$2 ,(tl->list $4)))
   ;; enum-specifier => "enum" "{" enumerator-list "}"
   (lambda ($4 $3 $2 $1 . $rest)
     `(enum-def ,(tl->list $3)))
   ;; enum-specifier => "enum" "{" enumerator-list "," "}"
   (lambda ($5 $4 $3 $2 $1 . $rest)
     `(enum-def ,(tl->list $3)))
   ;; enum-specifier => "enum" ident-like
   (lambda ($2 $1 . $rest) `(enum-ref ,$2))
   ;; enumerator-list => enumerator
   (lambda ($1 . $rest) (make-tl 'enum-def-list $1))
   ;; enumerator-list => enumerator-list "," enumerator
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $3))
   ;; enumerator => identifier
   (lambda ($1 . $rest) `(enum-defn ,$1))
   ;; enumerator => identifier "=" constant-expression
   (lambda ($3 $2 $1 . $rest) `(enum-defn ,$1 ,$3))
   ;; type-qualifier => "const"
   (lambda ($1 . $rest) `(type-qual ,$1))
   ;; type-qualifier => "volatile"
   (lambda ($1 . $rest) `(type-qual ,$1))
   ;; type-qualifier => "restrict"
   (lambda ($1 . $rest) `(type-qual ,$1))
   ;; function-specifier => "inline"
   (lambda ($1 . $rest) `(fctn-spec ,$1))
   ;; function-specifier => "_Noreturn"
   (lambda ($1 . $rest) `(fctn-spec ,$1))
   ;; declarator => pointer direct-declarator
   (lambda ($2 $1 . $rest) `(ptr-declr ,$1 ,$2))
   ;; declarator => direct-declarator
   (lambda ($1 . $rest) $1)
   ;; direct-declarator => identifier
   (lambda ($1 . $rest) $1)
   ;; direct-declarator => "(" declarator ")"
   (lambda ($3 $2 $1 . $rest) `(scope ,$2))
   ;; direct-declarator => direct-declarator "[" type-qualifier-list assign...
   (lambda ($5 $4 $3 $2 $1 . $rest)
     `(array-of ,$1 ,$3 ,$4))
   ;; direct-declarator => direct-declarator "[" type-qualifier-list "]"
   (lambda ($4 $3 $2 $1 . $rest)
     `(array-of ,$1 ,$3))
   ;; direct-declarator => direct-declarator "[" assignment-expression "]"
   (lambda ($4 $3 $2 $1 . $rest)
     `(array-of ,$1 ,$3))
   ;; direct-declarator => direct-declarator "[" "]"
   (lambda ($3 $2 $1 . $rest) `(array-of ,$1))
   ;; direct-declarator => direct-declarator "[" "static" type-qualifier-li...
   (lambda ($6 $5 $4 $3 $2 $1 . $rest)
     `(array-of
        ,$1
        ,(tl->list (tl-insert '(stor-spec "static") $4))
        ,$5))
   ;; direct-declarator => direct-declarator "[" type-qualifier-list "stati...
   (lambda ($6 $5 $4 $3 $2 $1 . $rest)
     `(array-of
        ,$1
        ,(tl->list (tl-insert '(stor-spec "static") $3))
        ,$5))
   ;; direct-declarator => direct-declarator "[" type-qualifier-list "*" "]"
   (lambda ($5 $4 $3 $2 $1 . $rest)
     `(array-of ,$1 ,$3 (var-len)))
   ;; direct-declarator => direct-declarator "[" "*" "]"
   (lambda ($4 $3 $2 $1 . $rest)
     `(array-of ,$1 (var-len)))
   ;; direct-declarator => direct-declarator "(" parameter-type-list ")"
   (lambda ($4 $3 $2 $1 . $rest)
     `(ftn-declr ,$1 ,(tl->list $3)))
   ;; direct-declarator => direct-declarator "(" identifier-list ")"
   (lambda ($4 $3 $2 $1 . $rest)
     `(ftn-declr ,$1 ,(tl->list $3)))
   ;; direct-declarator => direct-declarator "(" ")"
   (lambda ($3 $2 $1 . $rest)
     `(ftn-declr ,$1 (param-list)))
   ;; pointer => "*" type-qualifier-list pointer
   (lambda ($3 $2 $1 . $rest)
     `(pointer ,(tl->list $2) ,$3))
   ;; pointer => "*" type-qualifier-list
   (lambda ($2 $1 . $rest)
     `(pointer ,(tl->list $2)))
   ;; pointer => "*" pointer
   (lambda ($2 $1 . $rest) `(pointer ,$2))
   ;; pointer => "*"
   (lambda ($1 . $rest) '(pointer))
   ;; type-qualifier-list => type-qualifier
   (lambda ($1 . $rest)
     (make-tl 'type-qual-list $1))
   ;; type-qualifier-list => type-qualifier-list type-qualifier
   (lambda ($2 $1 . $rest) (tl-append $1 $2))
   ;; parameter-type-list => parameter-list
   (lambda ($1 . $rest) $1)
   ;; parameter-type-list => parameter-list "," "..."
   (lambda ($3 $2 $1 . $rest)
     (tl-append $1 '(ellipsis)))
   ;; parameter-list => parameter-declaration
   (lambda ($1 . $rest) (make-tl 'param-list $1))
   ;; parameter-list => parameter-list "," parameter-declaration
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $3))
   ;; parameter-declaration => declaration-specifiers declarator
   (lambda ($2 $1 . $rest)
     `(param-decl ,(tl->list $1) (param-declr ,$2)))
   ;; parameter-declaration => declaration-specifiers abstract-declarator
   (lambda ($2 $1 . $rest)
     `(param-decl ,(tl->list $1) (param-declr ,$2)))
   ;; parameter-declaration => declaration-specifiers
   (lambda ($1 . $rest)
     `(param-decl ,(tl->list $1)))
   ;; identifier-list => identifier
   (lambda ($1 . $rest) (make-tl 'ident-list $1))
   ;; identifier-list => identifier-list "," identifier
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $3))
   ;; type-name => specifier-qualifier-list abstract-declarator
   (lambda ($2 $1 . $rest)
     `(type-name ,(tl->list $1) ,$2))
   ;; type-name => declaration-specifiers
   (lambda ($1 . $rest) `(type-name ,(tl->list $1)))
   ;; abstract-declarator => pointer direct-abstract-declarator
   (lambda ($2 $1 . $rest) `(abs-declr ,$1 ,$2))
   ;; abstract-declarator => pointer
   (lambda ($1 . $rest) `(abs-declr ,$1))
   ;; abstract-declarator => direct-abstract-declarator
   (lambda ($1 . $rest) `(abs-declr ,$1))
   ;; direct-abstract-declarator => "(" abstract-declarator ")"
   (lambda ($3 $2 $1 . $rest) `(declr-scope ,$2))
   ;; direct-abstract-declarator => direct-abstract-declarator "[" type-qua...
   (lambda ($5 $4 $3 $2 $1 . $rest)
     `(declr-array ,$1 ,$3 ,$4))
   ;; direct-abstract-declarator => direct-abstract-declarator "[" type-qua...
   (lambda ($4 $3 $2 $1 . $rest)
     `(declr-array ,$1 ,$3))
   ;; direct-abstract-declarator => direct-abstract-declarator "[" assignme...
   (lambda ($4 $3 $2 $1 . $rest)
     `(declr-array ,$1 ,$3))
   ;; direct-abstract-declarator => direct-abstract-declarator "[" "]"
   (lambda ($3 $2 $1 . $rest) `(declr-array ,$1))
   ;; direct-abstract-declarator => direct-abstract-declarator "[" "static"...
   (lambda ($6 $5 $4 $3 $2 $1 . $rest)
     `(declr-array
        ,$1
        ,(tl->list (tl-insert '(stor-spec "static") $4))
        ,$5))
   ;; direct-abstract-declarator => direct-abstract-declarator "[" "static"...
   (lambda ($5 $4 $3 $2 $1 . $rest)
     `(declr-array
        ,$1
        ,(tl->list (tl-insert '(stor-spec "static") $4))))
   ;; direct-abstract-declarator => direct-abstract-declarator "[" type-qua...
   (lambda ($6 $5 $4 $3 $2 $1 . $rest)
     `(declr-array
        ,$1
        ,(tl->list (tl-insert '(stor-spec "static") $3))
        ,$5))
   ;; direct-abstract-declarator => "[" type-qualifier-list assignment-expr...
   (lambda ($4 $3 $2 $1 . $rest)
     `(declr-anon-array ,$2 ,$3))
   ;; direct-abstract-declarator => "[" type-qualifier-list "]"
   (lambda ($3 $2 $1 . $rest)
     `(declr-anon-array ,$2))
   ;; direct-abstract-declarator => "[" assignment-expression "]"
   (lambda ($3 $2 $1 . $rest)
     `(declr-anon-array ,$2))
   ;; direct-abstract-declarator => "[" "]"
   (lambda ($2 $1 . $rest) `(declr-anon-array))
   ;; direct-abstract-declarator => "[" "static" type-qualifier-list assign...
   (lambda ($5 $4 $3 $2 $1 . $rest)
     `(declr-anon-array
        ,(tl->list (tl-insert '(stor-spec "static") $3))
        ,$4))
   ;; direct-abstract-declarator => "[" "static" type-qualifier-list "]"
   (lambda ($4 $3 $2 $1 . $rest)
     `(declr-anon-array
        ,(tl->list (tl-insert '(stor-spec "static") $3))))
   ;; direct-abstract-declarator => "[" type-qualifier-list "static" assign...
   (lambda ($5 $4 $3 $2 $1 . $rest)
     `(declr-anon-array
        ,(tl->list (tl-insert '(stor-spec "static") $2))
        ,$4))
   ;; direct-abstract-declarator => direct-abstract-declarator "[" "*" "]"
   (lambda ($4 $3 $2 $1 . $rest) `(declr-star ,$1))
   ;; direct-abstract-declarator => "[" "*" "]"
   (lambda ($3 $2 $1 . $rest) '(declr-star))
   ;; direct-abstract-declarator => direct-abstract-declarator "(" paramete...
   (lambda ($4 $3 $2 $1 . $rest)
     `(abs-ftn-declr ,$1 ,(tl->list $3)))
   ;; direct-abstract-declarator => direct-abstract-declarator "(" ")"
   (lambda ($3 $2 $1 . $rest) `(abs-ftn-declr ,$1))
   ;; direct-abstract-declarator => "(" parameter-type-list ")"
   (lambda ($3 $2 $1 . $rest)
     `(anon-ftn-declr ,(tl->list $2)))
   ;; direct-abstract-declarator => "(" ")"
   (lambda ($2 $1 . $rest) '(anon-ftn-declr))
   ;; typedef-name => 'typename
   (lambda ($1 . $rest) `(typename ,$1))
   ;; initializer => assignment-expression
   (lambda ($1 . $rest) `(initzer ,$1))
   ;; initializer => "{" initializer-list "}"
   (lambda ($3 $2 $1 . $rest)
     `(initzer ,(tl->list $2)))
   ;; initializer => "{" initializer-list "," "}"
   (lambda ($4 $3 $2 $1 . $rest)
     `(initzer ,(tl->list $2)))
   ;; initializer-list => designation initializer
   (lambda ($2 $1 . $rest)
     (make-tl 'initzer-list $1 $2))
   ;; initializer-list => initializer
   (lambda ($1 . $rest) (make-tl 'initzer-list $1))
   ;; initializer-list => initializer-list "," designation initializer
   (lambda ($4 $3 $2 $1 . $rest)
     (tl-append $1 $3 $4))
   ;; initializer-list => initializer-list "," initializer
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $3))
   ;; designation => designator-list "="
   (lambda ($2 $1 . $rest) `(desig ,$1))
   ;; designator-list => designator
   (lambda ($1 . $rest) (make-tl 'desgr-list $1))
   ;; designator-list => designator-list designator
   (lambda ($2 $1 . $rest) (tl-append $1 $2))
   ;; designator => "[" constant-expression "]"
   (lambda ($3 $2 $1 . $rest) `(array-dsgr ,$2))
   ;; designator => "." identifier
   (lambda ($2 $1 . $rest) `(sel-dsgr ,$2))
   ;; statement => labeled-statement
   (lambda ($1 . $rest) $1)
   ;; statement => compound-statement
   (lambda ($1 . $rest) $1)
   ;; statement => expression-statement
   (lambda ($1 . $rest) $1)
   ;; statement => selection-statement
   (lambda ($1 . $rest) $1)
   ;; statement => iteration-statement
   (lambda ($1 . $rest) $1)
   ;; statement => jump-statement
   (lambda ($1 . $rest) $1)
   ;; statement => asm-statement
   (lambda ($1 . $rest) $1)
   ;; statement => cpp-statement
   (lambda ($1 . $rest) $1)
   ;; labeled-statement => identifier ":" statement
   (lambda ($3 $2 $1 . $rest)
     `(labeled-stmt ,$1 ,$3))
   ;; labeled-statement => "case" constant-expression ":" statement
   (lambda ($4 $3 $2 $1 . $rest) `(case ,$2 ,$4))
   ;; labeled-statement => "default" ":" statement
   (lambda ($3 $2 $1 . $rest) `(default ,$3))
   ;; compound-statement => "{" block-item-list "}"
   (lambda ($3 $2 $1 . $rest)
     `(compd-stmt ,(tl->list $2)))
   ;; compound-statement => "{" "}"
   (lambda ($2 $1 . $rest)
     `(compd-stmt (block-item-list)))
   ;; block-item-list => block-item
   (lambda ($1 . $rest)
     (make-tl 'block-item-list $1))
   ;; block-item-list => block-item-list block-item
   (lambda ($2 $1 . $rest) (tl-append $1 $2))
   ;; block-item => declaration
   (lambda ($1 . $rest) $1)
   ;; block-item => statement
   (lambda ($1 . $rest) $1)
   ;; expression-statement => expression ";"
   (lambda ($2 $1 . $rest) `(expr-stmt ,$1))
   ;; expression-statement => ";"
   (lambda ($1 . $rest) '(expr-stmt))
   ;; selection-statement => "if" "(" expression ")" statement
   (lambda ($5 $4 $3 $2 $1 . $rest) `(if ,$3 ,$5))
   ;; selection-statement => "if" "(" expression ")" statement "else" state...
   (lambda ($7 $6 $5 $4 $3 $2 $1 . $rest)
     `(if ,$3 ,$5 ,$7))
   ;; selection-statement => "switch" "(" expression ")" statement
   (lambda ($5 $4 $3 $2 $1 . $rest)
     `(switch ,$3 ,$5))
   ;; iteration-statement => "while" "(" expression ")" statement
   (lambda ($5 $4 $3 $2 $1 . $rest)
     `(while ,$3 ,$5))
   ;; iteration-statement => "do" statement "while" "(" expression ")" ";"
   (lambda ($7 $6 $5 $4 $3 $2 $1 . $rest)
     `(do-while ,$2 ,$5))
   ;; iteration-statement => "for" "(" initial-clause opt-expression ";" op...
   (lambda ($8 $7 $6 $5 $4 $3 $2 $1 . $rest)
     `(for ,$3 ,$4 ,$6 ,$8))
   ;; initial-clause => expression ";"
   (lambda ($2 $1 . $rest) $1)
   ;; initial-clause => ";"
   (lambda ($1 . $rest) '(expr))
   ;; initial-clause => declaration
   (lambda ($1 . $rest) $1)
   ;; opt-expression => 
   (lambda $rest '(expr))
   ;; opt-expression => expression
   (lambda ($1 . $rest) $1)
   ;; jump-statement => "goto" identifier ";"
   (lambda ($3 $2 $1 . $rest) `(goto ,$2))
   ;; jump-statement => "continue" ";"
   (lambda ($2 $1 . $rest) '(continue))
   ;; jump-statement => "break" ";"
   (lambda ($2 $1 . $rest) '(break))
   ;; jump-statement => "return" expression ";"
   (lambda ($3 $2 $1 . $rest) `(return ,$2))
   ;; jump-statement => "return" ";"
   (lambda ($2 $1 . $rest) `(return (expr)))
   ;; asm-statement => asm-expression ";"
   (lambda ($2 $1 . $rest) $1)
   ;; asm-expression => "asm" "(" string-literal ")"
   (lambda ($4 $3 $2 $1 . $rest)
     `(asm-expr (@ (extension "GNUC")) ,$3))
   ;; asm-expression => "asm" "(" string-literal asm-outputs ")"
   (lambda ($5 $4 $3 $2 $1 . $rest)
     `(asm-expr
        (@ (extension "GNUC"))
        ,$3
        ,(tl->list $4)))
   ;; asm-expression => "asm" "(" string-literal asm-outputs asm-inputs ")"
   (lambda ($6 $5 $4 $3 $2 $1 . $rest)
     `(asm-expr
        (@ (extension "GNUC"))
        ,$3
        ,(tl->list $4)
        ,(tl->list $5)))
   ;; asm-expression => "asm" "(" string-literal asm-outputs asm-inputs asm...
   (lambda ($7 $6 $5 $4 $3 $2 $1 . $rest)
     `(asm-expr
        (@ (extension "GNUC"))
        ,$3
        ,(tl->list $4)
        ,(tl->list $5)
        ,(tl->list $6)))
   ;; asm-outputs => ":"
   (lambda ($1 . $rest) (make-tl 'asm-outputs))
   ;; asm-outputs => ":" asm-output
   (lambda ($2 $1 . $rest)
     (make-tl 'asm-outputs $2))
   ;; asm-outputs => asm-outputs "," asm-output
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $3))
   ;; asm-output => string-literal "(" identifier ")"
   (lambda ($4 $3 $2 $1 . $rest)
     `(asm-operand ,$1 ,$3))
   ;; asm-output => "[" identifier "]" string-literal "(" identifier ")"
   (lambda ($7 $6 $5 $4 $3 $2 $1 . $rest)
     `(asm-operand ,$2 ,$4 ,$6))
   ;; asm-inputs => ":"
   (lambda ($1 . $rest) (make-tl 'asm-inputs))
   ;; asm-inputs => ":" asm-input
   (lambda ($2 $1 . $rest) (make-tl 'asm-inputs $2))
   ;; asm-inputs => asm-inputs "," asm-input
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $3))
   ;; asm-input => string-literal "(" expression ")"
   (lambda ($4 $3 $2 $1 . $rest)
     `(asm-operand ,$1 ,$3))
   ;; asm-input => "[" identifier "]" string-literal "(" expression ")"
   (lambda ($7 $6 $5 $4 $3 $2 $1 . $rest)
     `(asm-operand ,$2 ,$4 ,$6))
   ;; asm-clobbers => ":"
   (lambda ($1 . $rest) (make-tl 'asm-clobbers))
   ;; asm-clobbers => ":" string-literal
   (lambda ($2 $1 . $rest)
     (make-tl 'asm-clobbers $2))
   ;; asm-clobbers => asm-clobbers "," string-literal
   (lambda ($3 $2 $1 . $rest) (tl-append $1 $3))
   ;; translation-unit => external-declaration-list
   (lambda ($1 . $rest) (tl->list $1))
   ;; external-declaration-list => 
   (lambda $rest (make-tl 'trans-unit))
   ;; external-declaration-list => external-declaration-list external-decla...
   (lambda ($2 $1 . $rest)
     (if (eqv? (sx-tag $2) 'extern-block)
       (tl-extend $1 (sx-tail $2 1))
       (tl-append $1 $2)))
   ;; external-declaration => function-definition
   (lambda ($1 . $rest) $1)
   ;; external-declaration => declaration
   (lambda ($1 . $rest) $1)
   ;; external-declaration => attribute-specifier declaration
   (lambda ($2 $1 . $rest) $2)
   ;; external-declaration => lone-comment
   (lambda ($1 . $rest) $1)
   ;; external-declaration => cpp-statement
   (lambda ($1 . $rest) $1)
   ;; external-declaration => pragma
   (lambda ($1 . $rest) $1)
   ;; external-declaration => "extern" '$string "{" $P2 external-declaratio...
   (lambda ($7 $6 $5 $4 $3 $2 $1 . $rest)
     `(extern-block
        (extern-begin ,$2)
        ,@(sx-tail (tl->list $5) 1)
        (extern-end)))
   ;; external-declaration => ";"
   (lambda ($1 . $rest)
     `(decl (@ (extension "GNUC"))))
   ;; $P2 => 
   (lambda ($3 $2 $1 . $rest) (cpi-dec-blev!))
   ;; $P3 => 
   (lambda ($5 $4 $3 $2 $1 . $rest) (cpi-inc-blev!))
   ;; function-definition => declaration-specifiers declarator declaration-...
   (lambda ($4 $3 $2 $1 . $rest)
     `(knr-fctn-defn
        ,(tl->list $1)
        ,$2
        ,(tl->list $3)
        ,$4))
   ;; function-definition => declaration-specifiers declarator compound-sta...
   (lambda ($3 $2 $1 . $rest)
     `(fctn-defn ,(tl->list $1) ,$2 ,$3))
   ;; declaration-list => declaration
   (lambda ($1 . $rest) (make-tl $1))
   ;; declaration-list => declaration-list declaration
   (lambda ($2 $1 . $rest) (tl-append $1 $2))
   ;; opt-code-comment => 
   (lambda $rest (list))
   ;; opt-code-comment => code-comment
   (lambda ($1 . $rest) $1)
   ;; identifier => '$ident
   (lambda ($1 . $rest) `(ident ,$1))
   ;; identifier => 'cpp-ident
   (lambda ($1 . $rest) `(ident ,$1))
   ;; constant => '$fixed
   (lambda ($1 . $rest) `(fixed ,$1))
   ;; constant => '$float
   (lambda ($1 . $rest) `(float ,$1))
   ;; constant => '$chlit
   (lambda ($1 . $rest) `(char ,$1))
   ;; constant => '$chlit/L
   (lambda ($1 . $rest)
     `(char (@ (type "wchar_t")) ,$1))
   ;; constant => '$chlit/u
   (lambda ($1 . $rest)
     `(char (@ (type "char16_t")) ,$1))
   ;; constant => '$chlit/U
   (lambda ($1 . $rest)
     `(char (@ (type "char32_t")) ,$1))
   ;; string-literal => '$string
   (lambda ($1 . $rest) (make-tl 'string $1))
   ;; string-literal => string-literal '$string
   (lambda ($2 $1 . $rest) (tl-append $1 $2))
   ;; code-comment => '$code-comm
   (lambda ($1 . $rest) `(comment ,$1))
   ;; lone-comment => '$lone-comm
   (lambda ($1 . $rest) `(comment ,$1))
   ;; cpp-statement => 'cpp-stmt
   (lambda ($1 . $rest) `(cpp-stmt ,$1))
   ;; pragma => "_Pragma" "(" string-literal ")"
   (lambda ($4 $3 $2 $1 . $rest)
     `(pragma ,(tl->list $3)))
   ))

;;; end tables
