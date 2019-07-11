;;; -*- coding: utf-8-unix  -*-
;;;
;;;Part of: MMCK Infix
;;;Contents: core module
;;;Date: Jul  9, 2019
;;;
;;;Abstract
;;;
;;;	This  file implements  the INFIX  syntax.  It  is an  infix to  prefix notation  transformer
;;;	supporting the traditional mathematical expressions  infix syntax.  The transformer is based
;;;	on a Pratt parser as exposed in:
;;;
;;;        Pratt, Vaughan.  "Top Down Operator  Precedence".  Massachussets Institute of Technology.
;;;        Proceedings of the  1st Annual ACM SIGACT-SIGPLAN Symposium on  Principles of Programming
;;;        Languages (1973).
;;;
;;;Copyright (C) 2019 Marco Maggi <mrc.mgg@gmail.com>
;;;
;;;This program is  free software: you can redistribute  it and/or modify it under the  terms of the
;;;GNU Lesser General Public License as published  by the Free Software Foundation, either version 3
;;;of the License, or (at your option) any later version.
;;;
;;;This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
;;;even the implied  warranty of MERCHANTABILITY or  FITNESS FOR A PARTICULAR PURPOSE.   See the GNU
;;;Lesser General Public License for more details.
;;;
;;;You should have received a copy of the GNU Lesser General Public License along with this program.
;;;If not, see <http://www.gnu.org/licenses/>.
;;;


;;;; copyright notice for the XOR macro
;;;
;;;Copyright (c) 2008 Derick Eddington
;;;
;;;Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
;;;associated documentation  files (the "Software"),  to deal  in the Software  without restriction,
;;;including  without limitation  the  rights  to use,  copy,  modify,  merge, publish,  distribute,
;;;sublicense, and/or  sell copies of the  Software, and to permit  persons to whom the  Software is
;;;furnished to do so, subject to the following conditions:
;;;
;;;The  above copyright  notice  and this  permission  notice shall  be included  in  all copies  or
;;;substantial portions of the Software.
;;;
;;;Except as contained in this notice, the name(s)  of the above copyright holders shall not be used
;;;in advertising or otherwise  to promote the sale, use or other dealings  in this Software without
;;;prior written authorization.
;;;
;;;THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
;;;NOT  LIMITED  TO  THE  WARRANTIES  OF  MERCHANTABILITY, FITNESS  FOR  A  PARTICULAR  PURPOSE  AND
;;;NONINFRINGEMENT.  IN  NO EVENT SHALL THE  AUTHORS OR COPYRIGHT  HOLDERS BE LIABLE FOR  ANY CLAIM,
;;;DAMAGES OR OTHER  LIABILITY, WHETHER IN AN  ACTION OF CONTRACT, TORT OR  OTHERWISE, ARISING FROM,
;;;OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.


;;;; units and module header

(declare (unit mmck.infix.core)
	 (uses mmck.infix.helpers)
	 (uses mmck.infix.funcs)
	 (emit-import-library mmck.infix.core))

(module (mmck.infix.core)
    ((syntax: infix))
  (import (scheme)
	  (chicken fixnum)
	  (mmck infix helpers)
	  (mmck infix funcs))
  (import-for-syntax (scheme)
		     (chicken fixnum)
		     (mmck infix helpers)
		     (coops)
		     (coops-primitive-objects)
		     (only (matchable)
			   match))


(define-syntax infix
  (er-macro-transformer
    (lambda (input-form.stx rename compare)


;;;; helpers

(define-syntax-rule (end-of-input? ?object-from-lexer)
  (eof-object? ?object-from-lexer))

(define-syntax-rule (end-of-input-object)
  #!eof)

(define (is-a? obj cls)
  ;;Return true  if OBJ is an  instance of class  CLS, or of  one of its subtypes;  otherwise return
  ;;false.
  ;;
  (let ((obj.cls (class-of obj)))
    (or (eq? obj.cls cls)
	(subclass? obj.cls cls))))

(define-syntax case-identifiers
  (syntax-rules (else)
    ((_ ?expr ((?id0 ?id ...) ?body0 ?body ...) ... (else ?else-body0 ?else-body ...))
     (let ((obj ?expr))
       (define (else-branch)
	 ?else-body0 ?else-body ...)
       (if (identifier? obj)
	   (cond ((or (compare obj (rename (quote ?id0)))
		      (compare obj (rename (quote ?id)))
		      ...)
		  ?body0 ?body ...)
		 ...
		 (else
		  (else-branch)))
	 (else-branch))))
    ((_ ?expr ((?id0 ?id ...) ?body0 ?body ...) ...)
     (let ((obj ?expr))
       (if (identifier? obj)
	   (cond ((or (compare obj (rename (quote ?id0)))
		      (compare obj (rename (quote ?id)))
		      ...)
		  ?body0 ?body ...)
		 ...)
	 (values))))
    ))

(define-syntax case-token
  (syntax-rules ()
    ((_ ?token-expr
	((#:token)        . ?token-object-body)
	((#:end-of-input) . ?end-of-input-body))
     (let ((TOKEN ?token-expr))
       (cond ((is-a? TOKEN <token>) . ?token-object-body)
	     ((end-of-input? TOKEN) . ?end-of-input-body)
	     (else
	      (synner "internal error: invalid object from lexer" TOKEN)))))
    ))

(define (synner message . args)
  (apply syntax-error 'infix message input-form.stx args))

(define (synner-warning message . args)
  (apply warning message input-form.stx args))

(define (immediate-end-of-input-handler)
  ;;Special case: no input tokens.
  (list %values))


;;;; syntactic identifiers
;;
;;The following  syntactic identifier objects are  needed to build the  output form or to  parse the
;;input form.

(define %if			(rename 'if))
(define %begin			(rename 'begin))
(define %values			(rename 'values))

(define %quote			(rename 'quote))
(define %quasiquote		(rename 'quasiquote))
(define %unquote		(rename 'unquote))


;;;; binding powers

;;The fixnum zero can be  used as minimum binding power (but such value  is not enforced as minimum,
;;negative fixnums will work just fine).
(define MINIMUM-BINDING-POWER			0)
(define INFIX-LOGIC-BINDING-POWER		300)
(define PREFIX-LOGIC-BINDING-POWER		350)
(define COMPARISON-BINDING-POWER		400)
(define PLUS/MINUS-BINDING-POWER		500)
(define MUL/DIV-BINDING-POWER			600)
(define MOD-BINDING-POWER			650)
(define EXPT-RIGHT-BINDING-POWER		700)
(define EXPT-LEFT-BINDING-POWER			800)
(define FACTORIAL-LEFT-BINDING-POWER		900)
(define INCR/DECR-BINDING-POWER			1000)
(define INFIX-BITWISE-BINDING-POWER		1300)
(define PREFIX-BITWISE-BINDING-POWER		1350)
(define INFIX-BITSHIFT-BINDING-POWER		1400)
(define LEFT-PAREN-LEFT-BINDING-POWER		2000)
(define LEFT-PAREN-RIGHT-BINDING-POWER		MINIMUM-BINDING-POWER)

(define QUESTION-MARK-LEFT-BINDING-POWER	400)
(define QUESTION-MARK-RIGHT-BINDING-POWER	MINIMUM-BINDING-POWER)


;;;; generic procedures

;;The null-denotator (NUD).  It  is called when this token does *not* have  a left semantic value to
;;which it might left-bind,  or it has already been decided that this  token does *not* left-bind to
;;the left semantic value.  This denotator is allowed to select its right-binding power depending on
;;which tokens come next.
;;
;;Both operands  and operators might have  a NUD that does  something (rather than raising  a syntax
;;error exception).
;;
(define-generic (null-denotator token lexer))

;;The left-denotator (LED).  It  is called when this token *does* have a  left semantic value and it
;;has already  been decided that this  token *does* left-bind to  it.  This denotator is  allowed to
;;select its right-binding power depending on which tokens come next.
;;
;;The argument LEFT-SEMANTIC-VALUE is the semantic value composed by parsing previous tokens.
(define-generic (left-denotator token lexer left-semantic-value))


(define (flatten-input stx)
  ;;Given a syntax object STX representing an input expression: decompose it into a flat list of raw
  ;;tokens and  return the  list holding the  raw tokens in  reverse order.   We expect the  list to
  ;;contain: atoms as  defined by the function ATOM?; identifiers;  the characters open parenthesis,
  ;;close parenthesis and comma.
  ;;
  ;;We  have to  remember that  a syntax  object can  be both  wrapper or  fully unwrapped;  this is
  ;;relevant when we flatten syntax objects like:
  ;;
  ;;   (quote a)
  ;;   (begin a)
  ;;
  ;;which are meant to be  handled as atoms, not as lists.  For this  reason this function returns 2
  ;;values: the first  is a boolean, the  second is an improper  list of raw tokens;  the boolean is
  ;;true if the second return value is to be intepreted as an actual list of raw tokens, false if it
  ;;is to be interpreted as an atom.
  ;;
  (define (is-quote? stx)
    (compare stx %quote))
  (define (is-quasiquote? stx)
    (compare stx %quasiquote))
  (define (is-unquote? stx)
    (compare stx %unquote))
  (define (is-begin? stx)
    (compare stx %begin))

  (match stx
    (()
     (values #f '()))

    ;;We want:
    ;;
    ;;   (infix '(1 2 3))
    ;;   ==> (infix (quote (1 2 3)))
    ;;   ==> (quote (1 2 3))
    ;;
    (((? is-quote?) . ?stuff)
     (values #f stx))

    ;;We want:
    ;;
    ;;   (infix `(1 2 3))
    ;;   ==> (infix (quasiquote (1 2 3)))
    ;;   ==> (quasiquote (1 2 3))
    ;;
    (((? is-quasiquote?) . ?stuff)
     (values #f stx))

    ;;We want:
    ;;
    ;;   (infix atan ( 1 , 2 ) )
    ;;   ==> (infix atan ( 1 (unquote 2) ) )
    ;;   ==> (atan 1 2)
    ;;
    (((? is-unquote?) . ?stuff)
     (receive (unused result)
	 (flatten-input ?stuff)
       (values #t (append result (list #\,)))))

    ;;We want:
    ;;
    ;;   (infix (begin (+ 1 2)))
    ;;   ==> (begin (+ 1 2))
    ;;
    (((? is-begin?) . ?stuff)
     (values #f stx))

    ;;This matches a list.
    ;;
    ((?item* ...)
     (values #t
	     (cons #\)
		   (fold-left
		       (lambda (knil item)
			 (match item
			   ;;This  clause  is needed  to  correctly  handle  the case  of  procedure
			   ;;application with no arguments.  Examples:
			   ;;
			   ;;   (infix noargs())
			   ;;   (infix 2 + noargs() + 3)
			   ;;
			   ;;in which the "()" must be lexed as the two tokens #\( and #\).
			   (()
			    (append '(#\) #\() knil))
			   (_
			    (receive (is-list? result)
				(flatten-input item)
			      (if is-list?
				  (append result knil)
				(cons result knil))))
			   ))
		     '( #\( )
		     ?item*))))

    ;;This matches a variable reference.
    ;;
    ((? identifier? ?id)
     (values #f ?id))

    ;;This matches an atom.
    ;;
    ((? atom? ?atom)
     (values #f (strip-syntax ?atom)))

    (_
     (synner "invalid expression syntax" stx))))


(define (make-lexer token*)
  ;;Given a list of "<token>" records return a closure implementing the lexer.
  ;;
  ;;* When the lexer is  called without arguments: it pops the next token  from the list and returns
  ;;  it.
  ;;
  ;;* When  the lexer is  called with a  single argument:  it returns the  next token from  the list
  ;;  without popping it.  The argument itself is ignored.
  ;;
  ;;* When there are no more tokens: the lexer returns the end-of-input object.
  ;;
  (assert (and (list? token*)
	       (for-all (lambda (obj)
			  (is-a? obj <token>))
		 token*)))
  (case-lambda
    ((obj)	;peek
     (if (pair? token*)
	 (car token*)
       (end-of-input-object)))
    (()		;get
     (if (pair? token*)
	 (receive-and-return (T)
	     (car token*)
	   (set! token* (cdr token*)))
       (end-of-input-object)))))


(define (parse lexer caller-right-binding-power immediate-end-of-input-handler)
  ;;This procedure is the core of the  Pratt parser.  It composes the next sub-expression's semantic
  ;;value parsing  tokens from  the lexer;  it can be  called by  the operator's  left-denotators to
  ;;produce a right-hand argument.
  ;;
  ;;Let's examine  what happens when parsing the expression:
  ;;
  ;;   1 + 2 - 3
  ;;
  ;;which we represent with the sequence of tokens:
  ;;
  ;;   OPERAND[1] PLUS OPERAND[2] MINUS OPERAND[3]
  ;;
  ;;where  PLUS  and  MINUS  are  operator  tokens.  We  use  the  abbreviations:  NUD  for  token's
  ;;null-denotator procedure; LED for token's left-denotator procedure.
  ;;
  ;;PARSE is called with minimum right-binding power:
  ;;
  ;;01.PARSE consumes the token OPERAND[1] and calls its NUD, which just returns the semantic value:
  ;;fixnum 1.  PARSE tail-calls its sub-procedure LOOP.
  ;;
  ;;02.LOOP  consumes  the  token  PLUS,  whose  left-binding power  is  greater  than  the  minimum
  ;;right-binding power; LOOP calls the LED of PLUS with 1 as left semantic value.
  ;;
  ;;03.,,The LED of PLUS calls PARSE with its right-binding power.
  ;;
  ;;04.,,..PARSE consumes the  token OPERAND[2] and calls  its NUD, which just  returns the semantic
  ;;value: fixnum 2.  PARSE tail-calls LOOP.
  ;;
  ;;05.,,..LOOP looks ahead the token MINUS, whose  left-binding power is equal to the right-binding
  ;;power or PLUS; LOOP returns the semantic value 2.
  ;;
  ;;06.,,The LED of PLUS composes the semantic value (#'+ 1 2) and returns it.
  ;;
  ;;07.LOOP  consumes  the  token MINUS,  whose  left-binding  power  is  greater than  the  minimum
  ;;right-binding power; LOOP calls the LED of MINUS with (#'+ 1 2) as left semantic value.
  ;;
  ;;08.,,The LED of MINUS calls PARSE with its right-binding power.
  ;;
  ;;09.,,..PARSE consumes the  token OPERAND[3] and calls  its NUD, which just  returns the semantic
  ;;value: fixnum 3.  PARSE tail-calls LOOP.
  ;;
  ;;10.,,..LOOP looks ahead the end-of-input and returns the semantic value 3.
  ;;
  ;;11.,,The LED of MINUS composes the semantic value (#'- (#'+ 1 2) 3) and returns it.
  ;;
  ;;12.LOOP looks ahead the end-of-input and returns the semantic value (#'- (#'+ 1 2) 3).
  ;;
  (define (loop left-semantic-value lexer caller-right-binding-power)
    ;;We have acquired  a semantic value from  parsing previous tokens.  Now we  loop parsing tokens
    ;;until a full sub-expression's semantic value has been formed.
    ;;
    ;;While the left-binding power of the next token  is greater than the right-binding power of the
    ;;caller denotator: we continue  calling the left-denotator of the next token.   It is this very
    ;;mechanism that allows us to implement operator precedence, left and right associativity.
    ;;
    (define-syntax-rule (recurse ?new-semantic-value)
      (loop ?new-semantic-value lexer caller-right-binding-power))
    (let ((token (lexer 'lookahead)))
      (case-token token
	((#:token)
	 ;;By using FX>=  we make the binary infix  operators, with the same left  and right binding
	 ;;powers, left-associative by default; example:
	 ;;
	 ;;   (1 + 2 + 3) => (+ (+ 1 2) 3)
	 ;;
	 ;;by using FX> they would have been right-associative by default.
	 (if (fx>= caller-right-binding-power (slot-value token 'left-binding-power))
	     left-semantic-value
	   (begin
	     (lexer) ;consume the token
	     (recurse (left-denotator token lexer left-semantic-value)))))
	((#:end-of-input)
	 left-semantic-value))))

  ;;When correct  input is given:  we expect at  least a sub-expression.  It  is an exception  if we
  ;;immediately find end-of-input.
  (let ((token (lexer)))
    (case-token token
      ((#:token)
       (loop (null-denotator token lexer)
	     lexer caller-right-binding-power))
      ((#:end-of-input)
       ;;We got an EOI before composing a full expression.
       (immediate-end-of-input-handler)))))


;;;; atom predicates

(define (atom? obj)
  (or (number?     obj)
      (boolean?    obj)
      (char?       obj)
      (string?     obj)
      (bytevector? obj)
      (null?       obj)))

(define (identifier? stx)
  (symbol? stx))


;;This is the base type of tokens for Pratt parsing.
;;
(define-class <token>
    ()
  ((semantic-value)
		;Scheme object  representing the semantic value  of this token.  This  object can be
		;anything.

   (left-binding-power)
		;A fixnum representing the left-binding power of this token.  Operands must have the
		;minimum left-binding power; operators must  have a left-binding power which defines
		;precedence; passive syntactic tokens must have minimum left-binding power.
   ))

(define (make-<token> semantic-value left-binding-power)
  (make <token>
    'semantic-value	semantic-value
    'left-binding-power	left-binding-power))


;;This is the type of operand tokens for  Pratt parsing.  Conceptually, an operand has both left and
;;right binding powers  set to the minimum; the  previous and next tokens decide  whether an operand
;;will bind to the left or right.
;;
(define-class <operand>
    (<token>))

(define (make-<operand> semantic-value)
  (make <operand>
    'semantic-value	semantic-value
    'left-binding-power	MINIMUM-BINDING-POWER))

(define-method (null-denotator {token <operand>} {lexer <procedure>})
  ;;If this method is called:  the token is an operand and it starts a  sequence of tokens which are
  ;;meant to compose a sub-expression.  Return the semantic  value and let the caller decide if this
  ;;token binds to the left or right; the caller is usually the PARSE procedure.
  ;;
  (slot-value token 'semantic-value))

(define-method (left-denotator {token <operand>} {lexer <procedure>} {left-semantic-value #t})
  (synner "operand found when an operator was expected" token))


;;This is the type  of operator tokens for Pratt parsing.   We decide that only
;;identifiers are operators.
;;
(define-class <operator>
    (<token>))

(define (make-<operator> id token-left-binding-power)
  (make <operator>
    'semantic-value	id
    'left-binding-power	token-left-binding-power))


;;object type of operators of which we know  that: the right-binding power of the null-denotator and
;;left-denotator procedures is always the same, no matter which next tokens come out of the lexer.
;;
(define-class <fixed-right-binding-power-operator>
    (<operator>)
  ((right-binding-power))
		;A fixnum  representing the right-binding  power of this token's  null-denotator and
		;left-denotator procedures.
  #| end of DEFINE-CLASS |# )

(define (make-<fixed-right-binding-power-operator> id token-left-binding-power nud/led-right-binding-power)
  (make <fixed-right-binding-power-operator>
    'semantic-value		id
    'left-binding-power		token-left-binding-power
    'right-binding-power	nud/led-right-binding-power))


;;This is the object type of binary infix operators.
;;
(define-class <infix-operator>
    (<fixed-right-binding-power-operator>))

(define (make-<infix-operators> id token-left-binding-power nud/led-right-binding-power)
  (make <infix-operator>
    'semantic-value		id
    'left-binding-power		token-left-binding-power
    'right-binding-power	nud/led-right-binding-power))

(define (<infix-operator>-nud token lexer)
  ;;If this method  is called: this token is a  binary infix operator and it has  *no* left semantic
  ;;value it might left-bind to: this is a syntax error.
  ;;
  (synner "binary infix operator without left operand" token))

(define (<infix-operator>-led token lexer left-semantic-value)
  ;;If this method  is called: this token is a  binary infix operator, it has a  left semantic value
  ;;and it has already been decided that this token binds to it.
  ;;
  (define THIS-DENOTATOR-RIGHT-BINDING-POWER
    (slot-value token 'right-binding-power))
  (define (immediate-end-of-input-handler)
    (synner "infix operator without right operand" token))
  (let* ((this-semantic-value	(slot-value token 'semantic-value))
	 (right-semantic-value	(parse lexer THIS-DENOTATOR-RIGHT-BINDING-POWER immediate-end-of-input-handler)))
    (list this-semantic-value
	  left-semantic-value
	  right-semantic-value)))

(define-method (null-denotator {token <infix-operator>} {lexer <procedure>})
  (<infix-operator>-nud token lexer))

(define-method (left-denotator {token <infix-operator>} {lexer <procedure>} {left-semantic-value #t})
  (<infix-operator>-led token lexer left-semantic-value))


;;This is the object type of left-associative binary infix operators like *.  To be left-associative
;;the operator X must act as follows:
;;
;;   A x B x C => (x (x A B) C)
;;
;;the compared binding powers are the following:
;;
;;                       A x B x C
;;                         ^   ^
;;   LED right-binding power   token left-binding power
;;
;;and the LED right-binding power must be higher than or equal to the other.
;;
(define-class <left-assoc-infix-operator>
    (<infix-operator>))

(define (make-<left-assoc-infix-operator> id token-left-binding-power nud/led-right-binding-power)
  (assert (fx<= token-left-binding-power nud/led-right-binding-power))
  (make <left-assoc-infix-operator>
    'semantic-value		id
    'left-binding-power		token-left-binding-power
    'right-binding-power	nud/led-right-binding-power))


;;This  is  the  record  type  of  right-associative  binary  infix  operators  like  EXPT.   To  be
;;right-associative the operator X must act as follows:
;;
;;   A x B x C => (x A (x B C))
;;
;;the compared binding powers are the following:
;;
;;                       A x B x C
;;                         ^   ^
;;   LED right-binding power   token left-binding power
;;
;;and the token left-binding power must be higher than the other.
;;
(define-class <right-assoc-infix-operator>
    (<infix-operator>))

(define (make-<right-assoc-infix-operator> id token-left-binding-power nud/led-right-binding-power)
  (assert (fx> token-left-binding-power nud/led-right-binding-power))
  (make <right-assoc-infix-operator>
    'semantic-value		id
    'left-binding-power		token-left-binding-power
    'right-binding-power	nud/led-right-binding-power))


;;object type  of left-associative binary  infix operators of which  we know that:  the left-binding
;;power of the token and the right-binding power of the left-denotator procedure is always the same,
;;no matter which next tokens come out of the lexer.
;;
;;Examples are the common arithmetic operators +, -, *, /, the logic operators AND, OR, XOR, NOT and
;;the comparison operators <, >, <=, >=, =, !=;  notice that EXPT is right-associative, so it is not
;;of this type.
;;
(define-class <symmetric-left-assoc-infix-operator>
    (<left-assoc-infix-operator>))

(define (make-<symmetric-left-assoc-infix-operator> id binding-power)
  (make <symmetric-left-assoc-infix-operator>
    'semantic-value		id
    'left-binding-power		binding-power
    'right-binding-power	binding-power))


;;This is the object type of unary prefix operators like NOT.
;;
(define-class <prefix-operator>
    (<fixed-right-binding-power-operator>))

(define (make-<prefix-operator> id nud/led-right-binding-power)
  (make <prefix-operator>
    'semantic-value		id
    'left-binding-power		MINIMUM-BINDING-POWER
    'right-binding-power	nud/led-right-binding-power))

(define (<prefix-operator>-nud token lexer)
  ;;If this method  is called: this token is a  unary prefix operator and it has  *no* left semantic
  ;;value it might left-bind to.
  ;;
  ;;The scenario of tokens from the lexer is this:
  ;;
  ;;   this-token next-token
  ;;
  ;;where NEXT-TOKEN is still in the lexer.
  ;;
  ;;Examples:
  ;;
  ;;* This token is the operator "not" in the following expression:
  ;;
  ;;     not 4
  ;;
  ;;the next token  is "4".  The next token is  an operand: we just acquire the  next token and call
  ;;its left-denotator to obtain the full right operand.
  ;;
  ;;* This token is the leftmost operator "not" in the following expression:
  ;;
  ;;     not not 4
  ;;      ^
  ;;     this one
  ;;
  ;;the next token is "not".  The next token is an operator: we just acquire the next token and call
  ;;its null-denotator to obtain the full right operand.
  ;;
  (define THIS-DENOTATOR-RIGHT-BINDING-POWER
    (slot-value token 'right-binding-power))
  (define (immediate-end-of-input-handler)
    (synner "unexpected end of input while parsing operand for unary prefix operator" token))
  (let ((this-semantic-value  (slot-value token 'semantic-value))
	(right-semantic-value (parse lexer THIS-DENOTATOR-RIGHT-BINDING-POWER immediate-end-of-input-handler)))
    (list this-semantic-value
	  right-semantic-value)))

(define (<prefix-operator>-led token lexer left-semantic-value)
  ;;If this method  is called: this token is a  unary prefix operator, it has a  left semantic value
  ;;and it has already been decided that it left-binds to it: this is a syntax error.
  ;;
  (synner "unary prefix operator has no left operand" token))

;;; --------------------------------------------------------------------

(define-method (null-denotator {token <prefix-operator>} {lexer <procedure>})
  (<prefix-operator>-nud token lexer))

(define-method (left-denotator {token <prefix-operator>} {lexer <procedure>} {left-semantic-value #t})
  (<prefix-operator>-led token lexer left-semantic-value))


;;This is the object type of unary postfix operators like "!" (which is factorial).
;;
(define-class <postfix-operator>
    (<fixed-right-binding-power-operator>))

(define (make-<postfix-operator> id token-left-binding-power)
  (make <postfix-operator>
    'semantic-value		id
    'left-binding-power		token-left-binding-power
    'right-binding-power	MINIMUM-BINDING-POWER))

(define (<postfix-operator>-nud token lexer)
  ;;If this method is called:  this token is a unary postfix operator and  it has *no* left semantic
  ;;value it might left-bind to: this is a syntax error.
  ;;
  (synner "unary postfix operator without left operand" token))

(define (<postfix-operator>-led token lexer left-semantic-value)
  ;;If this method is called:  this token is a unary postfix operator, it  has a left semantic value
  ;;and it has already been been decided that it left-binds to it.
  ;;
  (list (slot-value token 'semantic-value) left-semantic-value))

;;; --------------------------------------------------------------------

(define-method (null-denotator {token <postfix-operator>} {lexer <procedure>})
  (<postfix-operator>-nud token lexer))

(define-method (left-denotator {token <postfix-operator>} {lexer <procedure>} {left-semantic-value #t})
  (<postfix-operator>-led token lexer left-semantic-value))


;;This is  the object type  of operators that  can be  used both as  binary infix and  unary prefix.
;;Examples are the arithmetic operators + and -.
;;
(define-class <left-assoc-infix/prefix-operator>
    (<fixed-right-binding-power-operator>))

(define (make-<left-assoc-infix/prefix-operator> id token-left-binding-power nud/led-right-binding-power)
  (make <left-assoc-infix/prefix-operator>
    'semantic-value		id
    'left-binding-power		token-left-binding-power
    'right-binding-power	nud/led-right-binding-power))

;;; --------------------------------------------------------------------

(define-method (null-denotator {token <left-assoc-infix/prefix-operator>} {lexer <procedure>})
  (<prefix-operator>-nud token lexer))

(define-method (left-denotator {token <left-assoc-infix/prefix-operator>} {lexer <procedure>} {left-semantic-value #t})
  (<infix-operator>-led token lexer left-semantic-value))


;;This is the  base object type for operators  that can appear in both prefix  and postfix position,
;;for example the increment and decrement operators.
;;
(define-class <prefix/postfix-operator>
    (<fixed-right-binding-power-operator>)
  ((postfix-semantic-value)
		;A semantic value to be used when the operator is in postfix position.
   ))

(define (make-<prefix/postfix-operator> prefix-id postfix-id token-left-binding-power nud/led-right-binding-power)
  (assert (identifier? postfix-id))
  (make <prefix/postfix-operator>
    'semantic-value		prefix-id
    'left-binding-power		token-left-binding-power
    'right-binding-power	nud/led-right-binding-power
    'postfix-semantic-value	postfix-id))

;;; --------------------------------------------------------------------

(define-method (null-denotator {token <prefix/postfix-operator>} {lexer <procedure>})
  (<prefix-operator>-nud token lexer))

(define-method (left-denotator {token <prefix/postfix-operator>} {lexer <procedure>} {left-semantic-value #t})
  ;;If this method is called: this token is  a unary prefix/postfix operator, it has a left semantic
  ;;value and it has already been been decided that  it left-binds to it; this means the operator is
  ;;in postfix position.
  ;;
  (list (slot-value token 'postfix-semantic-value)
	left-semantic-value))


;;This is the record  type for operators that can appear in both  prefix and postfix position, for
;;example the increment  and decrement operators, for  which we know the  token left-binding power
;;equals the left-denotator right-binding power.
;;
(define-class <symmetric-prefix/postfix-operator>
    (<prefix/postfix-operator>))

(define (make-<symmetric-prefix/postfix-operator> prefix-id postfix-id binding-power)
  (make <symmetric-prefix/postfix-operator>
    'semantic-value		prefix-id
    'left-binding-power		binding-power
    'right-binding-power	binding-power
    'postfix-semantic-value	postfix-id))


;;This is the base type  of passive syntactic tokens.  These tokens are meant  to be consumed by the
;;NUD or LED of operators; the NUD and LED of a passive token are never called when correct input is
;;parsed.
;;
(define-class <passive-syntactic-token>
    (<token>)
  ((description)))

(define (make-<passive-syntactic-token> description)
  (make <passive-syntactic-token>
    'semantic-value		#f
    'left-binding-power		MINIMUM-BINDING-POWER
    'description		description))

(define (<passive-syntactic-token>-syntax-error-message token)
  (string-append "unexpected " (slot-value token 'description)))

(define-method (null-denotator {token <passive-syntactic-token>} {lexer <procedure>})
  (synner (<passive-syntactic-token>-syntax-error-message token)
	  token))

(define-method (left-denotator {token <passive-syntactic-token>} {lexer <procedure>} {left-semantic-value #t})
  (synner (<passive-syntactic-token>-syntax-error-message token)
	  token))


;;The right parenthesis is meant to be passively consumed by the NUD or LED of the left parenthesis.
;;
(define-class <right-paren>
    (<passive-syntactic-token>))

(define make-<right-paren>
  (let ((memoised #f))
    (lambda ()
      (or memoised
	  (receive-and-return (V)
	      (make <right-paren>
		'semantic-value		#f
		'left-binding-power	MINIMUM-BINDING-POWER
		'description		"right parenthesis")
	    (set! memoised V))))))


;;The comma separator is meant to be passively consumed by the LED of the left parenthesis.
;;
(define-class <comma>
    (<passive-syntactic-token>))

(define make-<comma>
  (let ((memoised #f))
    (lambda ()
      (or memoised
	  (receive-and-return (V)
	      (make <comma>
		'semantic-value		#f
		'left-binding-power	MINIMUM-BINDING-POWER
		'description		"comma separator")
	    (set! memoised V))))))


;;The left parenthesis is an operator.
;;
(define-class <left-paren>
    (<fixed-right-binding-power-operator>))

(define make-<left-paren>
  (let ((memoised #f))
    (lambda ()
      (or memoised
	  (receive-and-return (V)
	      (make <left-paren>
		'semantic-value		#\(
		'left-binding-power	LEFT-PAREN-LEFT-BINDING-POWER
		'right-binding-power	LEFT-PAREN-RIGHT-BINDING-POWER)
	    (set! memoised V))))))

(define-method (null-denotator {token <left-paren>} {lexer <procedure>})
  ;;If this method is called:  this token is a left parenthesis and there  is no left semantic value
  ;;it might left-bind to.  Open a parenthetical sub-expression: read a sub-expression and consume a
  ;;right parenthesis token.
  ;;
  ;;Example, when this procedure is called the left-paren has just been consumed:
  ;;
  ;;   a * (b - c)
  ;;        ^
  ;;    we are here
  ;;
  (define THIS-DENOTATOR-RIGHT-BINDING-POWER
    (slot-value token 'right-binding-power))
  (define (end-of-input-handler)
    (synner "end of input while looking for matching sub-expression right parenthesis"
	    token))
  (let* ((left-semantic-value	(parse lexer THIS-DENOTATOR-RIGHT-BINDING-POWER end-of-input-handler))
	 (token			(lexer)))
    (case-token token
      ((#:token)
       (if (is-a? token <right-paren>)
	   left-semantic-value
	 (synner "expected matching right parenthesis" token)))
      ((#:end-of-input)
       (end-of-input-handler)))))

(define-method (left-denotator {token <left-paren>} {lexer <procedure>} {left-semantic-value #t})
  ;;If this method is called:  this token is a left parenthesis, there is  a left semantic value and
  ;;it has already been decided that this token left-binds to it.  Start the list of arguments for a
  ;;procedure  application sub-expression:  read arguments  separated  by comma  tokens and  finally
  ;;consume the right parenthesis token.
  ;;
  ;;Example, when this procedure is called the left-paren has just been consumed:
  ;;
  ;;   func ( arg1 , arg , ... )
  ;;          ^
  ;;    we are here
  ;;
  ;;We expect the LEFT-SEMANTIC-VALUE  to represent an expression evaluating to  a procedure and the
  ;;next tokens to form an arguments list.
  ;;
  ;;NOTE We use the comma as arguments separator; the Scheme reader transforms the sequence:
  ;;
  ;;   , ?form
  ;;
  ;;into:
  ;;
  ;;  (unquote ?form)
  ;;
  ;;so the list of arguments is:
  ;;
  ;;  ( arg1 (unquote arg) ... )
  ;;
  ;;NOTE R6RS does not define the comma to be a delimiter, so writing:
  ;;
  ;;   func (arg1, arg, ...)
  ;;
  ;;with no space  between the ARG and the comma  is a syntax error; it should  be trivial to change
  ;;the Scheme reader to handle the comma as a delimiter.
  ;;
  (define THIS-DENOTATOR-RIGHT-BINDING-POWER
    (slot-value token 'right-binding-power))

  (define (end-of-input-handler)
    (synner "unexpected end of input while reading list of procedure application arguments" token))

  (define (unexpected-token token)
    (synner "unexpected token while reading list of procedure application arguments" token))

  (define (loop reversed-list-of-arguments)
    (let ((token (lexer 'lookahead)))
      (case-token token
	((#:token)
	 (cond ((is-a? token <right-paren>)
		(lexer) ;consume the token
		;;Return a semantic value representing a function application with arguments.
		(cons left-semantic-value (reverse reversed-list-of-arguments)))
	       ((is-a? token <comma>)
		(lexer) ;consume the token
		(let ((next-semantic-value (parse lexer THIS-DENOTATOR-RIGHT-BINDING-POWER end-of-input-handler)))
		  (loop (cons next-semantic-value reversed-list-of-arguments))))
	       (else
		(unexpected-token token))))
	((#:end-of-input)
	 (end-of-input-handler)))))

  ;;First we expect a closed parenthesis or an argument.
  (let ((token (lexer 'lookahead)))
    (case-token token
      ((#:token)
       (if (is-a? token <right-paren>)
	   (begin
	     (lexer) ;consume the token
	     ;;Return a semantic value representing a function application with no arguments.
	     (list left-semantic-value))
	 (loop (list (parse lexer THIS-DENOTATOR-RIGHT-BINDING-POWER end-of-input-handler)))))
      ((#:end-of-input)
       (end-of-input-handler)))))


;;The colon is the separator in the syntax:
;;
;;   ?test ? ?consequent : ?alternate
;;
;;it is meant to be passively consumed by the left-denotator procedure of the question mark.
;;
(define-class <colon>
    (<passive-syntactic-token>))

(define make-<colon>
  (let ((memoised #f))
    (lambda ()
      (or memoised
	  (receive-and-return (V)
	      (make <colon>
		'semantic-value		#f
		'left-binding-power	MINIMUM-BINDING-POWER
		'description		"colon")
	    (set! memoised V))))))


;;The question mark is the operator in the ternary conditional expression syntax:
;;
;;   ?test ? ?consequent : ?alternate
;;
(define-class <question-mark>
    (<fixed-right-binding-power-operator>))

(define make-<question-mark>
  (let ((memoised #f))
    (lambda ()
      (or memoised
	  (receive-and-return (V)
	      (make <question-mark>
		'semantic-value		#\?
		'left-binding-power	QUESTION-MARK-LEFT-BINDING-POWER
		'right-binding-power	QUESTION-MARK-RIGHT-BINDING-POWER)
	    (set! memoised V))))))

(define-method (null-denotator {token <question-mark>} {lexer <procedure>})
  ;;If this method is called:  this token is a question mark and there is  no left semantic value it
  ;;might left-bind to: this is a syntax error.
  ;;
  (synner "question mark operator without test operand" token))

(define-method (left-denotator {token <question-mark>} {lexer <procedure>} {left-semantic-value #t})
  ;;If this method is called:  this token is a question mark, there is a  left semantic value and it
  ;;has already been decided that this token left-binds to it.
  ;;
  ;;We  expect  the LEFT-SEMANTIC-VALUE  to  represent  the test  expression.   When  we enter  this
  ;;procedure the question mark has been already parsed:
  ;;
  ;;   ?test ? ?consequent : ?alternate
  ;;           ^
  ;;           we are here
  ;;
  ;;so  we read  a sub-expression  as consequent,  consume the  colon passive  syntactic token,  and
  ;;finally read a sub-expression as alternate.
  ;;
  (define THIS-DENOTATOR-RIGHT-BINDING-POWER
    (slot-value token 'right-binding-power))

  (define (end-of-input-handler)
    (synner "unexpected end of input while reading ternary conditional expression" token))

  (define (unexpected-token token)
    (synner "unexpected token while reading ternary conditional expression" token))

  (let* ((consequent	(parse lexer THIS-DENOTATOR-RIGHT-BINDING-POWER end-of-input-handler))
	 (token		(lexer)))
    (case-token token
      ((#:token)
       (if (is-a? token <colon>)
	   (let ((alternate (parse lexer THIS-DENOTATOR-RIGHT-BINDING-POWER end-of-input-handler)))
	     `(,%if ,left-semantic-value ,consequent ,alternate))
	 (unexpected-token token)))
      ((#:end-of-input)
       (end-of-input-handler)))))


;;;; predefined tokens

(define PLUS-TOKEN
  (make-<left-assoc-infix/prefix-operator> (rename '+) PLUS/MINUS-BINDING-POWER PLUS/MINUS-BINDING-POWER))

(define MINUS-TOKEN
  (make-<left-assoc-infix/prefix-operator> (rename '-) PLUS/MINUS-BINDING-POWER PLUS/MINUS-BINDING-POWER))

(define MUL-TOKEN
  (make-<symmetric-left-assoc-infix-operator> (rename '*) MUL/DIV-BINDING-POWER))

(define DIV-TOKEN
  (make-<symmetric-left-assoc-infix-operator> (rename '/) MUL/DIV-BINDING-POWER))

;; (define IDIV-TOKEN
;;   (make-<symmetric-left-assoc-infix-operator> (rename 'div) MUL/DIV-BINDING-POWER))

;; (define IMOD-TOKEN
;;   (make-<symmetric-left-assoc-infix-operator> (rename 'mod) MOD-BINDING-POWER))

;; (define IDIV0-TOKEN
;;   (make-<symmetric-left-assoc-infix-operator> (rename 'div0) MUL/DIV-BINDING-POWER))

;; (define IMOD0-TOKEN
;;   (make-<symmetric-left-assoc-infix-operator> (rename 'mod0) MOD-BINDING-POWER))

(define FACTORIAL-TOKEN
  (make-<postfix-operator> (rename 'factorial) FACTORIAL-LEFT-BINDING-POWER))

(define BANG-TOKEN
  (make-<prefix/postfix-operator> (rename 'not) (rename 'factorial) FACTORIAL-LEFT-BINDING-POWER PREFIX-LOGIC-BINDING-POWER))

(define EXPT-TOKEN
  (make-<right-assoc-infix-operator> (rename 'expt) EXPT-LEFT-BINDING-POWER EXPT-RIGHT-BINDING-POWER))

(define INCR-TOKEN
  (make-<symmetric-prefix/postfix-operator> (rename 'pre-incr) (rename 'post-incr) INCR/DECR-BINDING-POWER))

(define DECR-TOKEN
  (make-<symmetric-prefix/postfix-operator> (rename 'pre-decr) (rename 'post-decr) INCR/DECR-BINDING-POWER))

;;; --------------------------------------------------------------------

(define FXPLUS-TOKEN
  (make-<left-assoc-infix/prefix-operator> (rename 'fx+) PLUS/MINUS-BINDING-POWER PLUS/MINUS-BINDING-POWER))

(define FXMINUS-TOKEN
  (make-<left-assoc-infix/prefix-operator> (rename 'fx-) PLUS/MINUS-BINDING-POWER PLUS/MINUS-BINDING-POWER))

(define FXMUL-TOKEN
  (make-<symmetric-left-assoc-infix-operator> (rename 'fx*) MUL/DIV-BINDING-POWER))

;; (define FXDIV-TOKEN
;;   (make-<symmetric-left-assoc-infix-operator> (rename 'fxdiv) MUL/DIV-BINDING-POWER))

;; (define FXMOD-TOKEN
;;   (make-<symmetric-left-assoc-infix-operator> (rename 'fxmod) MOD-BINDING-POWER))

;; (define FXDIV0-TOKEN
;;   (make-<symmetric-left-assoc-infix-operator> (rename 'fxdiv0) MUL/DIV-BINDING-POWER))

;; (define FXMOD0-TOKEN
;;   (make-<symmetric-left-assoc-infix-operator> (rename 'fxmod0) MOD-BINDING-POWER))

;;; --------------------------------------------------------------------

;; (define FLPLUS-TOKEN
;;   (make-<left-assoc-infix/prefix-operator> (rename 'fl+) PLUS/MINUS-BINDING-POWER PLUS/MINUS-BINDING-POWER))

;; (define FLMINUS-TOKEN
;;   (make-<left-assoc-infix/prefix-operator> (rename 'fl-) PLUS/MINUS-BINDING-POWER PLUS/MINUS-BINDING-POWER))

;; (define FLMUL-TOKEN
;;   (make-<symmetric-left-assoc-infix-operator> (rename 'fl*) MUL/DIV-BINDING-POWER))

;; (define FLDIV-TOKEN
;;   (make-<symmetric-left-assoc-infix-operator> (rename 'fl/) MUL/DIV-BINDING-POWER))

;; (define FLEXPT-TOKEN
;;   (make-<right-assoc-infix-operator> (rename 'flexpt) EXPT-LEFT-BINDING-POWER EXPT-RIGHT-BINDING-POWER))

;;; --------------------------------------------------------------------

(define AND-TOKEN
  (make-<symmetric-left-assoc-infix-operator> (rename 'and) INFIX-LOGIC-BINDING-POWER))

(define OR-TOKEN
  (make-<symmetric-left-assoc-infix-operator> (rename 'or) INFIX-LOGIC-BINDING-POWER))

(define XOR-TOKEN
  (make-<symmetric-left-assoc-infix-operator> (rename 'xor) INFIX-LOGIC-BINDING-POWER))

(define NOT-TOKEN
  (make-<prefix-operator> (rename 'not) PREFIX-LOGIC-BINDING-POWER))

;;; --------------------------------------------------------------------

(define LESS-THAN-TOKEN
  (make-<symmetric-left-assoc-infix-operator> (rename '<) COMPARISON-BINDING-POWER))

(define GREATER-THAN-TOKEN
  (make-<symmetric-left-assoc-infix-operator> (rename '>) COMPARISON-BINDING-POWER))

(define LESS-THAN-EQUAL-TO-TOKEN
  (make-<symmetric-left-assoc-infix-operator> (rename '<=) COMPARISON-BINDING-POWER))

(define GREATER-THAN-EQUAL-TO-TOKEN
  (make-<symmetric-left-assoc-infix-operator> (rename '>=) COMPARISON-BINDING-POWER))

(define EQUAL-TO-TOKEN
  (make-<symmetric-left-assoc-infix-operator> (rename '=) COMPARISON-BINDING-POWER))

(define NOT-EQUAL-TO-TOKEN
  (make-<symmetric-left-assoc-infix-operator> (rename '!=) COMPARISON-BINDING-POWER))

;;; --------------------------------------------------------------------

(define FXLESS-THAN-TOKEN
  (make-<symmetric-left-assoc-infix-operator> (rename 'fx<) COMPARISON-BINDING-POWER))

(define FXGREATER-THAN-TOKEN
  (make-<symmetric-left-assoc-infix-operator> (rename 'fx>) COMPARISON-BINDING-POWER))

(define FXLESS-THAN-EQUAL-TO-TOKEN
  (make-<symmetric-left-assoc-infix-operator> (rename 'fx<=) COMPARISON-BINDING-POWER))

(define FXGREATER-THAN-EQUAL-TO-TOKEN
  (make-<symmetric-left-assoc-infix-operator> (rename 'fx>=) COMPARISON-BINDING-POWER))

(define FXEQUAL-TO-TOKEN
  (make-<symmetric-left-assoc-infix-operator> (rename 'fx=) COMPARISON-BINDING-POWER))

;;; --------------------------------------------------------------------

;; (define FLLESS-THAN-TOKEN
;;   (make-<symmetric-left-assoc-infix-operator> (rename 'fl<) COMPARISON-BINDING-POWER))

;; (define FLGREATER-THAN-TOKEN
;;   (make-<symmetric-left-assoc-infix-operator> (rename 'fl>) COMPARISON-BINDING-POWER))

;; (define FLLESS-THAN-EQUAL-TO-TOKEN
;;   (make-<symmetric-left-assoc-infix-operator> (rename 'fl<=) COMPARISON-BINDING-POWER))

;; (define FLGREATER-THAN-EQUAL-TO-TOKEN
;;   (make-<symmetric-left-assoc-infix-operator> (rename 'fl>=) COMPARISON-BINDING-POWER))

;; (define FLEQUAL-TO-TOKEN
;;   (make-<symmetric-left-assoc-infix-operator> (rename 'fl=) COMPARISON-BINDING-POWER))

;;; --------------------------------------------------------------------

(define EQ-PRED-TOKEN
  (make-<symmetric-left-assoc-infix-operator> (rename 'eq?) COMPARISON-BINDING-POWER))

(define EQV-PRED-TOKEN
  (make-<symmetric-left-assoc-infix-operator> (rename 'eqv?) COMPARISON-BINDING-POWER))

(define EQUAL-PRED-TOKEN
  (make-<symmetric-left-assoc-infix-operator> (rename 'equal?) COMPARISON-BINDING-POWER))

(define BITWISE-AND-TOKEN
  (make-<symmetric-left-assoc-infix-operator> (rename 'bitwise-and) INFIX-BITWISE-BINDING-POWER))

;;; --------------------------------------------------------------------

(define BITWISE-IOR-TOKEN
  (make-<symmetric-left-assoc-infix-operator> (rename 'bitwise-ior) INFIX-BITWISE-BINDING-POWER))

(define BITWISE-XOR-TOKEN
  (make-<symmetric-left-assoc-infix-operator> (rename 'bitwise-xor) INFIX-BITWISE-BINDING-POWER))

(define BITWISE-NOT-TOKEN
  (make-<prefix-operator> (rename 'bitwise-not) PREFIX-BITWISE-BINDING-POWER))

(define BITWISE-SHIFT-LEFT-TOKEN
  (make-<symmetric-left-assoc-infix-operator> (rename 'bitwise-arithmetic-shift-left) INFIX-BITSHIFT-BINDING-POWER))

(define BITWISE-SHIFT-RIGHT-TOKEN
  (make-<symmetric-left-assoc-infix-operator> (rename 'bitwise-arithmetic-shift-right) INFIX-BITSHIFT-BINDING-POWER))

;;; --------------------------------------------------------------------

(define FXAND-TOKEN
  (make-<symmetric-left-assoc-infix-operator> (rename 'fxand) INFIX-BITWISE-BINDING-POWER))

(define FXIOR-TOKEN
  (make-<symmetric-left-assoc-infix-operator> (rename 'fxior) INFIX-BITWISE-BINDING-POWER))

(define FXXOR-TOKEN
  (make-<symmetric-left-assoc-infix-operator> (rename 'fxxor) INFIX-BITWISE-BINDING-POWER))

(define FXNOT-TOKEN
  (make-<prefix-operator> (rename 'fxnot) PREFIX-BITWISE-BINDING-POWER))

(define FXSHIFT-LEFT-TOKEN
  (make-<symmetric-left-assoc-infix-operator> (rename 'fxarithmetic-shift-left) INFIX-BITSHIFT-BINDING-POWER))

(define FXSHIFT-RIGHT-TOKEN
  (make-<symmetric-left-assoc-infix-operator> (rename 'fxarithmetic-shift-right) INFIX-BITSHIFT-BINDING-POWER))

;;; --------------------------------------------------------------------

(define LEFT-PAREN-TOKEN
  (make-<left-paren>))

(define RIGHT-PAREN-TOKEN
  (make-<right-paren>))

(define COMMA-TOKEN
  (make-<comma>))

(define QUESTION-MARK-TOKEN
  (make-<question-mark>))

(define COLON-TOKEN
  (make-<colon>))


(define (tokenise obj)
  ;;Given a  raw object  from the flattened  list of  input tokens return  an instance  of "<token>"
  ;;describing it.
  ;;
  (cond ((identifier? obj)
	 (case (strip-syntax obj)
	   ((++)	INCR-TOKEN)
	   ((--)	DECR-TOKEN)

	   ((==)	EQUAL-TO-TOKEN)
	   ((!=)	NOT-EQUAL-TO-TOKEN)
	   ((<>)	NOT-EQUAL-TO-TOKEN)
	   ((≠)		NOT-EQUAL-TO-TOKEN) ;;Unicode symbol "\x2260;".
	   ((!)		BANG-TOKEN)
	   ((**)	EXPT-TOKEN)
	   ;;((%)		IMOD-TOKEN)
	   ((×)		MUL-TOKEN) ;;Unicode symbol "\xD7;".
	   ((⋅)		MUL-TOKEN) ;;Unicode symbol "\x22C5;".

	   ((&&)	AND-TOKEN)
	   ((⏐⏐)	OR-TOKEN)	;;Symbol "\x23D0;\x23D0;".
	   ((∧)	AND-TOKEN)	;;Unicode symbol "\x2227;".
	   ((∨)	OR-TOKEN)	;;Unicode symbol "\x2228;".
	   ((⊻)	XOR-TOKEN)	;;Unicode symbol "\x22BB;".
	   ((¬)		NOT-TOKEN)	;;Unicode symbol "\xAC;".

	   ((⏐)		BITWISE-IOR-TOKEN) ;;Unicode symbol "\x23D0;".
	   ((&)		BITWISE-AND-TOKEN)
	   ((^)		BITWISE-XOR-TOKEN)
	   ((~)		BITWISE-NOT-TOKEN)
	   ((<<)	BITWISE-SHIFT-LEFT-TOKEN)
	   ((>>)	BITWISE-SHIFT-RIGHT-TOKEN)

	   ((?)		QUESTION-MARK-TOKEN)
	   ((:)		COLON-TOKEN)
	   (else
	    (case-identifiers obj
	      ((+)		PLUS-TOKEN)
	      ((-)		MINUS-TOKEN)
	      ((*)		MUL-TOKEN)
	      ((/)		DIV-TOKEN)
	      ;; ((div)		IDIV-TOKEN)
	      ;; ((mod)		IMOD-TOKEN)
	      ;; ((div0)		IDIV0-TOKEN)
	      ;; ((mod0)		IMOD0-TOKEN)
	      ((expt)		EXPT-TOKEN)
	      ((factorial)	FACTORIAL-TOKEN)

	      ((fx+)		FXPLUS-TOKEN)
	      ((fx-)		FXMINUS-TOKEN)
	      ((fx*)		FXMUL-TOKEN)
	      ;; ((fxdiv)		FXDIV-TOKEN)
	      ;; ((fxmod)		FXMOD-TOKEN)
	      ;; ((fxdiv0)		FXDIV0-TOKEN)
	      ;; ((fxmod0)		FXMOD0-TOKEN)

	      ;; ((fl+)		FLPLUS-TOKEN)
	      ;; ((fl-)		FLMINUS-TOKEN)
	      ;; ((fl*)		FLMUL-TOKEN)
	      ;; ((fl/)		FLDIV-TOKEN)
	      ;; ((flexpt)		FLEXPT-TOKEN)

	      ((and)		AND-TOKEN)
	      ((or)		OR-TOKEN)
	      ((xor)		XOR-TOKEN)
	      ((not)		NOT-TOKEN)

	      ((<)		LESS-THAN-TOKEN)
	      ((>)		GREATER-THAN-TOKEN)
	      ((<=)		LESS-THAN-EQUAL-TO-TOKEN)
	      ((>=)		GREATER-THAN-EQUAL-TO-TOKEN)
	      ((=)		EQUAL-TO-TOKEN)

	      ((fx<)		FXLESS-THAN-TOKEN)
	      ((fx>)		FXGREATER-THAN-TOKEN)
	      ((fx<=)		FXLESS-THAN-EQUAL-TO-TOKEN)
	      ((fx>=)		FXGREATER-THAN-EQUAL-TO-TOKEN)
	      ((fx=)		FXEQUAL-TO-TOKEN)

	      ;; ((fl<)		FLLESS-THAN-TOKEN)
	      ;; ((fl>)		FLGREATER-THAN-TOKEN)
	      ;; ((fl<=)		FLLESS-THAN-EQUAL-TO-TOKEN)
	      ;; ((fl>=)		FLGREATER-THAN-EQUAL-TO-TOKEN)
	      ;; ((fl=)		FLEQUAL-TO-TOKEN)

	      ((eq?)		EQ-PRED-TOKEN)
	      ((eqv?)		EQV-PRED-TOKEN)
	      ((equal?)		EQUAL-PRED-TOKEN)

	      ;; bitwise operators
	      ((bitwise-and)			BITWISE-AND-TOKEN)
	      ((bitwise-ior)			BITWISE-IOR-TOKEN)
	      ((bitwise-xor)			BITWISE-XOR-TOKEN)
	      ((bitwise-not)			BITWISE-NOT-TOKEN)
	      ((bitwise-arithmetic-shift-left)	BITWISE-SHIFT-LEFT-TOKEN)
	      ((bitwise-arithmetic-shift-right)	BITWISE-SHIFT-RIGHT-TOKEN)

	      ;; bitwise operators
	      ((fxand)				FXAND-TOKEN)
	      ((fxior)				FXIOR-TOKEN)
	      ((fxxor)				FXXOR-TOKEN)
	      ((fxnot)				FXNOT-TOKEN)
	      ((fxarithmetic-shift-left)	FXSHIFT-LEFT-TOKEN)
	      ((fxarithmetic-shift-right)	FXSHIFT-RIGHT-TOKEN)

	      (else
	       ;;It is a variable.
	       (make-<operand> obj))))))
	((char? obj)
	 (cond ((char=? obj #\()	LEFT-PAREN-TOKEN)
	       ((char=? obj #\))	RIGHT-PAREN-TOKEN)
	       ((char=? obj #\,)	COMMA-TOKEN)
	       (else
		(synner "invalid character object from input" obj))))
	(else
	 (make-<operand> obj))))


;;;; transformer and helpers

(define (main input-form.stx)
  ;;This is the INFIX syntax transformer.
  ;;
  (match input-form.stx
    ((_ . ?tokens)
     (receive (unused obj*)
	 (flatten-input ?tokens)
       (let* ((obj*   (reverse obj*))
	      (token* (map (lambda (obj)
			     (tokenise obj))
			obj*))
	      (lexer  (make-lexer token*))
	      (expr   (parse lexer MINIMUM-BINDING-POWER
			     immediate-end-of-input-handler)))
	 expr)))
    ))


;;;; the syntax transformer

(main input-form.stx))))


;;;; done

#| end of module |# )

;;; end of file
