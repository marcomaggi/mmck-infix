;;; -*- coding: utf-8-unix  -*-
;;;
;;;Part of: MMCK Infix
;;;Contents: functions module
;;;Date: Jul 10, 2019
;;;
;;;Abstract
;;;
;;;	This file implements  additional functions and syntaxes  used in the expansion  of the INFIX
;;;	syntax.
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

(declare (unit mmck.infix.funcs)
	 (uses mmck.infix.helpers)
	 (emit-import-library mmck.infix.funcs))

(module (mmck.infix.funcs)
    ((syntax: xor)
     (syntax: pre-incr +)
     (syntax: pre-decr -)
     (syntax: post-incr +)
     (syntax: post-decr -)
     factorial
     !=)
  (import (scheme)
	  (mmck infix helpers))
  (import-for-syntax (scheme)
		     (mmck infix helpers)
		     (only (matchable)
			   match))


;;;; additional functions

(define (factorial N)
  (assert (and (integer? N)
	       (not (negative? N))))
  (let recur ((N N)
	      (R 1))
    (if (zero? N)
	R
      (recur (- N 1) (* N R)))))

(case-define !=
  ((arg)
   #f)
  ((arg1 arg2)
   (not (= arg1 arg2)))
  ((arg1 arg2 . arg*)
   (not (apply = arg1 arg2 arg*))))


;;;; pre increment and decrement operators

(define-syntax pre-incr
  ;;Pre-increment macro.  We want the following expansions:
  ;;
  ;;   (infix ++ var)
  ;;   ==> (pre-incr var)
  ;;   ==> (begin
  ;;         (set! var (+ var 1))
  ;;         var)
  ;;
  ;;   (infix ++ (3 * var))
  ;;   ==> (pre-incr (3 * var))
  ;;   ==> (+ (3 * var) 1)
  ;;
  (er-macro-transformer
    (lambda (input-form.stx rename compare)
      (match input-form.stx
	((_ (? symbol? ?id))
	 (let ((%begin	(rename 'begin))
	       (%set!	(rename 'set!))
	       (%+	(rename '+)))
	   `(,%begin
	     (,%set! ,?id (,%+ ,?id 1))
	     ,?id)))
	((_ ?expr)
	 (let ((%+	(rename '+)))
	   `(,%+ ?expr 1)))
	(_
	 (syntax-error 'pre-incr "invalid pre-increment operation" input-form.stx))))))

(define-syntax pre-decr
  ;;Pre-decrement macro.  We want the following expansions:
  ;;
  ;;   (infix -- var)
  ;;   ==> (pre-incr var)
  ;;   ==> (begin
  ;;         (set! var (- var 1))
  ;;         var)
  ;;
  ;;   (infix ++ (3 * var))
  ;;   ==> (pre-incr (3 * var))
  ;;   ==> (- (3 * var) 1)
  ;;
  (er-macro-transformer
    (lambda (input-form.stx rename compare)
      (match input-form.stx
	((_ (? symbol? ?id))
	 (let ((%begin	(rename 'begin))
	       (%set!	(rename 'set!))
	       (%-	(rename '-)))
	   `(,%begin
	     (,%set! ,?id (,%- ,?id 1))
	     ,?id)))
	((_ ?expr)
	 (let ((%-	(rename '-)))
	   `(,%- ?expr 1)))
	(_
	 (syntax-error 'pre-decr "invalid pre-decrement operation" input-form.stx))))))


;;;; post increment and decrement operators

(define-syntax post-incr
  ;;Post-increment macro.  We want the following expansions:
  ;;
  ;;   (infix var ++)
  ;;   ==> (post-incr var)
  ;;   ==> (begin0
  ;;         var
  ;;         (set! var (+ var 1)))
  ;;
  ;;   (infix (3 * var) ++)
  ;;   ==> (post-incr (3 * var))
  ;;   ==> (+ (3 * var) 1)
  ;;
  (er-macro-transformer
    (lambda (input-form.stx rename compare)
      (match input-form.stx
	((_ (? symbol? ?id))
	 (let ((%begin0	(rename 'begin0))
	       (%set!	(rename 'set!))
	       (%+	(rename '+)))
	   `(,%begin0
	     ,?id
	     (,%set! ,?id (,%+ ,?id 1)))))
	((_ ?expr)
	 (let ((%+	(rename '+)))
	   `(,%+ ?expr 1)))
	(_
	 (syntax-error 'post-incr "invalid post-increment operation" input-form.stx))))))

(define-syntax post-decr
  ;;Post-decrement macro.  We want the following expansions:
  ;;
  ;;   (infix var --)
  ;;   ==> (post-incr var)
  ;;   ==> (begin0
  ;;         var
  ;;         (set! var (- var 1)))
  ;;
  ;;   (infix (3 * var) --)
  ;;   ==> (post-incr (3 * var))
  ;;   ==> (- (3 * var) 1)
  ;;
  (er-macro-transformer
    (lambda (input-form.stx rename compare)
      (match input-form.stx
	((_ (? symbol? ?id))
	 (let ((%begin0	(rename 'begin0))
	       (%set!	(rename 'set!))
	       (%-	(rename '-)))
	   `(,%begin0
	     ,?id
	     (,%set! ,?id (,%- ,?id 1)))))
	((_ ?expr)
	 (let ((%-	(rename '-)))
	   `(,%- ?expr 1)))
	(_
	 (syntax-error 'post-decr "invalid post-decrement operation" input-form.stx))))))


;;;; XOR macro

(define-syntax xor
  (syntax-rules ()
    ((_ expr ...)
     (xor-aux #F expr ...))))

(define-syntax xor-aux
  (syntax-rules ()
    ((_ r)
     r)
    ((_ r expr)
     (let ((x expr))
       (if r
           (and (not x) r)
	 x)))
    ((_ r expr0 expr ...)
     (let ((x expr0))
       (and (or (not r) (not x))
	    (let ((n (or r x)))
	      (xor-aux n expr ...)))))))


;;;; done

#| end of module |# )

;;; end of file
