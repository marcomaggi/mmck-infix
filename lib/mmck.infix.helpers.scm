;;; -*- coding: utf-8-unix  -*-
;;;
;;;Part of: MMCK Infix
;;;Contents: helpers module
;;;Date: Jul 10, 2019
;;;
;;;Abstract
;;;
;;;	This unit defines the helpers module.
;;;
;;;Copyright (C) 2019 Marco Maggi <mrc.mgg@gmail.com>
;;;All rights reserved.
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


;;;; units and module header

(declare (unit mmck.infix.helpers)
	 (emit-import-library mmck.infix.helpers))

(module (mmck.infix.helpers)
    ((syntax: define-syntax-rule)
     (syntax: define*)
     (syntax: case-define)
     (syntax: case-define*)
     (syntax: receive-and-return apply values call-with-values)
     (syntax: begin0)
     (syntax: internal-body)
     (syntax: define-auxiliary-syntaxes)

     debug-print)
  (import (scheme)
	  (only (chicken module)
		reexport))
  (reexport (only (chicken base)
		  assert
		  case-lambda
		  current-error-port
		  error
		  parameterize
		  receive
		  warning)
	    (only (chicken pretty-print)
		  pretty-print
		  pretty-print-width)
	    (only (chicken syntax)
		  syntax-error
		  er-macro-transformer))
  (import-for-syntax (scheme)
		     (only (chicken syntax)
			   syntax-error
			   er-macro-transformer)
		     (only (matchable)
			   match))


;;;; syntaxes

(define-syntax define-syntax-rule
  (syntax-rules ()
    ((_ (?name . ?args) ?body0 ?body ...)
     (define-syntax ?name
       (syntax-rules ()
	 ((_ . ?args)
	  (begin ?body0 ?body ...)))))))

(define-syntax case-define
  (syntax-rules ()
    ((_ ?who (?formals ?body0 ?body ...) ...)
     (define ?who
       (case-lambda (?formals ?body0 ?body ...) ...)))
    ))

(define-syntax case-define*
  (syntax-rules ()
    ((_ ?who (?formals ?body0 ?body ...) ...)
     (define* ?who
       (case-lambda (?formals ?body0 ?body ...) ...)))
    ))

(define-syntax internal-body
  (syntax-rules ()
    ((_ ?body0 ?body ...)
     (let () ?body0 ?body ...))
    ))

(define-syntax define-auxiliary-syntaxes
  (syntax-rules ()
    ((_ ?name0)
     (define-syntax ?name0 (syntax-rules ())))
    ((_ ?name0 ?name1 ?name ...)
     (begin
       (define-auxiliary-syntaxes ?name0)
       (define-auxiliary-syntaxes ?name1 ?name ...)))
    ))


;;;; more syntaxes

(define-syntax receive-and-return
  (ir-macro-transformer
    (lambda (input-form.stx inject compare)
      (define (synner message . args)
	(apply syntax-error 'receive-and-return message input-form.stx args))

      (match input-form.stx
	((_ ((? symbol? ?single-formal)) ?expr ?body0 ?body* ...)
	 `(let ((,?single-formal ,?expr))
	    (begin ,?body0 ,@?body*)
	    ,?single-formal))

	((_ (?formal0 ?formal* ...) ?expr ?body0 ?body* ...)
	 `(call-with-values
	      (lambda () ,?expr)
	    (lambda (,?formal0 ,@?formal* ...)
	      (begin ,?body0 ,@?body*)
	      (values ,?formal0 ,@?formal* ...))))

	((_ (?formal0 . ?formals-rest) ?expr ?body0 ?body* ...)
	 `(call-with-values
	      (lambda () ,?expr)
	    (lambda (,?formal0 ,@?formal* ...)
	      (begin ,?body0 ,@?body)
	      (values ,?formal0 . ,?formals-rest))))

	((_ (? symbol? ?formals-args) ?expr ?body0 ?body* ...)
	 `(call-with-values
	      (lambda () ,?expr)
	    (lambda ,?formals-args
	      (begin ,?body0 ,@?body*)
	      (apply values ,?formals-args))))

	(_
	 (synner "invalid syntax"))))))

(define-syntax begin0
  (syntax-rules ()
    ((_ ?form0 ?form1 ?form ...)
     (receive-and-return args
	 ?form0
       (begin ?form1 ?form ...)))
    ))

;;; --------------------------------------------------------------------

(define-syntax define*
  (ir-macro-transformer
    (lambda (input-form.stx inject compare)
      (match input-form.stx
	((_ (?who . ?formals) ?body0 ?body* ...)
	 (let ((%__who__ (inject '__who__)))
	   `(define (,?who . ,?formals)
	      (let ((,%__who__ (quote ,?who)))
		,?body0 ,@?body*))))

	((_ ?who ?expr)
	 (let ((%__who__ (inject '__who__)))
	   `(define ,?who
	      (let ((,%__who__ (quote ,?who)))
		,?expr))))
	))))


;;;; list processing

(define (fold-left combine nil ell)
  (if (pair? ell)
      (fold-left combine (combine nil (car ell)) (cdr ell))
    nil))

(define (fold-right combine nil ell)
  (let loop ((combine	combine)
	     (nil	nil)
	     (ell	(reverse ell)))
    (if (pair? ell)
	(loop combine (combine (car ell) nil) (cdr ell))
      nil)))


;;;; miscellaneous

(define (debug-print . args)
  (parameterize ((pretty-print-width 150))
    (pretty-print args (current-error-port))))


;;;; done

#| end of module |# )

;;; end of file

