;;; -*- coding: utf-8-unix  -*-
;;;
;;;Part of: MMCK Infix
;;;Contents: test program for demo
;;;Date: Jul  9, 2019
;;;
;;;Abstract
;;;
;;;	This program is a demo of the features.
;;;
;;;Copyright (C) 2019 Marco Maggi <mrc.mgg@gmail.com>
;;;
;;;This program is free software: you can  redistribute it and/or modify it under the
;;;terms of the GNU  Lesser General Public License as published  by the Free Software
;;;Foundation,  either version  3  of the  License,  or (at  your  option) any  later
;;;version.
;;;
;;;This program is  distributed in the hope  that it will be useful,  but WITHOUT ANY
;;;WARRANTY; without  even the implied warranty  of MERCHANTABILITY or FITNESS  FOR A
;;;PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more details.
;;;
;;;You should  have received a  copy of the GNU  Lesser General Public  License along
;;;with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


;;;; units and module header

(require-library (mmck infix)
		 (mmck checks))

(module (test-demo)
    ()
  (import (scheme)
	  (chicken base)
	  (chicken fixnum)
	  (chicken flonum)
	  (chicken bitwise)
	  (mmck infix)
	  (mmck checks))

(check-set-mode! 'report-failed)
(check-display "*** testing INFIX macro\n")


;;;; numbers

(check (begin (infix) #f)	=> #f)

;;; integers

(check (infix 1)	=> 1)
(check (infix -1)	=> -1)
(check (infix +1)	=> 1)

;;; reals

(check (infix 1.1)		=> 1.1)
(check (infix -1.1)		=> -1.1)
(check (infix +1.1)		=> +1.1)
(check (infix 1.1e10)		=> 1.1e10)
(check (infix 1.1E10)		=> 1.1e10)
(check (infix 1.1e-10)		=> 1.1e-10)
(check (infix 1.1E-10)		=> 1.1e-10)
(check (infix 1e10)		=> 1e10)
(check (infix 1E10)		=> 1e10)
(check (infix 1e-10)		=> 1e-10)
(check (infix 1E-10)		=> 1e-10)

(check (infix .0)		=> 0.0)
(check (infix -.0)		=> -0.0)
(check (infix 0.)		=> 0.0)

;;; complexes

(check (infix +1i)		=> +1i)
(check (infix -1i)		=> -1i)
(check (infix +1.1i)		=> +1.1i)
(check (infix -1.1i)		=> -1.1i)
(check (infix +.1i)		=> +0.1i)
(check (infix -.1i)		=> -0.1i)

;;; nan and infinity

(check (nan? (infix +nan.0))	=> #t)
(check (nan? (infix -nan.0))	=> #t)
(check (infix +inf.0)		=> +inf.0)
(check (infix -inf.0)		=> -inf.0)


;;;; arithmetic operators

(check (infix 1 + 2)		=> (+ 1 2))
(check (infix 1 + 2 + 3)	=> (+ (+ 1 2) 3))
(check (infix 1 + 2 - 3)	=> (- (+ 1 2) 3))
(check (infix 1 + (2 + 3))	=> (+ 1 (+ 2 3)))
(check (infix 1 + (2 - 3))	=> (+ 1 (- 2 3)))

(check (infix 1 * 1)		=> (* 1 1))
(check (infix 1 * 2 * 3)	=> (* (* 1 2) 3))
(check (infix 1 * 2 / 3)	=> (/ (* 1 2) 3))
(check (infix 1 * (2 * 3))	=> (* 1 (* 2 3)))
(check (infix 1 * (2 / 3))	=> (* 1 (/ 2 3)))

(check (infix 1 × 1)		=> (* 1 1))
(check (infix 1 × 2 × 3)	=> (* (* 1 2) 3))
(check (infix 1 × 2 / 3)	=> (/ (* 1 2) 3))
(check (infix 1 × (2 × 3))	=> (* 1 (* 2 3)))
(check (infix 1 × (2 / 3))	=> (* 1 (/ 2 3)))

(check (infix 1 ⋅ 1)		=> (* 1 1))
(check (infix 1 ⋅ 2 ⋅ 3)	=> (* (* 1 2) 3))
(check (infix 1 ⋅ 2 / 3)	=> (/ (* 1 2) 3))
(check (infix 1 ⋅ (2 ⋅ 3))	=> (* 1 (* 2 3)))
(check (infix 1 ⋅ (2 / 3))	=> (* 1 (/ 2 3)))

(check (infix 1 + 2 * 3)	=> (+ 1 (* 2 3)))
(check (infix 1 - 2 * 3)	=> (- 1 (* 2 3)))
(check (infix 1 + 2 / 3)	=> (+ 1 (/ 2 3)))
(check (infix 1 - 2 / 3)	=> (- 1 (/ 2 3)))

(check (infix 1 * 2 + 3)	=> (+ (* 1 2) 3))
(check (infix 1 * 2 - 3)	=> (- (* 1 2) 3))
(check (infix 1 / 2 + 3)	=> (+ (/ 1 2) 3))
(check (infix 1 / 2 - 3)	=> (- (/ 1 2) 3))

(check (infix - 2)		=> (- 2))
(check (infix (- 2))		=> (- 2))
(check (infix (1 + (- 2)))	=> (+ 1 (- 2)))
(let ((a 2))
  (check (infix (- a))		=> (- 2))
  (check (infix (1 + (- a)))	=> (+ 1 (- 2)))
  #f)

(check (infix 1 % 3)		=> (modulo 1 3))
(check (infix 10 modulo 3)	=> (modulo 10 3))

(check (infix 1 quotient 3)	=> (quotient 1 3))
(check (infix 10 remainder 3)	=> (remainder 10 3))

(check (infix 1 expt 3)		=> (expt 1 3))
(check (infix 10 expt 3)	=> (expt 10 3))

;; EXPT is right-associative
(check (infix 10 expt 5 expt 3)	=> (expt 10 (expt 5 3)))
(check (infix 10 ** 5 ** 3)	=> (expt 10 (expt 5 3)))

(check (infix 5 factorial)	=> 120)
(check (infix 0 !)		=> 1)
(check (infix 1 !)		=> 1)
(check (infix 2 !)		=> 2)
(check (infix 3 !)		=> (* 3 2 1))
(check (infix 5 !)		=> 120)
(check (infix 4 + 5 !)		=> (+ 4 120))
(check (infix 2 * 3 !)		=> (* 2 (factorial 3)))
(check (infix (2 * 3) !)	=> (factorial (* 2 3)))
(check (infix 5 ! + 4)		=> (+ 4 120))
(check (infix 5 ! * 4)		=> (* 4 120))
(check (infix - 5 !)		=> -120)
(check (infix + 5 !)		=> +120)

;;; --------------------------------------------------------------------

(check (infix ++ 10)	=> 11)
(check (infix 10 ++)	=> 11)
(check (infix -- 10)	=> 9)
(check (infix 10 --)	=> 9)

(check
    (let ((x 10))
      (let ((r (infix ++ x)))
	(list r x)))
  => '(11 11))

(check
    (let ((x 10))
      (let ((r (infix 2 + ++ x)))
	(list r x)))
  => '(13 11))

(check
    (let ((x 10))
      (let ((r (infix 2 + (++ x))))
	(list r x)))
  => '(13 11))

(check
    (let ((x 10))
      (let ((r (infix x ++)))
	(list r x)))
  => '(10 11))

(check
    (let ((x 10))
      (let ((r (infix -- x)))
	(list r x)))
  => '(9 9))

(check
    (let ((x 10))
      (let ((r (infix x --)))
	(list r x)))
  => '(10 9))


;;;; comparison operators

(check (infix 1 < 3)		=> (<  1 3))
(check (infix 1 > 3)		=> (>  1 3))
(check (infix 1 <= 3)		=> (<= 1 3))
(check (infix 1 >= 3)		=> (>= 1 3))
(check (infix 1 = 3)		=> (=  1 3))
(check (infix 1 != 3)		=> (not (=  1 3)))
(check (infix 1 <> 3)		=> (not (=  1 3)))
(check (infix 1 ≠ 3)		=> (not (=  1 3)))

;;FIXME This fails with Sagittarius.
(check
    (infix 'a eq? 'a)
  => (eq? 'a 'a))

;;FIXME This fails with Sagittarius.
(check
    (infix 'a eq? 'b)
  => (eq? 'a 'b))

(check (infix 123 eqv? 123)	=> (eqv? 123 123))
(check (infix 123 eqv? 456)	=> (eqv? 123 456))

(check (infix 123 equal? 123)	=> (equal? 123 123))
(check (infix 123 equal? 456)	=> (equal? 123 456))


;;;; functions

(let ()

  (define (fun a b c)
    (+ a b c))

  (check
      (infix fun (1 , 2 , 3))
    => (fun 1 2 3))

  (check
      (infix fun(1 , 2 , 3))
    => (fun 1 2 3))

  (check
      (infix (fun (1 , 2 , 3)))
    => (fun 1 2 3))

  (check
      (infix fun(1, 2, 3))
    => (fun 1 2 3))

  (values))

(let ()
  (define (noargs)
    1)
  (check (infix noargs())		=> 1)
  (check (infix 2 + noargs() + 3)	=> 6)
  (values))

(check (infix sin (1.1))		=> (sin 1.1))

(check (infix cos (sin (1.1)))		=> (cos (sin 1.1)))
(check (infix cos (sin (1.1) + 4))	=> (cos (+ (sin 1.1) 4)))

(check
    (infix 1 + 23e-45 + 0.006789e2 * (4.113 + +23i) / sin (0.5) + atan (0.1 , 0.2))
  => (+ (+ (+ 1 23e-45) (/ (* 0.006789e2 (+ 4.113 +23i)) (sin 0.5))) (atan 0.1 0.2)))

(let ()
  (define a 1.1)
  (define b 2.2)
  (define c 3.3)
  (check
      (infix cos(a) * tan(b) / c)
    => (/ (* (cos a) (tan b)) c)))


;;;; variables

(let ((a 1) (b 2) (c 3))
  (check (infix a * 1.1)	=> (* a 1.1))
  (check (infix (a * b) / c)	=> (/ (* a b) c))
  (check (infix a * (b / c))	=> (* a (/ b c)))

  (check (infix cos (a) * tan (b) / c)
    => (/ (* (cos a) (tan b)) c))

  (check (infix (cos (a) * tan (b) / c))
    => (/ (* (cos a) (tan b)) c))

  (values))


;;;; if-then-else

(let ((a 1) (b 2) (c 3))

  (check (infix a ? b : c)	=> (if a b c))
  (check (infix (1 ? b : c))	=> (if 1 b c))
  (check (infix (a ? 1 : c))	=> (if a 1 c))
  (check (infix (a ? b : 1))	=> (if a b 1))
  (check (infix (1 ? 2 : 3))	=> (if 1 2 3))
  (check (infix #f ? 2 : 3)	=> (if #f 2 3))
  (check (infix (#f ? 2 : 3))	=> (if #f 2 3))

  (check (infix (a * (b / a ? b : c)))
    => (* a (if (/ b a) b c)))

  (check (infix (1 + a ? 2 + b : 3 + c - 4))
    => (if (+ 1 a) (+ 2 b) (- (+ 3 c) 4)))

  (values))


;;;; nested prefix expressions

(check
    (infix (begin
	     (+ 1 2)))
  => (+ 1 2))

(check
    (infix (begin
	     (+ 1 2)
	     (+ 3 4)))
  => (+ 3 4))

(check
    (infix (begin
	     (let ((a 3))
	       (/ a 4))))
  => (/ 3 4))


(let ((a 3))
  (check
      (infix (begin
	       (/ a 4)))
    => (/ a 4)))

(check (infix (begin 1) + 2 * 3)	=> (+ 1 (* 2 3)))
(check (infix 1 - (begin 2) * 3)	=> (- 1 (* 2 3)))
(check (infix 1 + 2 / (begin 3))	=> (+ 1 (/ 2 3)))

(let ((a 1) (b 2) (c 3))
  (check (infix (1 + a ? (begin
			   (+ 2 b))
		   : 3 + c - 4))
    => (if (+ 1 a) (+ 2 b) (- (+ 3 c) 4)))
  #f)


;;;; logic operators

(check (infix 1 and 3)		=> 3)
(check (infix #f and 3)		=> #f)
(check (infix 1 and #f)		=> #f)
(check (infix 1 or 3)		=> 1)
(check (infix 1 or #f)		=> 1)
(check (infix #f or 1)		=> 1)
(check (infix 1 xor 3)		=> #f)
(check (infix 1 xor #f)		=> 1)
(check (infix #f xor 1)		=> 1)
(check (infix not 3)		=> #f)
(check (infix not #f)		=> #t)

(check (infix not #f and #t)	=> #t)
(check (infix not #t and #t)	=> #f)
(check (infix not #f or #t)	=> #t)
(check (infix not #t or #t)	=> #t)
(check (infix not #f or #f)	=> #t)
(check (infix not #t or #f)	=> #f)

;;This is fine with  Pratt but fails with LALR because LALR does  not allow ! to have
;;difference precedences when present in prefix and postfix positions.
;;
(check (infix ! 1 + 2)		=> #f)

(check (infix 1 && 3)		=> 3)
(check (infix #f && 3)		=> #f)
(check (infix 1 && #f)		=> #f)
(check (infix ! 3)		=> #f)
(check (infix ! #f)		=> #t)

(check (infix 1 ⏐⏐ 3)		=> 1)
(check (infix 1 ⏐⏐ #f)		=> 1)
(check (infix #f ⏐⏐ 1)		=> 1)

(check (infix 1 ∧ 3)		=> 3)
(check (infix 1 ∨ 3)		=> 1)
(check (infix 1 ⊻ 3)		=> #f)

(check (infix ¬ 3)		=> #f)
(check (infix ¬ #f)		=> #t)


;;;; bitwise operators

(let ((a #b0101) (b #b1111))
  (check
      (infix a & b)
    => (bitwise-and a b)))

(let ((a #b0101) (b #b1101))
  (check
      (infix a ⏐ b)
    => (bitwise-ior a b)))

(let ((a #b0111) (b #b1101))
  (check
      (infix a ^ b)
    => (bitwise-xor a b)))

(let ((a #b0101))
  (check
      (infix ~ a)
    => (bitwise-not a)))

(let ((a #b0111) (b 3))
  (check
      (infix a << b)
    => (bitwise-arithmetic-shift-left a b)))

(let ((a #b01110000) (b 3))
  (check
      (infix a >> b)
    => (bitwise-arithmetic-shift-right a b)))


;;;; fixnums

;;; arithmetic operators

(check (infix 1 fx+ 2)			=> (fx+ 1 2))
(check (infix 1 fx+ 2 fx+ 3)		=> (fx+ (fx+ 1 2) 3))
(check (infix 1 fx+ 2 fx- 3)		=> (fx- (fx+ 1 2) 3))
(check (infix 1 fx+ (2 fx+ 3))		=> (fx+ 1 (fx+ 2 3)))
(check (infix 1 fx+ (2 fx- 3))		=> (fx+ 1 (fx- 2 3)))

(check (infix 1 fx* 1)			=> (fx* 1 1))
(check (infix 1 fx* 2 fx* 3)		=> (fx* (fx* 1 2) 3))
(check (infix 1 fx* (2 fx* 3))		=> (fx* 1 (fx* 2 3)))

(check (infix 1 fx+ 2 fx* 3)		=> (fx+ 1 (fx* 2 3)))
(check (infix 1 fx- 2 fx* 3)		=> (fx- 1 (fx* 2 3)))

(check (infix 1 fx* 2 fx+ 3)		=> (fx+ (fx* 1 2) 3))
(check (infix 1 fx* 2 fx- 3)		=> (fx- (fx* 1 2) 3))

(check (infix fxneg 2)			=> (fxneg 2))
(check (infix (fxneg 2))		=> (fxneg 2))
(check (infix (1 fx+ (fxneg 2)))	=> (fx+ 1 (fxneg 2)))
(let ((a 2))
  (check (infix (fxneg a))		=> (fxneg 2))
  (check (infix (1 fx+ (fxneg a)))	=> (fx+ 1 (fxneg 2)))
  #f)

(check (infix 1 fx/ 3)			=> (fx/ 1 3))
(check (infix 1 fxmod 3)		=> (fxmod 1 3))
(check (infix 10 fxmod 3)		=> (fxmod 10 3))

;;; associativity

(check (infix 10 fx- 5 fx- 3)		=> (fx- (fx- 10 5) 3))
(check (infix 10 fx/ 5 fx/ 3)		=> (fx/ (fx/ 10 5) 3))

;;; comparison operators

(check (infix 1 fx< 3)			=> (fx<  1 3))
(check (infix 1 fx> 3)			=> (fx>  1 3))
(check (infix 1 fx<= 3)			=> (fx<= 1 3))
(check (infix 1 fx>= 3)			=> (fx>= 1 3))
(check (infix 1 fx= 3)			=> (fx=  1 3))

;; bitwise operations

(let ((a #b0101) (b #b1111))
  (check
      (infix a fxand b)
    => (fxand a b)))

(let ((a #b0101) (b #b1101))
  (check
      (infix a fxior b)
    => (fxior a b)))

(let ((a #b0111) (b #b1101))
  (check
      (infix a fxxor b)
    => (fxxor a b)))

(let ((a #b0101))
  (check
      (infix fxnot a)
    => (fxnot a)))

(let ((a #b0111) (b 3))
  (check
      (infix a fxshl b)
    => (fxshl a b)))

(let ((a #b01110000) (b 3))
  (check
      (infix a fxshr b)
    => (fxshr a b)))


;;;; flonums

;;; arithmetic operators

(check (infix 1. fp+ 2.)		=> (fp+ 1. 2.))
(check (infix 1. fp+ 2. fp+ 3.)		=> (fp+ (fp+ 1. 2.) 3.))
(check (infix 1. fp+ 2. fp- 3.)		=> (fp- (fp+ 1. 2.) 3.))
(check (infix 1. fp+ (2. fp+ 3.))	=> (fp+ 1. (fp+ 2. 3.)))
(check (infix 1. fp+ (2. fp- 3.))	=> (fp+ 1. (fp- 2. 3.)))

(check (infix 1. fp* 1.)		=> (fp* 1. 1.))
(check (infix 1. fp* 2. fp* 3.)		=> (fp* (fp* 1. 2.) 3.))
(check (infix 1. fp* 2. fp/ 3.)		=> (fp/ (fp* 1. 2.) 3.))
(check (infix 1. fp* (2. fp* 3.))	=> (fp* 1. (fp* 2. 3.)))
(check (infix 1. fp* (2. fp/ 3.))	=> (fp* 1. (fp/ 2. 3.)))

(check (infix 1. fp+ 2. fp* 3.)		=> (fp+ 1. (fp* 2. 3.)))
(check (infix 1. fp- 2. fp* 3.)		=> (fp- 1. (fp* 2. 3.)))
(check (infix 1. fp+ 2. fp/ 3.)		=> (fp+ 1. (fp/ 2. 3.)))
(check (infix 1. fp- 2. fp/ 3.)		=> (fp- 1. (fp/ 2. 3.)))

(check (infix 1. fp* 2. fp+ 3.)		=> (fp+ (fp* 1. 2.) 3.))
(check (infix 1. fp* 2. fp- 3.)		=> (fp- (fp* 1. 2.) 3.))
(check (infix 1. fp/ 2. fp+ 3.)		=> (fp+ (fp/ 1. 2.) 3.))
(check (infix 1. fp/ 2. fp- 3.)		=> (fp- (fp/ 1. 2.) 3.))

;; left associativity
(check (infix 10. fp- 5. fp- 3.)	=> (fp- (fp- 10. 5.) 3.))
(check (infix 10. fp/ 5. fp/ 3.)	=> (fp/ (fp/ 10. 5.) 3.))

(check (infix fpneg 2.)			=> (fpneg 2.))
(check (infix (fpneg 2.))			=> (fpneg 2.))
(check (infix (1. fp+ (fpneg 2.)))	=> (fp+ 1. (fpneg 2.)))
(let ((a 2.))
  (check (infix (fpneg a))		=> (fpneg 2.))
  (check (infix (1. fp+ (fpneg a)))	=> (fp+ 1. (fpneg 2.)))
  #f)

(check (infix 1.0 fpexpt 3.)		=> (fpexpt 1.0 3.))
;; FPEXPT is right-associative
(check (infix 10. fpexpt 5. fpexpt 3.)	=> (fpexpt 10. (fpexpt 5. 3.)))

;;; comparison operators

(check (infix 1. fp< 3.)		=> (fp<  1. 3.))
(check (infix 1. fp> 3.)		=> (fp>  1. 3.))
(check (infix 1. fp<= 3.)		=> (fp<= 1. 3.))
(check (infix 1. fp>= 3.)		=> (fp>= 1. 3.))
(check (infix 1. fp= 3.)		=> (fp=  1. 3.))


;;;; precedence

;;;In order of increasing precedence from left to right:
;;;
;;; + - * / quotient modulo expt < > <= >= =

(check (infix 11 + 22 * 33)		=> (+ 11 (* 22 33)))
(check (infix 11 - 22 * 33)		=> (- 11 (* 22 33)))
(check (infix 22 * 33 + 11)		=> (+ (* 22 33) 11))
(check (infix 22 * 33 - 11)		=> (- (* 22 33) 11))

(check (infix 11 + 22 / 33)		=> (+ 11 (/ 22 33)))
(check (infix 11 - 22 / 33)		=> (- 11 (/ 22 33)))
(check (infix 22 / 33 + 11)		=> (+ (/ 22 33) 11))
(check (infix 22 / 33 - 11)		=> (- (/ 22 33) 11))

;;; --------------------------------------------------------------------

(check (infix 11 + 22 quotient 33)		=> (+ 11 (quotient 22 33)))
(check (infix 11 - 22 quotient 33)		=> (- 11 (quotient 22 33)))
(check (infix 22 quotient 33 + 11)		=> (+ (quotient 22 33) 11))
(check (infix 22 quotient 33 - 11)		=> (- (quotient 22 33) 11))

(check (infix 11 + 22 modulo 33)		=> (+ 11 (modulo 22 33)))
(check (infix 11 - 22 modulo 33)		=> (- 11 (modulo 22 33)))
(check (infix 22 modulo 33 + 11)		=> (+ (modulo 22 33) 11))
(check (infix 22 modulo 33 - 11)		=> (- (modulo 22 33) 11))

;;; --------------------------------------------------------------------

(check (infix 11 + 22 expt 33)		=> (+ 11 (expt 22 33)))
(check (infix 11 - 22 expt 33)		=> (- 11 (expt 22 33)))
(check (infix 22 expt 33 + 11)		=> (+ (expt 22 33) 11))
(check (infix 22 expt 33 - 11)		=> (- (expt 22 33) 11))

(check (infix 11 * 22 expt 33)		=> (* 11 (expt 22 33)))
(check (infix 11 / 22 expt 33)		=> (/ 11 (expt 22 33)))
(check (infix 22 expt 33 * 11)		=> (* (expt 22 33) 11))
(check (infix 22 expt 33 / 11)		=> (/ (expt 22 33) 11))

(check (infix 11 quotient 22 expt 33)	=> (quotient 11 (expt 22 33)))
(check (infix 11 modulo 22 expt 33)	=> (modulo 11 (expt 22 33)))
(check (infix 22 expt 33 quotient 11)	=> (quotient (expt 22 33) 11))
(check (infix 22 expt 33 modulo 11)	=> (modulo (expt 22 33) 11))

;;; --------------------------------------------------------------------

(check (infix 11 + 22 & 33)	=> (+ 11 (bitwise-and 22 33)))
(check (infix 11 - 22 & 33)	=> (- 11 (bitwise-and 22 33)))
(check (infix 22 & 33 + 11)	=> (+ (bitwise-and 22 33) 11))
(check (infix 22 & 33 - 11)	=> (- (bitwise-and 22 33) 11))

(check (infix 11 + 22 ⏐ 33)	=> (+ 11 (bitwise-ior 22 33)))
(check (infix 11 - 22 ⏐ 33)	=> (- 11 (bitwise-ior 22 33)))
(check (infix 22 ⏐ 33 + 11)	=> (+ (bitwise-ior 22 33) 11))
(check (infix 22 ⏐ 33 - 11)	=> (- (bitwise-ior 22 33) 11))

(check (infix 11 + 22 ^ 33)	=> (+ 11 (bitwise-xor 22 33)))
(check (infix 11 - 22 ^ 33)	=> (- 11 (bitwise-xor 22 33)))
(check (infix 22 ^ 33 + 11)	=> (+ (bitwise-xor 22 33) 11))
(check (infix 22 ^ 33 - 11)	=> (- (bitwise-xor 22 33) 11))

(check (infix 1 & 2 << 3)	=> (bitwise-and 1 (bitwise-arithmetic-shift-left 2 3)))
(check (infix 1 ⏐ 2 << 3)	=> (bitwise-ior 1 (bitwise-arithmetic-shift-left 2 3)))
(check (infix 2 << 3 & 1)	=> (bitwise-and (bitwise-arithmetic-shift-left 2 3) 1))
(check (infix 2 << 3 ⏐ 1)	=> (bitwise-ior (bitwise-arithmetic-shift-left 2 3) 1))

;;; --------------------------------------------------------------------

;;first the arithmetic operator, then the logic one
(check (infix 11 + 22 and 33)	=> 33)
(check (infix 22 and 11 + 22)	=> (+ 11 22))

;;first the arithmetic operator, then the comparison one
(check (infix 11 + 44 > 33)	=> #t)
(check (infix 22 < 11 + 44)	=> #t)


;;;; miscellaneous

(check
    (infix list("ciao" , 'hello , '#u8(1 2 3 4)))
  => '("ciao" hello #u8(1 2 3 4)))


;;;; done

(check-report)

#| end of module |# )

;;; end of file
