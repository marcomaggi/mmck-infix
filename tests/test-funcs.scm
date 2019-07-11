;;; -*- coding: utf-8-unix  -*-
;;;
;;;Part of: MMCK Infix
;;;Contents: test program for additional functions
;;;Date: Jul 11, 2019
;;;
;;;Abstract
;;;
;;;	This program tests additional functions.
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

(require-library (mmck infix))

(module (test-funcs)
    ()
  (import (scheme)
	  (mmck infix)
	  (mmck checks)
	  (chicken pretty-print))

(check-set-mode! 'report-failed)
(check-display "*** testing additional functions\n")


(parameterise ((check-test-name		'factorial))

  (check (factorial 0)			=> 1)
  (check (factorial 1)			=> 1)
  (check (factorial 2)			=> 2)
  (check (factorial 3)			=> (* 3 2 1))
  (check (factorial 5)			=> (* 5 4 3 2 1))

  (values))


(parameterise ((check-test-name		'not-equal))

  (check-for-false	(!= 0))

  (check-for-false	(!=	0	0))
  (check-for-true	(!=	0	1))
  (check-for-true	(!=	1	0))

  (check-for-false	(!= 1 1 1 1 1 1))
  (check-for-true	(!= 2 1 1 1 1 1))
  (check-for-true	(!= 1 2 1 1 1 1))
  (check-for-true	(!= 1 1 2 1 1 1))
  (check-for-true	(!= 1 1 1 2 1 1))
  (check-for-true	(!= 1 1 1 1 2 1))
  (check-for-true	(!= 1 1 1 1 1 2))

  (values))


;;;; done

(check-report)

#| end of module |# )

;;; end of file
