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

(require-library (mmck infix))
(require-library (mmck checks))

(module (test-demo)
    ()
  (import (scheme)
	  (chicken base)
	  (mmck infix)
	  (mmck checks))

(check-set-mode! 'report-failed)
(check-display "*** testing demo\n")


;;;; stuff

(parameterize ((check-test-name		'alpha))

  (check
      (the-func)
    => #t)

  (values))


;;;; done

(check-report)

#| end of module |# )

;;; end of file
