;;; -*- coding: utf-8-unix  -*-
;;;
;;;Part of: MMCK Infix
;;;Contents: version functions
;;;Date: Jul  9, 2019
;;;
;;;Abstract
;;;
;;;	This unit defines version functions.
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

(declare (unit mmck.infix.version)
	 (emit-import-library mmck.infix.version))

(module (mmck.infix.version)
    (mmck-infix-package-major-version
     mmck-infix-package-minor-version
     mmck-infix-package-patch-level
     mmck-infix-package-prerelease-tag
     mmck-infix-package-build-metadata
     mmck-infix-package-version
     mmck-infix-package-semantic-version)
  (import (scheme)
	  (prefix mmck.infix.config config::))


;;;; version functions

(define (mmck-infix-package-major-version)	config::MMUX_PKG_MAJOR_VERSION)
(define (mmck-infix-package-minor-version)	config::MMUX_PKG_MINOR_VERSION)
(define (mmck-infix-package-patch-level)	config::MMUX_PKG_PATCH_LEVEL)
(define (mmck-infix-package-prerelease-tag)	config::MMUX_PKG_PRERELEASE_TAG)
(define (mmck-infix-package-build-metadata)	config::MMUX_PKG_BUILD_METADATA)
(define (mmck-infix-package-version)		config::MMUX_PKG_VERSION)
(define (mmck-infix-package-semantic-version)	config::MMUX_PKG_SEMANTIC_VERSION)


;;;; done

#| end of module |# )

;;; end of file
