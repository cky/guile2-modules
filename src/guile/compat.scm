;;; compat.scm -- Guile compatibility functionality

;; Copyright (c) 2011 Chris K. Jester-Young.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; The (guile compat) module is for stuff that is expected to be in
;; newer versions of Guile, but not yet available as of 2.0.0. This
;; way, this module provides enough compatibility for the modules to
;; work in all 2.x versions.
(define-module (guile compat)
  #:use-module ((guile)
                #:select (define-syntax)
                #:renamer (symbol-prefix-proc 'guile-))
  #:replace-syntax (define-syntax)
  #:export-syntax (define-syntax-rule when unless))

;; http://lists.gnu.org/archive/html/guile-devel/2011-07/msg00021.html
(guile-define-syntax define-syntax
  (syntax-rules ()
    ((_ (name . args) body ...)
     (guile-define-syntax name (lambda args body ...)))
    ((_ name syntax)
     (guile-define-syntax name syntax))))

(define-syntax define-syntax-rule
  (syntax-rules ()
    ((_ (name . pattern) template)
     (define-syntax name
       (syntax-rules ()
         ((_ . pattern) template))))))

;; http://lists.gnu.org/archive/html/guile-devel/2011-06/msg00044.html
(define-syntax-rule (when test body ...)
  (if test (begin body ... (if #f #f))))

(define-syntax-rule (unless test body ...)
  (when (not test) body ...))
