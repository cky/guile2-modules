;;; common.scm -- common functionality for streams modules

;; Copyright (c) 2011 Chris K. Jester-Young

;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES, OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF, OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

(define-module (srfi srfi-41 common)
  #:use-module (guile compat)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-8)
  #:export (must must-not must-every pair-map first-value second-value
            third-value))

(define (must-not1 pred obj func msg args)
  (when (pred obj)
    (throw 'wrong-type-arg func msg args (list obj))))

(define (must-not* pred objs func msg args)
  (define flunk (filter pred objs))
  (unless (null? flunk)
    (throw 'wrong-type-arg func msg args flunk)))

(define* (must-not pred obj func msg . args)
  (must-not1 pred obj func msg args))

(define* (must pred obj func msg . args)
  (must-not1 (negate pred) obj func msg args))

(define* (must-every pred objs func msg . args)
  (must-not* (negate pred) objs func msg args))

; Only the one-list version is supported since that's what we use.
(define (pair-map proc clist)
  (define (kons pair result)
    (cons (proc pair) result))
  (pair-fold-right kons '() clist))

(define-syntax-rule (first-value expr)
  (receive (first . _) expr
    first))

(define-syntax-rule (second-value expr)
  (receive (first second . _) expr
    second))

(define-syntax-rule (third-value expr)
  (receive (first second third . _) expr
    third))
