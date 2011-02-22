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

(define-module (streams common)
  #:export (must must-not must-every))

(define (must-not pred obj func msg . args)
  (if (pred obj)
      (scm-error 'wrong-type-arg func msg args (list obj))))

(define (must pred obj func msg . args)
  (apply must-not (negate pred) obj func msg args))

(define (must-every pred objs func msg . args)
  (let loop ((objs objs))
    (if (not (null? objs))
        (begin
          (apply must pred (car objs) func msg args)
          (loop (cdr objs))))))
