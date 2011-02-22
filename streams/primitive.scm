;;; primitive.scm -- SRFI 41 stream primitives

;; Copyright (c) 2007 Philip L. Bewig
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

(define-module (streams primitive)
  #:use-module (srfi srfi-9)
  #:use-module ((srfi srfi-45)
                #:select (lazy eager delay force promise?)
                #:renamer (symbol-prefix-proc 'stream-))
  #:use-module (streams common)
  #:export (stream? stream-null stream-null?
            stream-pair? stream-car stream-cdr)
  #:export-syntax (stream-cons stream-lambda))

(define (stream? obj)
  (or (stream-pair? obj) (stream-null? obj)))

(define stream-null (stream-delay '(stream . null)))

(define (stream-null? obj)
  (and (stream-promise? obj)
       (eqv? (stream-force obj) (stream-force stream-null))))

(define-record-type stream-pare (make-stream-pare kar kdr) stream-pare?
  (kar stream-kar)
  (kdr stream-kdr))

(define (stream-pair? obj)
  (and (stream-promise? obj) (stream-pare? (stream-force obj))))

(define-syntax stream-cons
  (syntax-rules ()
    ((stream-cons obj strm)
     (stream-eager (make-stream-pare (stream-delay obj) (stream-lazy strm))))))

(define (stream-car strm)
  (must stream? strm 'stream-car "non-stream")
  (must-not stream-null? strm 'stream-car "null stream")
  (stream-force (stream-kar (stream-force strm))))

(define (stream-cdr strm)
  (must stream? strm 'stream-cdr "non-stream")
  (must-not stream-null? strm 'stream-cdr "null stream")
  (stream-kdr (stream-force strm)))

(define-syntax stream-lambda
  (syntax-rules ()
    ((stream-lambda formals body0 body1 ...)
     (lambda formals (stream-lazy (begin body0 body1 ...))))))
