;;; ext.scm -- SRFI 41 extensions

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

(define-module (srfi srfi-41 ext)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-8)
  #:use-module (srfi srfi-41 common)
  #:use-module (srfi srfi-41 primitive)
  #:use-module ((srfi srfi-41 derived)
                #:select (stream-do stream-fold-aux))
  #:re-export (stream-do)
  #:export (stream-split-at))

;; Similar to (values (stream->list n strm) (stream-drop n strm)) but
;; only iterates through the stream once.
(define (stream-split-at n strm)
  (must stream? strm 'stream-split-at "non-stream argument")
  (must integer? n 'stream-split-at "non-integer argument")
  (must-not negative? n 'stream-split-at "negative argument")
  (receive (head tail _) (stream-fold-aux xcons '() strm n)
    (values (reverse! head) tail)))
