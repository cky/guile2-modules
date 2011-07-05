;;; derived.scm -- SRFI 41 derived stream functions

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

(define-module (srfi srfi-41 derived)
  #:use-module (guile compat)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-41 primitive)
  #:use-module (srfi srfi-41 common)
  #:export-syntax (define-stream stream stream-do stream-let)
  #:export (list->stream port->stream stream->list stream-append
            stream-concat stream-constant stream-drop stream-drop-while
            stream-filter stream-fold stream-for-each stream-from
            stream-iterate stream-length stream-map stream-match
            stream-of stream-range stream-ref stream-reverse stream-scan
            stream-take stream-take-while stream-unfold stream-unfolds
            stream-zip))

(define-syntax-rule (define-stream (name . formal) body0 body1 ...)
  (define name (stream-lambda formal body0 body1 ...)))

(define-syntax-rule (stream-let tag ((name val) ...) body1 body2 ...)
  ((letrec ((tag (stream-lambda (name ...) body1 body2 ...))) tag) val ...))

(define (list->stream objs)
  (must list? objs 'list->stream "non-list argument")
  (stream-let recur ((objs objs))
    (if (null? objs) stream-null
        (stream-cons (car objs) (recur (cdr objs))))))

(define* (port->stream #:optional (port (current-input-port)))
  (must input-port? port 'port->stream "non-input-port argument")
  (stream-let recur ()
    (let ((c (read-char port)))
      (if (eof-object? c) stream-null
          (stream-cons c (recur))))))

(define-syntax stream
  (syntax-rules ()
    ((_) stream-null)
    ((_ x y ...) (stream-cons x (stream y ...)))))

(define stream->list
  (case-lambda
   ((strm) (stream->list #f strm))
   ((n strm)
    (must stream? strm 'stream->list "non-stream argument")
    (when n
      (must integer? n 'stream->list "non-integer count")
      (must-not negative? n 'stream->list "negative count"))
    (do ((n (or n -1) (1- n))
         (res '() (cons (stream-car strm) res))
         (strm strm (stream-cdr strm)))
        ((or (zero? n) (stream-null? strm)) (reverse! res))))))

(define (stream-append . strms)
  (must-every stream? strms 'stread-append "non-stream argument")
  (stream-let recur ((strms strms))
    (if (null? strms) stream-null
        (let ((strm (car strms)))
          (if (stream-null? strm) (recur (cdr strms))
              (stream-cons (stream-car strm)
                           (recur (cons (stream-cdr strm) (cdr strms)))))))))

(define (stream-concat strms)
  (must stream? strms 'stream-concat "non-stream argument")
  (stream-let recur ((strms strms))
    (if (stream-null? strms) stream-null
        (let ((strm (stream-car strms)))
          (must stream? strm 'stream-concat "non-stream object in input stream")
          (if (stream-null? strm) (recur (stream-cdr strms))
              (stream-cons (stream-car strm)
                           (recur (stream-cons (stream-cdr strm)
                                               (stream-cdr strms)))))))))

(define stream-constant
  (case-lambda
   (() stream-null)
   (objs (stream-let recur ((objs (apply circular-list objs)))
           (stream-cons (car objs) (recur (cdr objs)))))))

(define-syntax-rule (stream-do ((var init . step) ...)
                               (test expr ...)
                               command ...)
  (stream-let loop ((var init) ...)
    (if test
        (stream-do-end expr ...)
        (begin
          command ...
          (loop (stream-do-step var . step) ...)))))

(define-syntax stream-do-end
  (syntax-rules ()
    ((_) (if #f #f))
    ((_ expr ...) (begin expr ...))))

(define-syntax stream-do-step
  (syntax-rules ()
    ((_ var) var)
    ((_ var step) step)))

(define (stream-drop n strm)
  (must integer? n 'stream-drop "non-integer argument")
  (must-not negative? n 'stream-drop "negative argument")
  (must stream? strm 'stream-drop "non-stream argument")
  (stream-do ((n n (1- n))
              (strm strm (stream-cdr strm)))
             ((or (zero? n) (stream-null? strm)) strm)))

(define (stream-drop-while pred? strm)
  (must procedure? pred? 'stream-drop-while "non-procedural argument")
  (must stream? strm 'stream-drop-while "non-stream argument")
  (stream-do ((strm strm (stream-cdr strm)))
             ((or (stream-null? strm) (not (pred? (stream-car strm)))) strm)))

(define (stream-filter pred? strm)
  (must procedure? pred? 'stream-filter "non-procedural argument")
  (must stream? strm 'stream-filter "non-stream argument")
  (stream-let recur ((strm strm))
    (cond ((stream-null? strm) stream-null)
          ((pred? (stream-car strm))
           (stream-cons (stream-car strm) (recur (stream-cdr strm))))
          (else (recur (stream-cdr strm))))))

(define (stream-fold proc base strm)
  (must procedure? proc 'stream-fold "non-procedural argument")
  (must stream? strm 'stream-fold "non-stream argument")
  (do ((base base (proc base (stream-car strm)))
       (strm strm (stream-cdr strm)))
      ((stream-null? strm) base)))

(define (stream-for-each proc strm . rest)
  (let ((strms (cons strm rest)))
    (must procedure? proc 'stream-for-each "non-procedural argument")
    (must-every stream? strms 'stream-for-each "non-stream argument")
    (do ((strms strms (map stream-cdr strms)))
        ((any stream-null? strms))
      (apply proc (map stream-car strms)))))

(define* (stream-from first #:optional (step 1))
  (must number? first 'stream-from "non-numeric starting number")
  (must number? step 'stream-from "non-numeric step size")
  (stream-let recur ((first first))
    (stream-cons first (recur (+ first step)))))

(define (stream-iterate proc base)
  (must procedure? proc 'stream-iterate "non-procedural argument")
  (stream-let recur ((base base))
    (stream-cons base (recur (proc base)))))

(define (stream-length strm)
  (must stream? strm 'stream-length "non-stream argument")
  (do ((len 0 (1+ len))
       (strm strm (stream-cdr strm)))
      ((stream-null? strm) len)))

(define (stream-map proc strm . rest)
  (let ((strms (cons strm rest)))
    (must procedure? proc 'stream-map "non-procedural argument")
    (must-every stream? strms 'stream-map "non-stream argument")
    (stream-let recur ((strms strms))
      (if (any stream-null? strms) stream-null
          (stream-cons (apply proc (map stream-car strms))
                       (recur (map stream-cdr strms)))))))

;; stream-match, stream-of not implemented yet

(define* (stream-range first past #:optional (step (if (< first past) 1 -1)))
  (must number? first 'stream-range "non-numeric starting number")
  (must number? past 'stream-range "non-numeric ending number")
  (must number? step 'stream-range "non-numeric step size")
  (let ((lt? (if (< 0 step) < >)))
    (stream-let recur ((first first))
      (if (lt? first past)
          (stream-cons first (recur (+ first step)))
          stream-null))))

(define (stream-ref strm n)
  (must stream? strm 'stream-ref "non-stream argument")
  (must integer? n 'stream-ref "non-integer argument")
  (must-not negative? n 'stream-ref "negative argument")
  (let loop ((strm strm) (n n))
    (must-not stream-null? strm 'stream-ref "beyond end of stream")
    (if (zero? n) (stream-car strm)
        (loop (stream-cdr strm) (1- n)))))

(define (stream-reverse strm)
  (must stream? strm 'stream-reverse "non-stream argument")
  (stream-do ((strm strm (stream-cdr strm))
              (rev stream-null (stream-cons (stream-car strm) rev)))
             ((stream-null? strm) rev)))

(define (stream-scan proc base strm)
  (must procedure? proc 'stream-scan "non-procedural argument")
  (must stream? strm 'stream-scan "non-stream argument")
  (stream-let recur ((base base) (strm strm))
    (if (stream-null? strm) (stream base)
        (stream-cons base (recur (proc base (stream-car strm))
                                 (stream-cdr strm))))))

(define (stream-take n strm)
  (must stream? strm 'stream-take "non-stream argument")
  (must integer? n 'stream-take "non-integer argument")
  (must-not negative? n 'stream-take "negative argument")
  (stream-let recur ((n n) (strm strm))
    (if (or (stream-null? strm) (zero? n)) stream-null
        (stream-cons (stream-car strm) (recur (1- n) (stream-cdr strm))))))

(define (stream-take-while pred? strm)
  (must procedure? pred? 'stream-take-while "non-procedural argument")
  (must stream? strm 'stream-take-while "non-stream argument")
  (stream-let recur ((strm strm))
    (cond ((stream-null? strm) stream-null)
          ((pred? (stream-car strm))
           (stream-cons (stream-car strm) (recur (stream-cdr strm))))
          (else stream-null))))

(define (stream-unfold mapper pred? generator base)
  (must procedure? mapper 'stream-unfold "non-procedural mapper")
  (must procedure? pred? 'stream-unfold "non-procedural pred?")
  (must procedure? generator 'stream-unfold "non-procedural generator")
  (stream-let recur ((base base))
    (if (pred? base)
        (stream-cons (mapper base) (recur (generator base)))
        stream-null)))

;; stream-unfolds not implemented yet

(define (stream-zip strm . rest)
  (let ((strms (cons strm rest)))
    (must-every stream? strms 'stream-zip "non-stream argument")
    (stream-let recur ((strms strms))
      (if (any stream-null? strms) stream-null
          (stream-cons (map stream-car strms) (recur (map stream-cdr strms)))))))
