;;; queue.scm---port of Racket's data/queue to Guile

;; Copyright (c) 2008 Carl Eastlund
;; Copyright (c) 2010 PLT Scheme Inc.
;; Copyright (c) 2011 Chris K. Jester-Young

;; This library is free software; you can redistribute it and/or modify
;; it under the terms of the GNU Lesser General Public License as published
;; by the Free Software Foundation; either version 2.1 of the License, or
;; (at your option) any later version.
;;
;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public License
;; along with this library.  If not, see <http://www.gnu.org/licenses/>.

(define-module (srfi srfi-41 queue)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-41 common)
  #:export (queue? make-queue queue-empty? queue-length queue->list
            enqueue! dequeue!))

; The defaulting of unspecified constructor fields to #f is a Guilism.
(define-record-type queue (make-queue) queue?
  (head queue-head set-queue-head!)
  (tail queue-tail set-queue-tail!))

(define-record-type link (make-link value) link?
  (value link-value)
  (tail link-tail set-link-tail!))

(define (queue-empty? q) (not (queue-head q)))

(define (nonempty-queue? v) (and (queue? v) (queue-head v) #t))

(define (enqueue! q v)
  (must queue? q 'enqueue! "queue")
  (let ((new (make-link v)))
    (if (queue-head q)
        (set-link-tail! (queue-tail q) new)
        (set-queue-head! q new))
    (set-queue-tail! q new)))

(define (dequeue! q)
  (must queue? q 'dequeue! "queue")
  (let ((old (queue-head q)))
    (must-not not old 'dequeue! "empty queue")
    (set-queue-head! q (link-tail old))
    (link-value old)))

(define (queue->list queue)
  (unfold not link-value link-tail (queue-head queue)))

(define (queue-length queue)
  (let loop ((link (queue-head queue))
             (count 0))
    (if (not link) count
        (loop (link-tail link) (1+ count)))))
