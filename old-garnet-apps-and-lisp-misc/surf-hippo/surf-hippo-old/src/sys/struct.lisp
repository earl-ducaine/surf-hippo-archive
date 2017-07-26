;;; -*- mode: lisp; Syntax: Common-lisp; package: #-parallel surf #+parallel *surf; base: 10; -*-
;;; (c) Copyright 1986, Donald Webber, Thinking Machines Corporation, Inc.
;;; alterations (c) Copyright 1990, Lyle Borg-Graham, MIT Center for Biological Information Processing
;;; File creation date: 6/17/86 16:20:21

;; This is stuff for the parallel version.



(in-package #+parallel '*surf #-parallel 'surf)

#|
(proclaim '((field-pvar 3000) *all-memory*))
(*defvar *all-memory*)

(eval-when (load)
  (format t "~%setting up structures")
;  (*cold-boot)
  (setup-def-*lisp-structs)
)
|#

#+parallel
(defvar *processor-allocation-counter* 0
  "A counter used to allocate processors with.")

#+parallel
(defun allocate-processor ()
  "Gets a new processor"
  (let ((proc *processor-allocation-counter*))
    (if (= *processor-allocation-counter* (- *number-of-processors-limit* 2))
						; processor N-1 holds a segment bit
	(sim-error "Out of processors"))
    (incf *processor-allocation-counter*)
    proc))

#+parallel
(defun make-core-resistor ()
  (let ((proc (allocate-processor)))
    (*setf (pref *lisp-struct-type proc) core-resistor)
    proc))

#+parallel
(defun make-core-capacitor ()
  (let ((proc (allocate-processor)))
    (*setf (pref *lisp-struct-type proc) core-capacitor)
    proc))

#+parallel
(defun make-core-mos1 ()
  (let ((proc (allocate-processor)))
    (*setf (pref *lisp-struct-type proc) core-mos1)
    proc))

#+parallel
(defun make-core-isource ()
  (let ((proc (allocate-processor)))
    (*setf (pref *lisp-struct-type proc) core-isource)
    proc))

#+parallel
(defun make-core-node ()
  (let ((proc (allocate-processor)))
    (*setf (pref *lisp-struct-type proc) core-node)
    proc))

#+parallel
(defun make-core-off-diag ()
  (let ((proc (allocate-processor)))
    (*setf (pref *lisp-struct-type proc) core-off-diag)
    proc))

;#+parallel
;(defun make-core-fan-node ()
;  (let ((proc (allocate-processor)))
;    (*setf (pref *lisp-struct-type proc) core-fanout)
;    proc))

