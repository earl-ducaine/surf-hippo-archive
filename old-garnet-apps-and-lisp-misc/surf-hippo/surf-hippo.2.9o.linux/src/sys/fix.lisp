;;; -*- Mode: LISP; Syntax: Common-lisp; Package: surf ; Base: 10 -*-
;;; Lyle Borg-Graham, Equipe Cognisciences, Institut Alfred Fessard, CNRS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;              The Surf-Hippo Neuron Simulator                    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Surf-Hippo project at the  ;;;
;;; Center for Biological Information Processing, Department of     ;;;
;;; Brain and Cognitive Sciences, Massachusetts Institute of        ;;;
;;; Technology, and currently at the Equipe Cognisciences, Institut ;;;
;;; Alfred Fessard, CNRS and has been placed in the public domain.  ;;;
;;; If you are using this code or any part of Surf-Hippo, please    ;;;
;;; contact surf-hippo@ai.mit.edu to be put on the mailing list.    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; SYS Source file: fix.lisp



(in-package "SURF-HIPPO")

#|
(defun adjust-element-pointer (elt nd)
  (typecase elt
    (isource (funcall (model-create-core-routine (gethash (string "isource") *model-hash-table*)) elt nd))))
|#



#|
(defun add-off-diag (elt nd1 nd2 off-diag)
  "Calls the add-off-diag routine of the appropriate type"
  (typecase elt
    (segment (funcall (model-add-off-diag-routine (gethash (string "segment") *model-hash-table*)) elt nd1 nd2 off-diag))))
|#
