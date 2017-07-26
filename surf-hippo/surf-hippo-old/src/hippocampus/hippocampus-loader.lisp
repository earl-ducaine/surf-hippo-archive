;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Surf-Hippo Neuron Simulator                         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Surf-Hippo project at the  ;;;
;;; Center for Biological Information Processing, Department of     ;;;
;;; Brain and Cognitive Sciences, Massachusetts Institute of        ;;;
;;; Technology, and has been placed in the public                   ;;;
;;; domain.  If you are using this code or any part of Surf-Hippo,  ;;;
;;; please contact lyle@ai.mit.edu to be put on the mailing list.   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 


;; Loader file for the Surf-Hippo Hippocampus files.



;;; This loader file was adapted from kr-loader.lisp:
;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
==================================================================
Change log:
==================================================================
|#


(in-package "USER")


(defparameter Hippocampus-Version-Number "1.0")

(format t "Loading Surf-Hippo Hippocampus...~%")

;; check first to see if place is set
(unless (boundp 'Surf-Hippo-Hippocampus-PathName)
  (error "Load 'Surf-Hippo-Loader' first to set Surf-Hippo-Hippocampus-PathName before loading Hippocampus."))


;; ---- Load Hippocampus files

(Defvar Surf-Hippo-Hippocampus-Files   ;; defvar rather than defparameter so can setq
                               ;; this variable before loading if only want
			       ;; to compile some of these files
  '(
    "hippos"
   "read-me"
    ))


(dolist (file Surf-Hippo-Hippocampus-Files)
  (load (merge-pathnames file 
			 #+cmu "hippocampus:"
			 #+(not cmu) Surf-Hippo-Hippocampus-PathName
			 )
	:verbose T))



(setf (get :surf-hippo-modules :hippocampus)  t)

(format t "...Done Hippocampus.~%")

