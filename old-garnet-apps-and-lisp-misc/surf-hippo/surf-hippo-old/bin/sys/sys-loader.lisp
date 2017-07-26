;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;

#|
==================================================================
Change log:
==================================================================
|#


(in-package "USER")


(defparameter Sys-Version-Number "1.0")

(format t "Loading Surf-Hippo Sys...~%")

;; check first to see if place is set
(unless (boundp 'Surf-Hippo-Sys-PathName)
  (error "Load 'Surf-Hippo-Loader' first to set Surf-Hippo-Sys-PathName before loading Sys."))


;; ---- Load gadgets files

(Defvar Surf-Hippo-Sys-Files   ;; defvar rather than defparameter so can setq
                               ;; this variable before loading if only want
			       ;; to compile some of these files
  '(
    "macros"
   "declare"
   "subs"
   ;; just for parallel -> "devices"
   "init"
   "sim"
   "node"
   ;; step.lisp needs to know about structure type 'node, so put it here
   "step"
   "tri-ge"				;don't need this for Hines method
   "soma"
   "segment"
   "res"
   "cap"
   "vsource"
   "isource"
   "currents"
;;   "new-currents"
   "conc-int"
   "channel"
   "conc-part"
   "particle"
   "synapse"
   "misc"
   "cell"
   "fix"
   "read"
   "print"
   "main"
   "plot"
   "output"
    "garnet-tools"
    "cell-graphics"
    ))


(dolist (file Surf-Hippo-Sys-Files)
  (load (merge-pathnames file 
			 #+cmu "sys:"
			 #+(not cmu) Surf-Hippo-Sys-PathName
			 )
	:verbose T))



(setf (get :surf-hippo-modules :sys)  t)

(format t "...Done Sys.~%")

