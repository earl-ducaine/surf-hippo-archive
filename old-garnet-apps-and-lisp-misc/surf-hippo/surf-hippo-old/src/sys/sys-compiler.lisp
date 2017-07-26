;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10 -*-
;;;
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


;; Loader file for the Surf-Hippo System files.



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

(in-package "USER" :use '("LISP"))

(Defparameter Surf-Hippo-Sys-Files
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
    ))

(dolist (file Surf-Hippo-Sys-Files)
	 (compile-file (merge-pathnames file Surf-Hippo-Sys-Src))
	 #+(or allegro explorer lispworks lucid cmu)
	 (load         (merge-pathnames file Surf-Hippo-Sys-Src)))



#+(or allegro explorer lispworks lucid cmu)
(setf (get :surf-hippo-modules :sys) T)



