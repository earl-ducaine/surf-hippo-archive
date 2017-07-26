;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10 -*-
;;; Lyle Borg-Graham, Equipe Cogniscience, Institut Alfred Fessard, CNRS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;              The Surf-Hippo Neuron Simulator                    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Surf-Hippo project at the  ;;;
;;; Center for Biological Information Processing, Department of     ;;;
;;; Brain and Cognitive Sciences, Massachusetts Institute of        ;;;
;;; Technology, and currently at the Equipe Cogniscience, Institut  ;;;
;;; Alfred Fessard, CNRS and has been placed in the public domain.  ;;;
;;; If you are using this code or any part of Surf-Hippo, please    ;;;
;;; contact surf-hippo@ai.mit.edu to be put on the mailing list.    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;;; Loader file for the Surf-Hippo GARNET fixes files.

;;; This loader file was adapted from kr-loader.lisp, part of the   
;;; Garnet project at CMU.                   

#|
==================================================================
Change log:
==================================================================
|#

(in-package "USER")


;; check first to see if place is set
(unless (boundp 'Surf-Hippo-Garnet-Fixes-PathName)
  (error "Load 'Surf-Hippo-Loader' first to set Surf-Hippo-Garnet-Fixes-PathName before loading Garnet-Fixes."))

(Defvar Surf-Hippo-Garnet-Fixes-Files	; We use a defvar rather than defparameter so that a setq of
					; this variable before loading this file will override the
					; definition here, if we want to compile only some of these files.
  (list
   #+GARNET-V3.0   "i-windows-fix"
					;   #-GARNET-V3.0
   "clx-hacks"
   #-GARNET-V3.0 "clx-colorized-seg"
   "new-defs-hack"
   
   #-GARNET-V3.0 "virtual-aggregates-hack"

   #+GARNET-V3.0 "gem"
   #+GARNET-V3.0 "virtual-aggregates-hack-3-0"
   #+GARNET-V3.0 "virtual-line-update"
   #+GARNET-V3.0 "virtual-circle-update"
   
   ;; For Garnet ps files
   #-GARNET-V3.0 "ps-virtual-methods"
   "make-ps-file"

   #-GARNET-V3.0 "misc-garnet-fixes"

   #-GARNET-V3.0 "raise-lower-wind-fix" ; 8/21/98 LBG Make sure that this is not needed in v3.0

   #+GARNET-V3.0 "virtual-aggregate-w-aggregadgets-fix"

   #+GARNET-V3.0 "DESTROY-ME-METHOD-VIRTUAL-AGGREGATE"

   "register-fns"			; 11/4/98

   "make-image"				; chmod of image file to 755
  
   "garnet-pathnames"			; If DIR doesn't exist, default to *SURF-HOME*.
   "utils"				; A small change to garnet-restart-function
   ))

(defvar garnet-fixes-files-to-force-compile-all nil)



(setq garnet-fixes-files-to-force-compile-all '(; "clx-hacks"
						"new-defs-hack"
						#-GARNET-V3.0 "virtual-aggregates-hack"))
  
(compile-source-directory Surf-Hippo-garnet-Fixes-Src
			  Surf-Hippo-garnet-Fixes-pathname Surf-Hippo-garnet-Fixes-Files
			  :files-to-force-compile-all garnet-fixes-files-to-force-compile-all
			  
			  :enable-compile compile-garnet-fixes-p)

(setf (get :surf-hippo-modules :garnet-fixes)  t)


(ext::gc) ; for some reason loading a compiled system chokes after the clmath sys - this prevents that??
