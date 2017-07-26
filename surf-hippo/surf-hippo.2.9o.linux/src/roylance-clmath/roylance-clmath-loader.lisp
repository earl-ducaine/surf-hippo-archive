;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10 -*-
;;; Lyle Borg-Graham, MIT Center for Biological Information Processing
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;              The Surf-Hippo Neuron Simulator                    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Surf-Hippo project at the  ;;;
;;; Center for Biological Information Processing, Department of     ;;;
;;; Brain and Cognitive Sciences, Massachusetts Institute of        ;;;
;;; Technology, and has been placed in the public domain. If you    ;;;
;;; are using this code or any part of Surf-Hippo, please contact   ;;;
;;; surf-hippo@ai.mit.edu to be put on the mailing list.            ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Loader file for the Surf-Hippo Roylance-Clmath files.

;;; This loader file was adapted from kr-loader.lisp, part of the   
;;; Garnet project at CMU.



#|
==================================================================
Change log:
==================================================================
|#


(in-package "USER")


;(defparameter Roylance-Clmath-Version-Number "1.0")


;; check first to see if place is set
(unless (boundp 'Surf-Hippo-Roylance-Clmath-PathName)
  (error "Load 'Surf-Hippo-Loader' first to set Surf-Hippo-Roylance-Clmath-PathName before loading Roylance-Clmath."))


;; ---- Load Roylance-Clmath files

(Defvar Surf-Hippo-Roylance-Clmath-Files	; We use a defvar rather than defparameter so that a setq of
					; this variable before loading this file will override the
					; definition here, if we want to compile only some of these files.
  '(

    "attrib"
    "gamma"
    "bessel"
    "horner"
    "beta"
    "import"
    "binomial"
    "integr"
    "bisection"
    "marq"
    "combin"
    "matrix"
    "matrix-double"
    "consts"
    "mod"
    "dft"
;;    "modules"
    "ellip"
    "poisson"
    "erf"
    "regres"
    "factor"
    "falsep"
    "fib"
    "runge"
    "fit"
    "statis"
    "fmfp"


    
    ))

(compile-source-directory Surf-Hippo-roylance-clmath-Src
			  Surf-Hippo-roylance-clmath-pathname Surf-Hippo-roylance-clmath-Files
			  :enable-compile compile-roylance-clmath-p)



(setf (get :surf-hippo-modules :roylance-clmath)  t)


