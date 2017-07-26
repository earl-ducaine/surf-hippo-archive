;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10 -*-
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


;;; SYS Source file: sys-loader.lisp
;;
;;; Loader file for the Surf-Hippo system files.

;;; This loader file was adapted from kr-loader.lisp, part of the   
;;; Garnet project at CMU.                   

#|
==================================================================
Change log:
==================================================================
|#

(in-package "USER")

;; check first to see if place is set
(unless (boundp 'Surf-Hippo-Sys-PathName)
  (error "Load 'Surf-Hippo-Loader' first to set Surf-Hippo-Sys-PathName before loading Sys."))

(Defvar Surf-Hippo-Sys-Files		; We use a defvar rather than defparameter so that a setq of
					; this variable before loading this file will override the
					; definition here, if we want to compile only some of these files.
  (list
   "macros"
   "declare"				; Most of the global variables.

   "biophysics-declare"			; Variable values associated with reality, not the code.

   "structures"				; All of the structure definitions and some slot macros
   "structure-macros"
   ;   "structure-data-macros"

   "models"
   "init"
   "structure-data-functions"

   "math"				; Some misc math functions
   "some-statistics"
   "filters"
   "fft"
   "randoms"
   "renewal-process"
   "waveforms"				; Must be before synapse.lisp

   "misc"
   "debug"


   "biophysics-conc-int"
   "pump-preliminaries"			; Need inlined PUMP-CONCENTRATION-CURRENT for conc-ints.
   "conc-int"
   "biophysics"
   
   "matrix-system-solver"
   "sim"  "circuit-input"  "hines"
   
   "node" "soma" "segment"

   "source" "isource" "vsource"

   "electrode" "extracellular-electrode"



   "general-membrane-elements"


   
   "channel"				; some inline functions here are also used in synapse.lisp
   "particle"				; some inline functions here are used in conc-part.lisp
   "markov-particle"
   "conc-part"				

   ;; Synapse code
   "synapse-rf"				; NG code
   "reduced-synapse"
   "synapse"
   "light-synapse-functions"
   "synapse-events"
   "synapse-evaluation"

   
   "buffer" "pump" "axon"

   "event-generators"			; applies to (at least) synapses and axons

   "cell"
   "cable_functions"
   "element_functions"
   "trees"

   "print"
   "analysis"
   "store-plot-data"
   "histology-xfrms"
   "histology-hack"
   "sparse-data"
   "colorizing"
   "cell-graphics-setup"
   "cell-graphics-virtual-schema"
   "cell-graphics-grapes"
   "cell-graphics"
   "user-cell-graphics"
   #-GARNET-V3.0 "virtual-aggregate-update-method"

   ;; "builder"

   "info-hack"
   "plot"
   "3dplot"
   "trace-functions"

   "menus"

   "data-folder"

   "calc-lte-ratio"			; 1/2/94 Put here since refers to earlier macros.

   "step"

   "hacks"			; Also includes some functions that refer to earlier macros.
   
   "raster-plot"
   "protocols"
   "sample-cells"
   "ntscable"
   "neurolucida"

   ))


(compile-source-directory Surf-Hippo-sys-Src
			  Surf-Hippo-sys-pathname Surf-Hippo-Sys-Files
			  :files-to-force-compile-all '("structures" "structure-macros" "macros" "math")
			  :enable-compile compile-sys-p)

(setf (get :surf-hippo-modules :sys)  t)



