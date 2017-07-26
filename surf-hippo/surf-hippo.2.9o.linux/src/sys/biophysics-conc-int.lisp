M;; -*- mode: lisp; Syntax: Common-lisp; package: surf ; base: 10;  -*-
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


;;; SYS Source file: biophysics-conc-int.lisp
(in-package "SURF-HIPPO")


;; Equations specifically related to biophysical relationships that need to be defined before conc-int.lisp

(defun nernst-potential (inside-conc outside-conc valence)
  "INSIDE-CONC and OUTSIDE-CONC are in mM, returns potential (outside - inside) in mV."
  (declare (optimize (safety 0) (speed 3) (space 1))
	   (single-float inside-conc outside-conc))
  (the sf (* (the sf (/ NERNST-EQN-CONST*1000*TEMP valence))
	     (the sf (log (/ outside-conc inside-conc))))))

(defun nernst-potential-all-floats (inside-conc outside-conc valence)
  (declare (optimize (safety 0) (speed 3) (space 1))
	   (single-float inside-conc outside-conc valence))
  (* (/ NERNST-EQN-CONST*1000*TEMP valence)
     (the sf (log (/ outside-conc inside-conc)))))

(proclaim '(inline nernst-potential-conc-double-floats))
(defun nernst-potential-conc-double-floats (inside-conc outside-conc valence)
  (declare (optimize (safety 0) (speed 3) (space 1))
	   (double-float inside-conc outside-conc)
	   (single-float valence))
  (* (/ NERNST-EQN-CONST*1000*TEMP valence)
     (the df (log (/ outside-conc inside-conc)))))




(defun ghk-potential (inside-activities outside-activities permeabilities &optional (temperature *temp-celcius*))
  "Returns potential (outside - inside) in mV.  For monovalent species, the list of activities in
INSIDE-ACTIVITIES and OUTSIDE-ACTIVITIES [mM] should be ordered according to specific species.  The
inside and outside activity of monovalent anions [e.g. Cl-] should be included in the
OUTSIDE-ACTIVITIES and INSIDE-ACTIVITIES, respectively. The relative PERMEABILITIES of the ions
should be in the same order as that for the other args. TEMPERATURE is in degrees celcius."
  (* (/ (* 1000.0 (temperature-centigrade-kelvin Temperature)) FoverR)
     (log (/
	   (apply '+ (mapcar '* outside-activities permeabilities))
	   (apply '+ (mapcar '* inside-activities permeabilities))))))



