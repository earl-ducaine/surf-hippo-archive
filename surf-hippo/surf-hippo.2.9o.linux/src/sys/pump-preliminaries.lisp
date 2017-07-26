;;;-*- mode: lisp; Syntax: Common-lisp; package: surf ; base: 10 ; -*-
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


;;; SYS Source file: pump-preliminaries.lisp
;
; These pump functions must be compiled before conc-int.lisp
;

(in-package "SURF-HIPPO")

(proclaim '(inline pump-conc-int-compartment-volume))
(defun pump-conc-int-compartment-volume (pump)
  "PUMP compartment volume in cm^3."
  (let ((cint (pump-conc-int pump)))
    (case (pump-conc-int-compartment pump)
      (1 (conc-int-shell-1-volume cint)) ; cm3
      (2 (conc-int-shell-2-volume cint))
      (3 (conc-int-shell-3-volume cint))
      (t (conc-int-total-volume cint)))))

(proclaim '(inline pump-concentration-current))
(defun pump-concentration-current (pump &optional compartment-volume)
  "Returns mM/ms. COMPARTMENT-VOLUME is in cm^3."
  (declare (optimize (safety 0) (speed 3) (space 0)))
  (let ((volume (the df (or compartment-volume (pump-conc-int-compartment-volume pump)))))
    (if (= volume 0)
	0.0d0
    
	(/ (pump-current pump)		; millimoles/ms
	   (* (the df volume )
	      1.0d-3))			; liters/cm3
	)))

