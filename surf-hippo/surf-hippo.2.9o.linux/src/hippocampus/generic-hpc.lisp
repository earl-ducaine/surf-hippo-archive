;;; -*- mode: lisp; Syntax: Common-lisp; package: surf ; base: 10;  -*-
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


(in-package "SURF-HIPPO")

#|
This file includes the Working model, as described in

Borg-Graham, L. {\it Interpretations of Data and Mechanisms for
Hippocampal Pyramidal Cell Models}, Chapter in {\it Cerebral Cortex}
Vol.\ 13 - Cortical Models, ed.\ E.\ Jones, P.\ Ulinski and A.\ Peters, Plenum
Publishing Corporation 1998

|#

(defun working-hpc ()
  (setq *fix-e-k* nil *fix-e-na* nil)
  (dead-hippo "HPC"
	      (create-cell-type "HPC"
				:v-leak -65
				:membrane-resistivity 40000
				:cytoplasmic-resistivity 200
				:soma-resistivity 2500
				:specific-capacitance 0.7
				:soma-specific-capacitance 0.7
				:K-CONC-EXTRA-DEPENDENCE :FOLLOWS-GLOBAL
				:na-CONC-EXTRA-DEPENDENCE :FOLLOWS-GLOBAL
				:e-na-dependence :follows-concentration
				:e-k-dependence :follows-concentration))
  (create-element *soma* '(
			   NA-HPC
			   KDR-HPC
			   KM-HPC
			   kd-HPC
			  
			   KCT-HPC
			   kahp-HPC
			   KA-HPC

			   h-HPC

			   ca-l-HPC
			   ca-n-HPC
			   ca-t-HPC))
  (loop for ch in (channels) when (element-of-ion-type-p ch 'ca)
	do (element-parameter ch 'conc-int-delta 0.34)))

(push 'working-hpc *CIRCUIT-FUNCTIONS*)