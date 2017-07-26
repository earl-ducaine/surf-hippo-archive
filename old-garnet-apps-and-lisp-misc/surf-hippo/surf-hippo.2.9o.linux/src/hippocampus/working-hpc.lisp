;;; -*- mode: lisp; Syntax: Common-lisp; package: surf ; base: 10;  -*-
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


(in-package "SURF-HIPPO")

#|
This file includes the Working model, as described in

Borg-Graham, L., "Interpretations of Data and Mechanisms for Hippocampal
Pyramidal Cell Models". Chapter in "Cerebral Cortex, Volume 12, Cortical
Models", edited by E.G. Jones and P.S. Ulinski, Plenum Press, 1997.
|#

(defun add-working-hpc-channels (&optional (element *soma*) absolute-model)
  (let ((chs (create-element (typecase (element-cell-element element)
			       (segment element)
			       (soma (SOMA-SEGMENT-ATTACHED-TO-VIRTUAL-SOMA element)))
			     `(NA-HPC
			       KDR-HPC
			       KM-HPC
			       kd-HPC
			  
			       ,(if absolute-model `KCT-HPC-ABSOLUTE `KCT-HPC)
			       ,(if absolute-model `KAHP-HPC-ABSOLUTE `KAHP-HPC)
			       KA-HPC

			       h-HPC

			       ,(if absolute-model `ca-l-HPC-absolute `ca-l-HPC)
			       ,(if absolute-model `ca-n-HPC-absolute `ca-n-HPC)
			       ,(if absolute-model `ca-t-HPC-absolute `ca-t-HPC)))))
    (loop for ch in chs when (element-of-ion-type-p ch 'ca)
	  do (element-parameter ch 'conc-int-delta 0.34))))

(cell-type-def
 '(HPC
   (membrane-resistivity . 40000)
   (cytoplasmic-resistivity  . 200)
   (soma-resistivity . 2500)
   (specific-capacitance . 0.7)
   (v-leak . -65)
   (e-na-dependence . :follows-concentration)
   (e-k-dependence . :follows-concentration)))

(defun working-hpc (&key (name "HPC") (cell-origin '(0.0 0.0 0.0)))
  (setq *fix-e-k* nil *fix-e-na* nil)
  (hippo name :cell-type (create-celltype 'HPC) :cell-origin cell-origin)
  (add-working-hpc-channels)
  *cell*)

(push 'working-hpc *CIRCUIT-FUNCTIONS*)


(defun working-hpc-test ()
  (working-hpc)
  (std-setup)
  (pulse-list *isource* '(10 500 0.7))
  (setq *user-stop-time* 1000))