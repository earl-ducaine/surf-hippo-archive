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

(load-surf-user-file "~/surf-hippo/data/HPC/hpc-hippochapter-3-21-97-draft.elts")

(defun hpc ()
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
				:e-k-dependence :follows-concentration)))


(topload 'hpc)

(add-isource "HPC-soma" "HPC-soma-isrc")

(create-channels *soma* '(
			  NA-4STATE-exp-GEN 
			  KDR-sah
			  KM-GEN
			  kd-gen
			  
			  kc-markov
			  kahp-gen
			  KA-gen

			  h-mac-etal93

			  ca-l-gen
			  ca-n-gen
			  ca-t-gen))



(loop for ch in (channels) when (element-of-ion-type-p ch 'ca)
      do (set-element-parameter ch 'conc-int-delta 0.34)
      (set-conc-integrators-parameters))

(revamp-type-parameters)

(setq *plot-node-elements* (list (soma-name *soma*)))


