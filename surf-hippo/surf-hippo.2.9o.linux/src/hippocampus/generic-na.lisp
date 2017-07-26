;;; -*- mode: lisp; Syntax: Common-lisp; package: surf ; base: 10;  -*-
;;; Lyle Borg-Graham, MIT Center for Biological Information Processing

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


(in-package "SURF-HIPPO")
(topload 'dead-hippo)
(add-isource *soma*)
(create-channels *soma* '(na-trb91 na-wdy na-mig95 ns-hh-fit na-vb91 na-patlak91 na-sgg
			  na-fsbg-slice))
(create-channel *soma* 'na-kb94)

(setq *plot-node-elements* (list (soma-name *soma*)))

