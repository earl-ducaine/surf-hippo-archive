; -*- mode: lisp; Syntax: Common-lisp; package: surf ; base: 10;  -*-
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



;; From Warman, E. N.; Durand, D. M.; and Yuen, G. L. F., Reconstruction of Hippocampal CA1
;; Pyramidal Cell Electrophysiology by Computer Simulation, J. Neurophy. v71, no 6, June 1994

;; see also parameters/warman94-chs.lisp

;; The top level cell function is WDY.


(defun wdy (&optional use-fitted-channels)
  (let* ((r-i 100.0)			; ohms cm
	 (r-m 16000.0)			; ohms cm2
	 (seg-diameter 5.2)		; microns
	 (segment-L 0.1)
	 (soma-area 942.5)		; um2
	 (r-m-soma 850.0)		; ohms cm2
	 (c-m-dendrite 1.85)		; uF/cm2
	 (c-m-soma 1.0)			; uF/cm2
	 (seg-length
	  ;; (LENGTH-FROM-LAMBDA 100.0 16000.0 2.6 0.1) => 144.22203 microns
	  (length-from-lambda r-i r-m (* 0.5 seg-diameter) segment-L))
	 (cell (create-cell "wdy"
			    :cell-type (create-cell-type "warman-durand-yuen"
							 :membrane-resistivity r-m
							 :cytoplasmic-resistivity r-i
							 :soma-resistivity r-m-soma
							 :soma-specific-capacitance c-m-soma
							 :dendrite-specific-capacitance c-m-dendrite
							 :dendrite-v-leak -65.0 ; ???
							 :soma-v-leak -65.0 ; ???
							 )))
	 ;; (SPHERE-DIAMETER-FROM-AREA 942.5) => 17.320711 microns
	 (soma (create-soma :cell cell :diameter (sphere-diameter-from-area soma-area))))
    ;; The distal apical section.
    (segment-chain
     ;; The short central apical segment.
     (segment-chain
      ;; The proximal apical section.
      (segment-chain soma "apical-root" 4 seg-length seg-diameter :proximal-phi 0.0 :proximal-theta (/ pi 2.0))
      "apical-center" 1 (* 0.5 seg-length) seg-diameter :proximal-phi 0.0 :proximal-theta 0.0)
     "apical-distal" 4 seg-length seg-diameter :proximal-phi 0.0 :proximal-theta  0.0)
    ;; The basal section.
    (segment-chain soma "basal" 6 seg-length seg-diameter :proximal-phi 0.0 :proximal-theta  (/ pi -2.0))
    
    (create-element soma
		    (if use-fitted-channels
			'(na-wdy-fit ca-wdy-fit kct-wdy-fit kahp-wdy km-wdy-fit ka-wdy-fit kdr-wdy-fit)
			'(na-wdy ca-wdy kct-wdy kahp-wdy km-wdy ka-wdy kdr-wdy)))
    cell))

(defun wdy-fitted ()
  (wdy t))

(defun wdy-test ()
  (wdy)
  (std-setup)
  (enable-element-plot (channels))
  nil)
  

(push 'wdy *CIRCUIT-FUNCTIONS*)
(push 'wdy-fitted *CIRCUIT-FUNCTIONS*)