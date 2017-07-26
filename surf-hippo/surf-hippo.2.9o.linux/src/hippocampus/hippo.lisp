;;; -*- Mode: Lisp; Syntax: Common-lisp; Package: #-PARALLEL SURF #+PARALLEL *SURF; Base: 10; -*-
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

;; See the surf-hippo/doc/basics.doc file for more information.


(cell-type-def
 '(CA1
   (membrane-resistivity . 40000)
   (cytoplasmic-resistivity  . 200)
   (soma-resistivity . 40000)
   (specific-capacitance . 0.7)))

;; Hippocampal pyramidal cartoon cell geometry.
(defun hippo (cell-name &key synapse-types synapse-segs (cell-origin '(0.0 0.0 0.0))
			nucleus-diameter
			cell-type
			(active *active*)
			channels

			(dendrite-diameter 12.0) (dendrite-length 1200.0)

			include-basal (basal-dendrite-diameter 12.0) (basal-dendrite-length 200.0)			

			(soma-diameter 35.0) ; microns, as specified in TR 1161

			(r-cyto 200.0)
			(r-mem 40000.0) (r-mem-soma 40000.0)
			(cap-mem 0.7) (cap-mem-soma 0.7)

			(total-segs 5)	; The Thesis had 5 segments, 240 uM each
			(basal-total-segs 5)
			soma-electrode)
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (let ((soma
	 (create-soma
	  :cell (create-cell cell-name
			     :cell-origin cell-origin
			     :cell-type
			     (or cell-type
				 (create-cell-type 'CA1
						   :membrane-resistivity r-mem  :cytoplasmic-resistivity r-cyto
						   :soma-resistivity r-mem-soma :specific-capacitance cap-mem
						   :soma-specific-capacitance cap-mem-soma)))		       
	  :diameter soma-diameter)))
    (when soma-electrode (add-isource soma))
    (when nucleus-diameter (element-parameter soma 'nucleus-diameter (s-flt nucleus-diameter)))
    (when active (create-element soma channels))
    (segment-chain soma (when include-basal "a") total-segs (/ dendrite-length total-segs) dendrite-diameter
		   :proximal-theta (* 0.5 pi))
    (when include-basal (segment-chain soma "b" basal-total-segs (/ basal-dendrite-length basal-total-segs)
				       basal-dendrite-diameter :proximal-theta (* -0.5 pi)))
    (create-element synapse-types synapse-segs)
    *cell*))

;; These parameters are taken from MIT-AI TR 1161, with some modifications, but in general
;; this can be a backbone for a standard soma/short-cable geometry cell model.

;; Note that the HIPPO thesis value for R-M-soma
;; is 1/3 too small. 2550 ohms-cm2 is the
;; correct value for matching R-in of 39Mohms.


(cell-type-def
 '(tr-1161-CA1
   (membrane-resistivity . 40000)
   (cytoplasmic-resistivity  . 200)
   (soma-resistivity . 2550)
   (v-leak . -70)
   (specific-capacitance . 1)))


(defun TR-1161-hippo ()
  (hippo "TR-1161"
   :soma-diameter 35.0
   :cell-type (create-celltype 'tr-1161-CA1)
   :total-segs 5
   :basal-total-segs 5
   :dendrite-diameter 12.0
   :dendrite-length 1200.0)
  (create-element *soma* 'na1-TR1161 'na2-TR1161 'na3-TR1161 'ca-TR1161 'a-TR1161 'dr-TR1161
		  'c-TR1161 'ahp-TR1161 'm-TR1161) 
  *cell*)
