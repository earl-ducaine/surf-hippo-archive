;;; -*- Mode: Lisp; Syntax: Common-lisp; Package: #-PARALLEL SURF #+PARALLEL *SURF; Base: 10; -*-
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


;;; SYS Source file: hippo.lisp
(in-package "SURF-HIPPO")

;; HIPPO Hippocampal pyramidal cartoon cell geometry, channels. The default parameters are taken
;; from MIT-AI TR 1161, with some modifications, but in general this can be a backbone for a
;; standard soma/short-cable geometry cell model.

(defun hippo (cell-name &key synapse-types synapse-segs  (cell-origin '(0.0 0.0 0.0))
			nucleus-diameter
			cell-type
			(active *active*)
			(channels '(na1 na2 na3 ca a dr c ahp m))

			(dendrite-diameter 12.0)
			(dendrite-length 1200.0)

			include-basal
			(basal-dendrite-diameter 12.0)
			(basal-dendrite-length 200.0)

			(soma-diameter  35.0) ; microns, as specified in TR 1161
			(r-cyto 200.0)
			(r-mem 40000.0)
			(r-mem-soma 2550.0)
			(cap-mem 1.0)
			(cap-mem-soma 1.0)
			
			(total-segs 5)	; The Thesis had 5 segments, 240 uM each
			(basal-total-segs 5)
			soma-electrode
			)
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (let ((soma
	 (create-soma
	  :cell (create-cell cell-name
			     :cell-origin cell-origin
			     :cell-type
			     (or cell-type
				 (create-cell-type "CA1"
						   :membrane-resistivity r-mem  :cytoplasmic-resistivity r-cyto
						   ;; Note that the HIPPO thesis value for R-M-soma
						   ;; is 1/3 too small. 2550 ohms-cm2 is the
						   ;; correct value for matching R-in of 39Mohms.
						   :soma-resistivity r-mem-soma :specific-capacitance cap-mem
						   :soma-specific-capacitance cap-mem-soma)))		       
	  :diameter soma-diameter)))
    (when soma-electrode (add-isource soma))
    (when nucleus-diameter
      (element-parameter soma 'nucleus-diameter (s-flt nucleus-diameter)))
    (typecase total-segs
      (float (setq total-segs (round total-segs))))
    (typecase basal-total-segs
      (float (setq basal-total-segs (round basal-total-segs))))
    
    (when active (create-channels soma channels))
    
    (make-soma-segment-chain-fast soma (if include-basal "a") total-segs
				  (the sf (/ (s-flt dendrite-length) (the fn total-segs)))
				  dendrite-diameter synapse-types synapse-segs
				  :proximal-theta (* 0.5 pi-single))			     
    (when include-basal (make-soma-segment-chain soma "b" basal-total-segs
						 (the sf (/ (s-flt basal-dendrite-length) (the fn basal-total-segs)))
						 basal-dendrite-diameter
						 synapse-types synapse-segs
						 :proximal-theta (* -0.5 pi-single)))
    (soma-cell soma)))

