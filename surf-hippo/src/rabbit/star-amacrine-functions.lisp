;;; -*- Mode: Lisp; Syntax: Common-lisp; Package: #-PARALLEL SURF #+PARALLEL *surf; Base: 10; -*-
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

;;; Functions using Cholinergic rabbit retina starburst AMACRINE cell
;;; described in /rabbit/star-amacrine.lisp.

;;; This is a moving bar, from left to right.


(defun rightward-bar ()
  (setq
   *enable-light* t
   *light-speed* 2.0
   *bar-length* 500.0 *bar-width* 50.0 *light-stimulus* :on-moving-bar
   *light-theta* (* 90 2.0 (COERCE user::PI 'SINGLE-FLOAT) (/ 1.0 360))
   *light-direction* nil		; T (nil) => movement is in the direction of / opposite to *light-theta-degrees
   *light-stimulus-start-time* 0.0	;Time to start bar moving, milliseconds
   *light-stimulus-stop-time* 100000.0
   *light-start-position-x* -300.0	; Stimulus position at *motion-start-time in microns
   *light-start-position-y* 0.0))



(defun star-amacrine-ds ()
  (rightward-bar)
  (setq ;; *synapse-names-to-do-first* '("222111B")
	*enable-synapses* t
	*user-stop-time* 300.0
	;; this gets a far left, far right, and top node.
	*plot-node-elements* '("DS-tip-soma" "222111B" "32211" "22332B" )

	*plot-soma-voltage-p t
	*plot-node-voltages-p t
	*plot-channel-currents-p nil
	*plot-channel-reversal-potentials-p nil
	*plot-channel-conductances-p nil
	*plot-shell-1-concentrations-p nil
	*plot-shell-2-concentrations-p nil
	*plot-particles-p nil
	*plot-vsource-currents-p t
	*plot-isource-currents-p nil
	*plot-conc-particles-p nil
	*plot-synapse-currents-p t
	*plot-synapse-conductances-p t)

  (star-amacrine "DS-tip" :cell-origin '(0.0 0.0 0.0)
		 :extras-list '(l-ex-fac L-IN-1)
		 :soma-channels '(na1-TR1161 na2-TR1161 na3-TR1161 dr-TR1161 a-TR1161)))

(push 'star-amacrine-ds *CIRCUIT-FUNCTIONS*)


;; gets distal nodes and orders them according to their angle
#|
(setq *plot-path-nodes* (loop for seg in (sort (loop for seg in (list-of-all-things 'segment) 
	    when (not (distal-segments seg)) 
	    collect
	    (list seg (atan (nth 1 (element-absolute-location seg))
			    (nth 0 (element-absolute-location seg)))))
     '>    :key #'cadr)
      collect (segment-name (car seg))))






(enable-element-analysis (distal-tips))
|#

(defun plot-tip-outputs ()
  (plot-scatter
   (loop for seg in (distal-tips)
	 collect
	 (let ((angle (atan (where-y seg) (where-x seg)))
	       (mag (integrate-x-y (element-data seg) *time* :y-base *integral-base)))
	   (list (* mag (cos angle))
		 (* mag (sin angle)))))))
(defun plot-tip-outputs ()
  (plot-polar-data
   (list
    (loop for seg in (distal-tips)
	  collect
	  (list 
	   (integrate-x-y (element-data seg) *time* :y-base *integral-base)
	   (atan (where-y seg) (where-x seg)))))
   '(foo)))



