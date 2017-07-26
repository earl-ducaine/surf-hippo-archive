;;; -*- mode: lisp; Syntax: Common-lisp; package: surf ; base: 10; -*-
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

;
; Parameters for synapse types.
;

(in-package "SURF-HIPPO")


#|

Parameters described in:

@ARTICLE{Ber-Dog-Mar-Koc-91,
	AUTHOR = {Bernander, O. and Douglas, R. J. and Martin, K. A. C. and Koch, C.},
	TITLE = {Synaptic background activity influences spatiotemporal integration in single pyramidal cells},
        Journal = {Proc. Natl. Acad. Sci. USA},
	YEAR = {1991},
	VOLUME = {88},
	PAGES = {11569-11573}
}

|#

(synapse-type-def
 '(ampa-auto-bernander-et-al-91
   (gbar . 0.0005)			; uS
   (e-rev . 0.0)			; mV
   (waveform-function . (alpha-array 1.5 :step 0.2)) ; The default for the amplitude of ALPHA-ARRAY is :NORMALIZE.
   (waveform-time-interval . 0.2))	; ms - consistent with :STEP arg for ALPHA-ARRAY.
 )

(synapse-type-def
 '(gaba-a-auto-bernander-et-al-91
   (gbar . 0.001)			; uS
   (e-rev . -70.0)			; mV
   (waveform-function . (alpha-array 10.0 :step 0.2)) ; The default for the amplitude of ALPHA-ARRAY is :NORMALIZE.
   (waveform-time-interval . 0.2))	; ms - consistent with :STEP arg for ALPHA-ARRAY.
 )

(synapse-type-def
 '(gaba-b-auto-bernander-et-al-91
   (gbar . 0.0001)			; uS
   (e-rev . -95.0)			; mV
   (waveform-function . (alpha-array 40.0 :step 0.2)) ; The default for the amplitude of ALPHA-ARRAY is :NORMALIZE.
   (waveform-time-interval . 0.2))	; ms - consistent with :STEP arg for ALPHA-ARRAY.
 )
