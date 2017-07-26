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
; Schematic parameters for synapse types.
;

(in-package "SURF-HIPPO")

(synapse-type-def
 '(auto-fast-ex-double-exp
   (gbar-density . 50.0)
   (e-rev . 0.0)
   (control . auto)
   (waveform-function . (double-exponential 0.10 10.0 :step 0.2 :length 400 :normalize t))
   (waveform-time-interval . 0.2)))


(synapse-type-def
 '(auto-fast-ex-double-exp-abs
   (gbar-source . :ABSOLUTE)
   (gbar-ref . 0.001)
   (e-rev . 0.0)
   (control . auto)
   (waveform-function . (double-exponential 0.10 10.0 :step 0.2 :length 400 :normalize t))
   (waveform-time-interval . 0.2)))

(synapse-type-def
 '(fast-ex
   (gbar-density . 50.0)
   (e-rev . 0.0)
   (refractory-period . 2.0)
   (input-THRESHOLD . 10.0)
   (waveform-function . (alpha-array 0.500 :step 0.1))
   (waveform-time-interval . 0.1)
   (control . voltage)))


(synapse-type-def
 '(auto-fast-ex
   (gbar-density . 50.0)
   (e-rev . 0.0)
   (control . auto)
   (waveform-function . (alpha-array 2 :step 0.2))
   (waveform-time-interval . 0.2)))



(synapse-type-def
 '(nmda
   (parent-type . fast-ex)
   (static-voltage-dependence . (sigmoid-array -50.0 0.5 -150 50 0.1))))

(synapse-type-def
 '(nmda-auto
   (parent-type . auto-fast-ex)
   (static-voltage-dependence . (sigmoid-array -50.0 0.5 -150 50 0.1))))


;; Inhibitory


(synapse-type-def
 '(auto-inhibitory
   (gbar-density . 1000.0)
   (e-rev . -70.0)
   (control . auto)
   (waveform-function . (alpha-array 10 :step 0.2))
   (waveform-time-interval . 0.2)))


(synapse-type-def
 '(auto-inh
   (gbar . 0.002)
   (e-rev . -90.0)
   (control . auto)
   (waveform-function . (alpha-array 4 :step 0.2))
   (waveform-time-interval . 0.2)))


(synapse-type-def
 '(inh-slow
   (gbar-density . 1000.0)
   (e-rev . -90.0)
   (refractory-period . 2.0)
   (input-THRESHOLD . -30.0)
   (waveform-function . (alpha-array 4 :step 0.2))
   (waveform-time-interval . 0.2)
   (control . voltage)))


(synapse-type-def
 '(auto-gaba-a
   (gbar . 0.02)
   (e-rev . -70.0)
   (control . auto)
   (waveform-function . (double-exponential .5 20 :step 0.2))
   (waveform-time-interval . 0.2)))

(synapse-type-def
 '(auto-gaba-a-abs
   (gbar . 0.01)
   (e-rev . -70.0)
   (control . auto)
   (waveform-function . (double-exponential .5 20 :step 0.2))
   (waveform-time-interval . 0.2)))


