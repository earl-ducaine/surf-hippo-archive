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
; Schematic parameters for light synapse types.
;

(in-package "SURF-HIPPO")

(synapse-type-def
 '(l-ex-1
   (gbar-density . 10.0)
   (e-rev . 0.0)
   (control . light)
   (impulse-function . (double-alpha 10 60))))

(synapse-type-def
 '(l-ex-1-nmda
   (parent-type . l-ex-1)
   (static-voltage-dependence . (sigmoid-array -50.0 0.5 -100 50 0.1))))


(synapse-type-def
 '(l-in-1
   (gbar-density . 1000.0)
   (e-rev . -70.0)
   (control . light)
   (impulse-function . (alpha-array 100 :ADJUSTMENT :UNIT-AREA ))))



(synapse-type-def
 '(l-ON-ex-1
   (gbar-density . 10.0)
   (e-rev . 0.0)
   (control . light)
   (impulse-function . (double-alpha 10 60))
   (ADJUST-TO-RF-AREA . t)
   (SPATIAL-RF-FUNCTION . (GAUSSIAN-RF 20 20))))

(synapse-type-def
 '(l-OFF-ex-1
   (gbar-density . 10.0)
   (e-rev . 0.0)
   (control . light)
   (impulse-function . (double-alpha 60 10))
   (ADJUST-TO-RF-AREA . t)
   (SPATIAL-RF-FUNCTION . (GAUSSIAN-RF 20 20))))

(synapse-type-def
 '(l-OFF-ex-2
   (gbar-density . 10.0)
   (e-rev . 0.0)
   (control . light)
   (impulse-function . (double-alpha 12 2))
   (ADJUST-TO-RF-AREA . t)
   (SPATIAL-RF-FUNCTION . (GAUSSIAN-RF 50 50 :grid-size 50))))

(synapse-type-def
 '(l-ONcs-ex-2
   (gbar-density . 10.0)
   (e-rev . 0.0)
   (control . light)
   (impulse-function . (double-alpha 2 12))
   (ADJUST-TO-RF-AREA . t)
   (SPATIAL-RF-FUNCTION . (dog-RF 20 20 50 50 1))))

(synapse-type-def
 '(l-OFFcs-ex-2
   (gbar-density . 10.0)
   (e-rev . 0.0)
   (control . light)
   (impulse-function . (double-alpha 12 2))
   (ADJUST-TO-RF-AREA . t)
   (SPATIAL-RF-FUNCTION . (dog-RF 20 20 50 50 1))))

(synapse-type-def
 '(l-ONcs-ex-3
   (gbar-density . 10.0)
   (e-rev . 0.0)
   (control . light)
   (impulse-function . (double-alpha 2 12))
   (ADJUST-TO-RF-AREA . t)
   (SPATIAL-RF-FUNCTION . (dog-RF 20 20 50 50 1 :grid-size 30))))

(synapse-type-def
 '(l-OFFcs-ex-3
   (gbar-density . 10.0)
   (e-rev . 0.0)
   (control . light)
   (impulse-function . (double-alpha 12 2))
   (ADJUST-TO-RF-AREA . t)
   (SPATIAL-RF-FUNCTION . (dog-RF 20 20 50 50 1 :grid-size 30))))






(synapse-type-def
 '(l-ex-fac
   (gbar-density .	 50.0)
   (e-rev . 0.0)
   (control . light)
   (impulse-function . (double-alpha 10 40 .9))))

