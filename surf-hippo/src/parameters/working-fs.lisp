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


;; A delayed rectifier and sodium channel model designed to reproduce basic fast-spiking cell properties.



(channel-type-def
  '(KDR-FS
        (iv-relation . :OHMIC)
        (gbar-source . :ABSOLUTE)
        (gbar-ref . 40.0)
        (e-rev . -80.0)
        (use-defined-rev . T)
        (ion-permeabilities . ((k 1.0)))
        (q10 . 1.0)
        (reference-temp . 35.0)
        (v-particles . ((K-X-FS 1)))
                   ))


#|
(particle-type-def
 `(K-X-FS
   (class . :MARKOV)
   (STATES . (C1 C2 O))
   (OPEN-STATES . (O))
   (STATE-TRANSITIONS . 
    ((C2 C1 (SQUASHED-EXPONENTIAL VOLTAGE :K -4.0 :V-HALF -30.0 :BASE-RATE 0.0 :MAX-RATE 20.0) NIL)
     (C1 C2 (SQUASHED-EXPONENTIAL VOLTAGE :K 10.0 :V-HALF 15.0 :BASE-RATE 0.0 :MAX-RATE 100.0) NIL)
     (C2 O (SQUASHED-EXPONENTIAL VOLTAGE :K -15.0 :V-HALF -20.0 :BASE-RATE 0.2 :MAX-RATE 4.0) NIL)
     (O C1 (SQUASHED-EXPONENTIAL VOLTAGE :K -10.0 :V-HALF -55.0 :BASE-RATE 1.0 :MAX-RATE 70.0) NIL)))
   (reference-temp . 27.0)
   (qten . 1.0)))
|#

(particle-type-def
 `(K-X-FS
   (class . :MARKOV)
   (STATES . (I C O))
   (OPEN-STATES . (O))
   (STATE-TRANSITIONS . 
    ((C I (SQUEEZED-EXPONENTIAL VOLTAGE :K -4.0 :V-HALF -30.0 :tau-min 0.05) NIL)
     (I C (SQUEEZED-EXPONENTIAL VOLTAGE :K 10.0 :V-HALF 15.0 :tau-min 0.01) NIL)
     (C O (SQUEEZED-EXPONENTIAL VOLTAGE :K -15.0 :V-HALF -20.0 :tau-max 5.25 :tau-min 0.25) NIL)
     (O I (SQUEEZED-EXPONENTIAL VOLTAGE :K -10.0 :V-HALF -55.0 :tau-max ,(+ 1.0 (/ 1 70.0)) :tau-min ,(/ 1 70.0)) NIL)))
   (reference-temp . 27.0)
   (qten . 1.0)))
   


(channel-type-def
  '(NA-FS
        (iv-relation . :OHMIC)
        (gbar-source . :ABSOLUTE)
        (gbar-ref . 0.67200005)
        (e-rev . 65.0)
        (use-defined-rev . T)
        (ion-permeabilities . ((NA 1.0)))
        (q10 . 1.0)
        (reference-temp . 27.0)
        (v-particles . ((NA-X-FS 1)))
                   ))




#|
(particle-type-def
 `(NA-X-FS
   (class . :MARKOV)
   (STATES . (C O I))
   (OPEN-STATES . (O))
   (STATE-TRANSITIONS . 
    ((O C (SQUASHED-EXPONENTIAL VOLTAGE :V-HALF -57.0 :K -2.0 :BASE-RATE 0.0 :MAX-RATE 3.0) NIL)
     (C O (SQUASHED-EXPONENTIAL VOLTAGE :K 1.0 :V-HALF -47.0 :BASE-RATE 0.0 :MAX-RATE 3.0) NIL)
     (I C (SQUASHED-EXPONENTIAL VOLTAGE :K -1.0 :V-HALF -60.0 :BASE-RATE 0.01 :MAX-RATE 40.0) NIL)
     (O I (CONSTANT-RATE VOLTAGE :RATE 3.0) NIL)
     ))
   (reference-temp . 27.0)
   (qten . 1.0)
   ))
|#

(particle-type-def
 `(NA-X-FS
   (class . :MARKOV)
   (STATES . (C O I))
   (OPEN-STATES . (O))
   (STATE-TRANSITIONS . 
    ((O C (SQUEEZED-EXPONENTIAL VOLTAGE :V-HALF -57.0 :K -2.0 :tau-min ,(/ 1 3.0)) NIL)
     (C O (SQUEEZED-EXPONENTIAL VOLTAGE :K 1.0 :V-HALF -47.0 :tau-min ,(/ 1 3.0)) NIL)
     (I C (SQUEEZED-EXPONENTIAL VOLTAGE :K -1.0 :V-HALF -60.0 :tau-max ,(+ 100 (/ 1 40)) :tau-min ,(/ 1 40)) NIL)
     (O I (CONSTANT-RATE VOLTAGE :RATE 3.0) NIL)
     ))
   (reference-temp . 27.0)
   (qten . 1.0)
   ))



