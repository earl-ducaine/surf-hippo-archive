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

#|
This file includes mechanisms from the Working model, as described in

Borg-Graham, L., "Interpretations of Data and Mechanisms for Hippocampal
Pyramidal Cell Models". Chapter in "Cerebral Cortex, Volume 13, Cortical
Models", edited by P.S. Ulinski, E.G. Jones and A. Peters, Plenum Press, 1998.
|#

(pump-type-def
 `(CA-HPC-MM
   (class . :MM)
   (v-max . 6.0e-11)
   (kd . 0.01)
   (species . CA)
   (qten . 1.0)
   (reference-temp . 27.0)
   ))


(channel-type-def
 '(KM-HPC
   (iv-relation . :OHMIC)
   (gbar-source . :ABSOLUTE)
   (gbar-ref . 0.4)
   (e-rev . -80.0)
   (use-defined-rev . T)
   (ion-permeabilities . ((K 0.9) (NA 0.1)))
   (q10 . 1.0)
   (reference-temp . 27.0)
   (v-particles . ((KMU-HPC 2)))))


(particle-type-def
 `(KMU-HPC
   (class . :HH-EXT)
   (valence . 6.0)
   (gamma . 0.6)
   (base-rate . 0.003)
   (v-half . -45.0)
   (tau-0 . 8.0)
   (IGNORE-TAU-VOLTAGE-DEPENDENCE . NIL)
   (reference-temp . 35.0)
   (FIXED-BOLTZMANN-REFERENCE-TEMPERATURE . 35.0)
   (qten . 1.0)))




(channel-type-def
 '(KA-HPC
   (iv-relation . :OHMIC)
   (gbar-source . :ABSOLUTE)
   (gbar-ref . 2.3)
   (e-rev . -70.0)
   (use-defined-rev . T)
   (ion-permeabilities . ((K 0.85) (NA 0.15)))
   (q10 . 1.0)
   (reference-temp . 35.0)
   (v-particles . ((KAX-HPC 4)(KAY-HPC 3)))))

(particle-type-def
 `(KAX-HPC
   (class . :HH-EXT)
   (valence . 2.8)
   (gamma . 0.85)
   (base-rate . 0.08)
   (v-half . -41.0)
   (tau-0 . 1.0)
   (IGNORE-TAU-VOLTAGE-DEPENDENCE . NIL)
   (reference-temp . 35.0)
   (FIXED-BOLTZMANN-REFERENCE-TEMPERATURE . 27.0)
   (qten . 1.0)))


(particle-type-def
 `(KAY-HPC
   (class . :HH-EXT)
   (valence . -3.0)
   (gamma . 1.0)
   (base-rate . 0.04)
   (v-half . -49.0)
   (tau-0 . 2.0)
   (IGNORE-TAU-VOLTAGE-DEPENDENCE . NIL)
   (reference-temp . 35.0)
   (FIXED-BOLTZMANN-REFERENCE-TEMPERATURE . 27.0)
   (qten . 1.0)))




(channel-type-def
 '(KAHP-HPC
   (iv-relation . :OHMIC)
   (gbar-source . :ABSOLUTE)
   (gbar-ref . 0.02)
   (e-rev . -85.0)
   (use-defined-rev . T)
   (ion-permeabilities . ((K 1.0)))
   (q10 . 1.0)
   (reference-temp . 27.0)
   (conc-particles . ((KAHPO-HPC 2)))))



(conc-particle-type-def
 '(KAHPO-HPC
   (class . :NTH-ORDER)
   (alpha . 2.0e+14)
   (beta . 0.01)
   (tau-0 . 100.0d0)
   (power . 4)
   (qten . 1.0)
   (reference-temp . 30.0)
   (shell . 2)
   (conc-int-type . CA-IN-HPC)))
                   


(channel-type-def
 '(CA-L-HPC
   (iv-relation . :CONSTANT-FIELD)
   (permeability-source . :ABSOLUTE)
   (permeability . 3.0e-9)
   (e-rev . 0.0)
   (use-defined-rev . T)
   (ion-permeabilities . ((CA 1.0)))
   (q10 . 1.0)
   (reference-temp . 22.0)
   (conc-int-type-params . ((CA-IN-HPC (1 1.0d0))))
   (v-particles . ((CA-LM-HPC 2)))))



(particle-type-def
 `(CA-LM-HPC
   (class . :HH-EXT)
   (valence . 4.6)
   (gamma . 0.0)
   (base-rate . 1.0)
   (v-half . -1.2)
   (tau-0 . 1.5)
   (IGNORE-TAU-VOLTAGE-DEPENDENCE . T)
   (reference-temp . 27.0)
   (FIXED-BOLTZMANN-REFERENCE-TEMPERATURE . 27.0)
   (qten . 1.0)))




(channel-type-def
 '(CA-T-HPC
   (iv-relation . :CONSTANT-FIELD)
   (permeability-source . :ABSOLUTE)
   (permeability . 3.0e-9)
   (e-rev . 0.0)
   (use-defined-rev . T)
   (ion-permeabilities . ((CA 1.0)))
   (q10 . 1.0)
   (reference-temp . 22.0)
   (conc-int-type-params . ((CA-IN-HPC (1 1.0d0))))
   (v-particles . ((CA-TM-HPC 2)(CA-TH-HPC 1)))))






(particle-type-def
 `(CA-TM-HPC
   (class . :HH-EXT)
   (valence . 3.0)
   (gamma . 0.0)
   (base-rate . 1.0)
   (v-half . -36.0)
   (tau-0 . 1.5)
   (IGNORE-TAU-VOLTAGE-DEPENDENCE . T)
   (reference-temp . 27.0)
   (qten . 1.0)))


(particle-type-def
 `(CA-TH-HPC
   (class . :HH-EXT)
   (valence . -5.2)
   (gamma . 0.0)
   (base-rate . 1.0)
   (v-half . -68.0)
   (tau-0 . 10.0)
   (IGNORE-TAU-VOLTAGE-DEPENDENCE . T)
   (reference-temp . 27.0)
   (qten . 1.0)))




(channel-type-def
 '(KDR-HPC
   (iv-relation . :OHMIC)
   (gbar-source . :ABSOLUTE)
   (gbar-ref . 0.4)
   (e-rev . -70.0)
   (use-defined-rev . T)
   (ion-permeabilities . ((K 1.0)))
   (q10 . 1.0)
   (reference-temp . 24.0)
   (v-particles . ((KDRX-HPC 1)(KDRY-HPC 1)))))






(particle-type-def
 `(KDRX-HPC
   (class . :HH-EXT)
   (valence . 3.0)
   (gamma . 0.8)
   (base-rate . 0.17)
   (v-half . -5.0)
   (tau-0 . 0.8)
   (IGNORE-TAU-VOLTAGE-DEPENDENCE . NIL)
   (reference-temp . 27.0)
   (FIXED-BOLTZMANN-REFERENCE-TEMPERATURE . 27.0)
   (qten . 3.0)))


(particle-type-def
 `(KDRY-HPC
   (class . :HH-EXT)
   (valence . -1.0)
   (gamma . 0.0)
   (base-rate . 0.0)
   (v-half . -68.0)
   (tau-0 . 300.0)
   (IGNORE-TAU-VOLTAGE-DEPENDENCE . T)
   (reference-temp . 27.0)
   (FIXED-BOLTZMANN-REFERENCE-TEMPERATURE . 27.0)
   (qten . 3.0)))




(channel-type-def
 '(NA-HPC
   (iv-relation . :OHMIC)
   (gbar-source . :ABSOLUTE)
   (gbar-ref . 1.2)
   (e-rev . 65.0)
   (use-defined-rev . T)
   (ion-permeabilities . ((NA 1.0)))
   (q10 . 1.0)
   (reference-temp . 27.0)
   (v-particles . ((NA-X-HPC 1)))))



(particle-type-def
 `(NA-X-HPC
   (class . :MARKOV)
   (STATES . (C1 C2 O I))
   (OPEN-STATES . (O))
   (STATE-TRANSITIONS . 
    ((C2 O (SQUEEZED-EXPONENTIAL VOLTAGE :V-HALF -51.0 :K 1.0 :TAU-MIN 0.3333))
     (O C2 (SQUEEZED-EXPONENTIAL VOLTAGE :V-HALF -57.0 :K -2.0 :TAU-MIN 0.3333))
     (O I 3)
     (O C1 (SQUEEZED-EXPONENTIAL VOLTAGE :V-HALF -51.0 :K -2.0 :TAU-MIN 0.3333))
     (C1 O (SQUEEZED-EXPONENTIAL VOLTAGE :V-HALF -42.0 :K 1.0 :TAU-MIN 0.3333))
     (I C1 (SQUEEZED-EXPONENTIAL VOLTAGE :V-HALF -53.0 :K -1.0 :TAU-MAX 100.0 :TAU-MIN 1.0))
     (C1 C2 (SQUEEZED-EXPONENTIAL VOLTAGE :V-HALF -60.0 :K -1.0 :TAU-MAX 100.0 :TAU-MIN 1.0))))
   (reference-temp . 27.0)
   (qten . 1.0)))
   




(channel-type-def
 '(KD-HPC
   (iv-relation . :OHMIC)
   (gbar-source . :ABSOLUTE)
   (gbar-ref . 0.05)
   (e-rev . -95.0)
   (use-defined-rev . T)
   (ion-permeabilities . ((K 1.0)))
   (q10 . 1.0)
   (reference-temp . 35.0)
   (v-particles . ((KDX-HPC 4)(KDY-HPC 4)))))



(particle-type-def
 `(KDX-HPC
   (class . :HH-EXT)
   (valence . 3.0)
   (gamma . 0.0)
   (base-rate . 1.0)
   (v-half . -63.0)
   (tau-0 . 1.0)
   (IGNORE-TAU-VOLTAGE-DEPENDENCE . T)
   (reference-temp . 35.0)
   (FIXED-BOLTZMANN-REFERENCE-TEMPERATURE . 27.0)
   (qten . 1.0)))


(particle-type-def
 `(KDY-HPC
   (class . :HH-EXT)
   (valence . -2.5)
   (gamma . 0.0)
   (base-rate . 2.0e-4)
   (v-half . -73.0)
   (tau-0 . 0.0)
   (IGNORE-TAU-VOLTAGE-DEPENDENCE . NIL)
   (reference-temp . 35.0)
   (FIXED-BOLTZMANN-REFERENCE-TEMPERATURE . 27.0)
   (qten . 1.0)))




(channel-type-def
 '(CA-N-HPC
   (iv-relation . :CONSTANT-FIELD)
   (permeability-source . :ABSOLUTE)
   (permeability . 10.0e-9)
   (e-rev . 0.0)
   (use-defined-rev . T)
   (ion-permeabilities . ((CA 1.0)))
   (q10 . 1.0)
   (reference-temp . 22.0)
   (conc-int-type-params . ((CA-IN-HPC (1 1.0d0))))
   (v-particles . ((CA-NM-HPC 2)(CA-NH-HPC 1)))))




(particle-type-def
 `(CA-NM-HPC
   (class . :HH-EXT)
   (valence . 3.4)
   (gamma . 0.0)
   (base-rate . 1.0)
   (v-half . -21.0)
   (tau-0 . 1.5)
   (IGNORE-TAU-VOLTAGE-DEPENDENCE . T)
   (reference-temp . 27.0)
   (qten . 1.0)))


(particle-type-def
 `(CA-NH-HPC
   (class . :HH-EXT)
   (valence . -2.0)
   (gamma . 0.0)
   (base-rate . 1.0)
   (v-half . -40.0)
   (tau-0 . 75.0)
   (IGNORE-TAU-VOLTAGE-DEPENDENCE . T)
   (reference-temp . 27.0)
   (qten . 1.0)))




(channel-type-def
 '(KCT-HPC
   (iv-relation . :OHMIC)
   (gbar-source . :ABSOLUTE)
   (gbar-ref . 0.4)
   (e-rev . -80.0)
   (use-defined-rev . T)
   (ion-permeabilities . ((K 0.9) (NA 0.1)))
   (q10 . 1.0)
   (reference-temp . 27.0)
   (v-particles . ((KCTX-HPC 1)))))





(defvar *enable-KCTX-HPC-ca-forward-v-term* t)
(defvar *KCTX-HPC-ca-activation-forward-exponential-term*)
(setq *KCTX-HPC-ca-activation-forward-exponential-term*
      (v-function-array '(squeezed-exponential
			  voltage
			  :tau-min 0.001
			  :k 7.0
			  :v-half -20.0
			  :tau-max 1.0)))

(defun KCTX-HPC-ca-activation-forward (prt)
  (declare (optimize (safety 0) (speed 3) (space 0) (compilation-speed 0)))
  (return-markov-rate
   (* (if *ENABLE-KCTX-HPC-CA-FORWARD-V-TERM*
	  (aref (the vec-df *KCTX-HPC-ca-activation-forward-exponential-term*) (particle-v-index prt))
	  1.0d0)
      (nthorder-conc-particle-forward-rate (particle-concentration-particle prt)))))

(defun KCTX-HPC-ca-activation-backward (prt)
  (declare (optimize (safety 0) (speed 3) (space 0) (compilation-speed 0)))
  (return-markov-rate (nthorder-conc-particle-backward-rate (particle-concentration-particle prt))))


(particle-type-def
 `(KCTX-HPC
   (class . :MARKOV)
   (STATES . (C O I))
   (OPEN-STATES . (O))
   (STATE-TRANSITIONS . 
    ((C O KCTX-HPC-CA-ACTIVATION-FORWARD T)
     (O C KCTX-HPC-CA-ACTIVATION-BACKWARD T)
     (I C (SQUEEZED-EXPONENTIAL VOLTAGE :V-HALF -120.0 :K -10.0 :TAU-MIN 10.0))
     (O I (SQUEEZED-EXPONENTIAL VOLTAGE :V-HALF -64.0 :K -3.5 :TAU-MIN 0.1))))
   (reference-temp . 27.0)
   (qten . 1.0)
   (concentration-particle-type . KCTX-HPC-CA)))
   

;;; This concentration particle sets the C<->O transitions via
;;; KCTX-HPC-CA-ACTIVATION-FORWARD and KCTX-HPC-CA-ACTIVATION-BACKWARD.
(conc-particle-type-def
 '(KCTX-HPC-CA
        (class . :NTH-ORDER)
        (alpha . 1000000.0)		; Note that this is in units of mM(-n) since there is a
					; leading voltage term.
        (beta . 0.05)
        (tau-0 . 0.0d0)
        (power . 3)
        (qten . 1.0)
        (reference-temp . 27.0)
        (shell . 1)
        (conc-int-type . CA-IN-HPC)
                   ))


(channel-type-def
 '(H-HPC
   (iv-relation . :OHMIC)
   (gbar-source . :ABSOLUTE)
   (gbar-ref . 0.003)
   (e-rev . -17.0)
   (use-defined-rev . T)
   (ion-permeabilities . NIL)
   (q10 . 1.0)
   (reference-temp . 32.0)
   (v-particles . ((HY-HPC 1)))))


(particle-type-def
 `(HY-HPC
   (class . :HH-EXT)
   (valence . -2.0)
   (gamma . 1.0)
   (base-rate . 1.0)
   (v-half . -98.0)
   (tau-0 . 180.0)
   (IGNORE-TAU-VOLTAGE-DEPENDENCE . T)
   (reference-temp . 32.0)
   (FIXED-BOLTZMANN-REFERENCE-TEMPERATURE . 32.0)
   (qten . 1.0)))


(conc-int-type-def
 '(CA-IN-HPC
   (class . :MULTI-SHELL)
   (species . CA)
   (intra-p . T)
   (shell-2-p . T)
   (shell-3-p . T)
   (juxtamembrane-shell-thickness . 1.0)
   (inner-shell-thickness . 0.0)
   (alpha-s . 10.0e-5)
   (interdigitation-coefficient . 1.0)
   (diffusion-coefficient . (((1 2) 8.0e-6)
			     ((1 3) 0.0)
			     ((2 3) 8.0e-6)))
   (transmembrane-concentration . 2.0)
   (core-conc . 0.00105)
   (PUMP-TYPE-PARAMS . ((CA-HPC-MM 2)))
   (resting-free-conc . 5.0e-5)
   (instantaneous-buffer-enabled . T)
   (instantaneous-buffer-ratio . 20.0)))
