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
Taken from

@InCollection{Yam-Koc-Ada-89,
  Author    = {Yamada, W. M. and Koch, C. and Adams, P. R.},
  Title     = {Multiple Channels and Calcium Dynamics},
  BookTitle = {Methods in Neuronal  Modeling},
  Publisher = {Mit Press/Bradford Books},
  Year      = {1989},
  Editor    = {Koch, C. and Segev, I.},
  Chapter   = {4}
  }

|#


;; *****************************
;; Potassium C current
;; *****************************

(channel-type-def
 '(kc-yka89
   (gbar . 1.20)			; uS
   (e-rev . -105.0)			; yka use a pottasium integrator to compute Ek, this value
					; from Mcc-Hug-92.
   (use-defined-e-rev . t)
   (QTEN . 1)          	 
   (ion-permeabilities . ((K 1.0)))
   (conc-particles . ((kcm-yka89 1)))))


(conc-particle-type-def
 `(kcm-yka89
   (class . :generic)
   (QTEN . 1)          	 
   (power . 1)
   (alpha-function .  ,#'(lambda (conc voltage type)
			   (declare (optimize (speed 3) (space 0)) 
				    (double-float conc voltage)
				    (ignore type))
			   (* 250 conc (exp (/ voltage 24)))))
			 
   (beta-function . ,#'(lambda (conc voltage type)
			 (declare (optimize (speed 3) (space 0)) 
				  (double-float voltage)
				  (ignore conc type))
			 (* 0.1 (exp (/ voltage -24)))))
   (conc-int-type . ca-in-gen)
   (shell . 1)))



