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

;
; Various Na channels.
;


(in-package "SURF-HIPPO")


;;  Rhodes & Gray, Neural Comp. 1994
(channel-type-def
 '(na-rho
   (gbar-density . 300.0)
   (e-rev . 50.0)
   (ion-permeabilities . ((NA 1.0)) )
   (QTEN . 3)
   (reference-temp . 37.0)
   (v-particles . ((nam-rho 2) (nah-rho 1)) )))




;; particule d'activation du canal SODIUM
;;
(particle-type-def
 `(nam-rho 
   (class . :hh)
   (alpha-function . ,#'(lambda (voltage)
			  (declare (optimize (speed 3) (space 0)) 
				   (single-float voltage))
			  (let ((diff (+ voltage 46.9)))
			    (/ (* -320.0 diff) (1- (exp (/ diff -4.0)))))))			    
   (beta-function . ,#'(lambda (voltage)
			 (declare (optimize (speed 3) (space 0)) 
				  (single-float voltage))
			 (let ((diff (+ voltage 19.9)))
			   (/  (* 280.0 diff)  (1- (exp (/ diff 5.0)))))))))



;; particule d'inactivation du canal SODIUM
(particle-type-def
 `(nah-rho
   (class . :hh)
   (alpha-function . ,#'(lambda (voltage)
			  (declare (optimize (speed 3) (space 0)) 
				   (single-float voltage))
			  (* 128.0 (exp (/ (+ voltage 43.0) -18.0)))))
   (beta-function . ,#'(lambda (voltage)
			 (declare (optimize (speed 3) (space 0)) 
				  (single-float voltage))
			 (/ 4000.0 (1+ (exp (/ (+ voltage 20.0) -5.0))))))))


