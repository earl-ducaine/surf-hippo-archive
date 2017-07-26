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

;
; The channel models for the canonical Hodgkin-Huxley channels, plus version fitted to the extended
; HH model.
;

; These are not adjusted to the correct reference temperatures.

(channel-type-def
 '(na-hh
   (gbar-density . 1200.0)
   (e-rev . 50.0)
   (ion-permeabilities . ((NA 1.0)))
   (v-particles . ((m-hh 3) (H-hh 1)))))


;; alpha_m = A (v-V0) / (exp((v-V0)/B) - 1)
;; A = -0.1e6    1/(Volts*sec)	= -0.1	1/(mV*msec)
;; B = -0.01     Volts		= -10	mV
;; V0= -0.040    Volts		= -40	mV


;; beta_m = A exp((v-V0)/B)
;; A = 4.0e3     1/sec		= 4.0	1/msec
;; B = -0.018    Volts		= -18	mV
;; V0= -0.065    Volts		= -65	mV

(particle-type-def
 `(m-hh
   (class . :hh)
   (alpha-function . ,#'(lambda (voltage)
			  (declare (optimize (speed 3) (space 0)) (single-float voltage))
			  (let ((v-40 (- voltage -40.0)))
			    (/ (* -0.1 v-40)
			       (1- (exp (/ v-40 -10.0)))))))
   (beta-function . ,#'(lambda (voltage)
			 (declare (optimize (speed 3) (space 0)) (single-float voltage))
			 (* 4.0 (exp (/ (- voltage -65.0) -18.0)))))))


;; alpha_h = A exp((v-V0)/B)
;; A = 70.0      1/sec		= 0.07	1/msec
;; B = -0.020    Volts		= -20	mV
;; V0= -0.065    Volts		= -65	mV

;; beta_h = A / (exp((v-V0)/B) + 1)
;; A = 1.0e3 1/sec		= 1.0	1/msec
;; B = -0.010 Volts		= -10	mV
;; V0= -0.035 Volts		= -35	mV

(particle-type-def
 `(h-hh
   (class . :hh)
   (alpha-function . ,#'(lambda (voltage)
			  (declare (optimize (speed 3) (space 0)) (single-float voltage))
			  (* 0.07 (exp (/ (- voltage -65.0) -20)))))
   (beta-function . ,#'(lambda (voltage)
			 (declare (optimize (speed 3) (space 0)) (single-float voltage))
			 (/ 1.0 (1+ (exp (/ (- voltage -35.0) -10.0))))))))




(channel-type-def
 '(na-hh-fit
   (gbar-density . 1200.0)
   (e-rev . 50.0)
   (ion-permeabilities . ((NA 1.0)))
   (v-particles . ((m-hh-fit 3) (H-hh-fit 1)))))

(particle-type-def
 '(m-hh-fit
   (class . :hh-ext)
   (VALENCE . 2.7)
   (GAMMA . 0.4)
   (BASE-RATE . 1.2)
   (V-HALF . -40.0)
   (TAU-0 . 0.07)
					;	(QTEN . 3.0)
					;	(reference-temp . 6.3)		; ??
   ))

(particle-type-def
 '(h-hh-fit
   (class . :hh-ext)
   (VALENCE . -3.7)
   (GAMMA . 0.4)
   (BASE-RATE . 0.07)
   (V-HALF . -62.0)
   (TAU-0 . 0.9)
					;	(QTEN . 3.0)
					;	(reference-temp . 6.3)		; ??
   ))





(channel-type-def
 '(dr-hh
   (gbar-density . 360.0)
   (e-rev . -77.0)
   (ion-permeabilities . ((K 1.0)))
   (v-particles . ((n-hh 4)))))


;; alpha_n = A (v-V0) / (exp((v-V0)/B) - 1)
;; A = -10.0e3   1/(Volts*sec)	= -0.01	1/(mV*msec)
;; B = -0.01     Volts		= -10	mV
;; V0= -0.055    Volts		= -55	mV


;; beta_n = A exp((v-V0)/B)
;; A = 125.0     1/sec		= 0.125	1/msec
;; B = -0.080    Volts		= -80	mV
;; V0= -0.065    Volts		= -65	mV


(particle-type-def
 `(n-hh
   (class . :hh)
   (alpha-function . ,#'(lambda (voltage)
			  (declare (optimize (speed 3) (space 0)) (single-float voltage))
			  (let ((v-55 (- voltage -55.0)))
			    (/ (* -0.01 v-55) (1- (exp (/ v-55 -10.0)))))))
   (beta-function . ,#'(lambda (voltage)
			 (declare (optimize (speed 3) (space 0)) (single-float voltage))
			 (* 0.125 (exp (/ (- voltage -65.0) -80.0)))))))


(channel-type-def
 '(dr-hh-fit
   (gbar-density . 360.0)
   (e-rev . -77.0)
   (ion-permeabilities . ((K 1.0)))
   (v-particles . ((n-hh-fit 4)))))

(particle-type-def
 '(n-hh-fit
   (class . :hh-ext)
   (VALENCE . 1.5)
   (GAMMA . 0.9)
   (BASE-RATE . 0.14)
   (V-HALF . -51.0)
   (TAU-0 . 1.0)
					;	(QTEN . 3.0)
					;	(reference-temp . 6.3)		; ??
   ))



