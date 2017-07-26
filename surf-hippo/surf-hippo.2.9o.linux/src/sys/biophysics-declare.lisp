;;; -*- mode: lisp; Syntax: Common-lisp; package: surf ; base: 10;   -*-
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


;;; SYS Source file: biophysics-declare.lisp

;;; SYS Source file: biophysics-declare.lisp

;; ****************************************
;;; Various declarations for reality based variables.

;; ****************************************


(in-package "SURF-HIPPO")

;; ****************************************
;;
;; Numerical / Biophysical Constants
;;
;; ****************************************
;;; 7/5/92 - CMU CL is uptight about number types, especially with respect to structure slots. We are
;;; keeping everything in single precision, so we will define a single precision pi internal to the
;;; SURF package.

(defconstant pi-single (COERCE user::PI 'SINGLE-FLOAT))
(defconstant pi-over-2 (/ pi-single 2.0))
(defconstant pi-over-4 (/ pi-single 4.0))

(defconstant ZERO 0e0)

(defconstant Faraday 9.648e4 "Faraday's constant") ; Coulombs/mole


;; Joules/(DegreesKelvin*mole) = (Volts*Coulombs)/(DegreesKelvin*mole)
;;  Boltzmanns-constant	k = 1.380622e-23 Joule per Kelvin = 1.380622e-23 (Volts*Coulombs)/ Kelvin

;; Faraday / R = 9.648e4 Coulombs/mole / 8.314 (Volts*Coulombs)/(DegreesKelvin*mole)
;;             = 11604.522 degreesK/volts

;; F/k = 9.648e4 Coulombs/mole /  1.380622e-23 (Volts*Coulombs)/ DegreesKelvin
;;     = 6.9881546e+27 DegreesK / (mole * Volts)

;; FV/RT => dimensionless

(defconstant Boltzmanns-constant 1.380622e-23) ; Joule per Kelvin
(defconstant electronic-charge	1.6021917e-19) ; coulomb

;; Gas constant R in (Volts*Coulombs)/(DegreesKelvin*mole) = k * 6.022169e23/mole
;;                     = 1.380622e-23 (Volts*Coulombs)/ Kelvin * 6.022169e23/mole
;;                     = 8.31434 (volts coulombs) / (degreesK mole)

(defconstant GasConstant 8.31434 "Gas Constant") 


(defconstant FoverR (/ Faraday GasConstant) "Faraday / GasConstant.")

(defvar *f/rt* (/ foverr *temperature*))

(defvar nernst-eqn-const*1000*temp (/ (* 1000.0 *Temperature*) FoverR))

(defconstant Eca-Nernst-eqn-const (/ 0.5 foverR))

(defvar Eca-Nernst-eqn-const*1000*temp
  (* 1000.0				;convert to millivolt units
     Eca-Nernst-eqn-const
     *Temperature*))

(defconstant Plancks-constant	6.626196e-34) ; Joule second

;; Diffusion constant D of ions in aqueous solutions (Taken from Hille, B., Ionic Channels of
;; Excitable Membranes, 1984, Sinaur).
;;
;; Units are cm^2 sec^-1, Temperature = 25 C
(defvar *D_NA* 1.33e-5
  "Diffusion constant of NA in aqueous solutions at 25deg C (Hille, B., Ionic Channels of Excitable
Membranes, 1984).")

(defvar *D_LI* 1.03e-5
  "Diffusion constant of LI in aqueous solutions at 25deg C (Hille, B., Ionic Channels of Excitable
Membranes, 1984).")

(defvar *D_K* 1.96e-5
  "Diffusion constant of K in aqueous solutions at 25deg C (Hille, B., Ionic Channels of Excitable
Membranes, 1984).")

(defvar *D_CS* 2.06e-5
  "Diffusion constant of CS in aqueous solutions at 25deg C (Hille, B., Ionic Channels of Excitable
Membranes, 1984).")

(defvar *D_CL* 2.03e-5
  "Diffusion constant of CL in aqueous solutions at 25deg C (Hille, B., Ionic Channels of Excitable
Membranes, 1984).")

(defvar *D_BR* 2.08e-5
  "Diffusion constant of BR in aqueous solutions at 25deg C (Hille, B., Ionic Channels of Excitable
Membranes, 1984).")

(defvar *D_TEA* 0.87e-5
  "Diffusion constant of TEA in aqueous solutions at 25deg C (Hille, B., Ionic Channels of Excitable
Membranes, 1984).")

(defvar *D_MG* 0.71e-5
  "Diffusion constant of MG in aqueous solutions at 25deg C (Hille, B., Ionic Channels of Excitable
Membranes, 1984).")

(defvar *D_CA* 0.79e-5
  "Diffusion constant of CA in aqueous solutions at 25deg C (Hille, B., Ionic Channels of Excitable
Membranes, 1984).")




;; ****************************************
;;
;; Reversal Potentials - Each cell type has a baseline value for the reversal potential and
;; concentrations of K+, Na+, Ca++, Cl-, and ??. Each cell type also has a baseline value for the
;; resting potential. The default value for these potentials are obtained from the global variables
;; below.
;;
;; ****************************************

(defvar *e-holding* -70.0)		;mvolts
(defvar *e-leak* -70.0 "Default membrane leak reversal potential, mV.")
(defvar *dendrite-e-leak* -70.0)				;constant leakage battery (mV)


;; Concentrations: all concentration related calculations are made assuming unity activity coefficients.

;; ****************************************


;; Values for mammalian skeletal muscle, Hille '84, Chapter 1, Table 3.
(defvar *na-conc-extra* 145.0 "mM")

					; Note that Vandenberg and Bezanilla 1991 used a value of on
					; the order of 500mM Na+ in some of their experiments with
					; squid axon.


(defvar *na-conc-intra* 12.0 "mM")

(defvar *e-na* 64.0 "mV")		; mvolts - at 300 deg (NERNST-POTENTIAL 12.0 145.0 1.0) = 64.4mV

(defvar *fix-e-na* t
  "Fix the global *E-NA* - otherwise, *E-NA* is updated as with the Nernst equation using
*TEMPERATURE* and the global *NA-CONC-INTRA* and *NA-CONC-EXTRA*.")

;; Values for mammalian skeletal muscle, Hille '84, Chapter 1, Table 3.  
(defvar *k-conc-extra* 4.0 "mM")
(defvar *k-conc-intra* 155.0 "mM")

(defvar *e-k*  -95.0 "mV")			; mvolts - at 300 deg (NERNST-POTENTIAL 155.0 4.0 1.0) = 94.5mV

(defvar *fix-e-k* t
  "Fix the global *E-K* - otherwise, *E-K* is updated as with the Nernst equation using
*TEMPERATURE* and the global *K-CONC-INTRA* and *K-CONC-EXTRA*.")

;; Values for mammalian skeletal muscle, Hille '84, Chapter 1, Table 3.
(defvar *e-cl*  -90.0 "mV")			; mvolts - at 300 deg (NERNST-POTENTIAL 4.0 120.0 -1.0) = 87.9mV
(defvar *cl-conc-extra* 120.0 "mM")

(defvar *cl-conc-intra* 4.0 "mM")

(defvar *fix-e-cl* t "Fix the global *E-Cl* - otherwise, *E-Cl* is updated as with the Nernst
equation using *TEMPERATURE* and the global *Cl-CONC-INTRA* and *Cl-CONC-EXTRA*.")

(defvar *e-ca* 110.0 "mV")
(defvar *ca-conc-extra* 1.8 "mM")		;Extra-cellular Ca++ concentration [mmol/liter]
					;Hille says 1.5 mM Ca out, <10e-7 mM in.  Segal and
					;Barker, 1986 use 4.0 mM Ca out Madison and Nicoll,
					;1982 use 2.5 mM Ca out Blaxter et al, 1986 use ACSF
					;with 3.25 mM Ca Wong and Prince, 1981 use 2.0 mM Ca

					; Note that Vandenberg and Bezanilla 1991 used a value of
					; 10mM Ca++ in some of their experiments with squid axon.


(defvar *ca-conc-intra* 5.0e-5 "mM")	;Resting intra-cellular Ca++ concentration [mmol/liter]

(defvar *fix-e-ca* t
  "Fix the global *E-Ca* - otherwise, *E-Ca* is updated
as with the Nernst equation using *TEMPERATURE* and
the global *Ca-CONC-INTRA* and *Ca-CONC-EXTRA*.")

(defvar *mg-conc-extra* 1.5 "mM")		; Typical used for cortical slice Ringer's solution
					; Note that Vandenberg and Bezanilla 1991 used a value of
					; 50mM Mg++ in some of their experiments with squid axon.

(defvar *mg-conc-intra* 1.5 "mM")		; Typical used for cortical slice patch solution

(defvar *e-mg* 0.0 "mV")


;; ****************************************
;;
;; Passive Components - used as defaults in create-cell-type
;;
;; ****************************************


(defvar *r-mem* 40000.0 "Default value of membrane resistivity (ohms cm^2)")
(defvar *r-a* 200.0)			; old default value of segment axial resistivity (ohms cm).
(defvar *r-i* 200.0 "Default value of segment axial resistivity (ohms cm)")
(defvar *r-mem-soma* 40000.0 "Default value of soma membrane resistivity (ohms cm^2)")
(defvar *cap-mem* 0.7 "Default value of membrane capacitance (uF/cm^2)")
(defvar *cap-mem-dendrite* 0.7 "Default value of membrane capacitance (uF/cm^2)")




(defvar *r-extracellular* 200.0 "Default value of extracellular resistivity (ohms cm)")






(defvar *integral-base* *e-holding*)	; The reference  for INTEGRATE-PLOT-DATA



(proclaim '(single-float pi-over-2 pi-over-4 pi-single
	    Faraday GasConstant FoverR  *F/RT*

	    *r-extracellular*
	    
	    *r-mem* *r-a* *r-i* *r-mem-soma* *cap-mem*

	    *e-k* *e-ca* *e-k* *e-holding* *e-l* *ca-conc-extra*
	    Eca-Nernst-eqn-const*1000*temp nernst-eqn-const*1000*temp
	    ))