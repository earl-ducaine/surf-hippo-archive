;;; -*- Mode: Lisp; Syntax: Common-lisp; Package: #-PARALLEL SURF #+PARALLEL *SURF; Base: 10; -*-
;;; (c) Copyright 1990, Lyle Borg-Graham, MIT Center for Biological Information Processing

#-parallel
(in-package "SURF-HIPPO"
	    :use '("COMMON-LISP-USER" "COMMON-LISP")
	    :nicknames '("SURF"))
#+parallel 
(in-package "*SURF")

;;;
;;;                        **** CURRENTS FILE ******
;;;
;;;   This file contains functions and parameters that determine the membrane currents.             
;;;   Most parameters are derived from HIPPO, the simulation of hippocampal pyramidal neurons.

;; NOTE: This file is under development to make it fully compatible with the SURF system.

;; NOTE: MOST OF THE FUNCTIONS DEFINED HERE ARE *NOT* USED BY THE SURF SIMULATOR. THE SIMULATOR DOES
;; REFERENCE THE CHANNEL PARAMETERS DEFINED HERE. THIS FILE WAS PART OF THE ORIGINAL HIPPO SIMULATOR.


;;;          **** UNITS ****
;;;
;;;   All dimensions are as follows (except when indicated) -
;;;
;;;   Distance (micrometers = 1.0e-4 centimeters)
;;;   Surface area (square-cm)
;;;   Volume (cubic micrometers = 1.0e-15 liter = 1.0e-12 cubic centimeter)
;;;   Concentration (millimoles per liter [millimolar])
;;;   
;;;   Time (milliseconds)
;;;
;;;   Voltage (millivolts)
;;;   Current (nano-amps)
;;;
;;;   Specific membrane capacitance (microfarads/square-cm)
;;;   Specific membrane resistivity (ohms-cm-cm)
;;;   Capacitance (nanofarads)
;;;   Resistance (mega-ohms)
;;;
;;;   Conductance (microsiemens)
;;;   Conductance density (pS/square-micron) ;(millisiemens/square-cm)



;;; ALPHA and BETA These functions give the voltage dependant rate constants for the single barrier model, where alpha is the
;;; forward rate constant and beta is the backward rate constant. "v-half" is the voltage at which the forward
;;; and backward rate constants are equal. Note that there are two aspects of the temperature dependence of
;;; these rate constants. The first derives from the voltage-dependent Boltzmann distribution, which is
;;; explicitely calculated in these functions. The second arises in a lumped "Qten" factor that is strictly a
;;; coefficient for the "base-rate", that is the rate derived from the original free-energy changes of the gating
;;; particle. Since this factor depends on each current, the Qten factor is not included here but in the time constant
;;; functions for each gating variable.



(defun alpha (voltage v-half base-rate valence gamma)
  (declare (single-float voltage v-half base-rate valence gamma))
  (let ((exponent (/ (* (-  voltage v-half ) 1.0e-3 valence *faraday gamma) (* *R *temperature*))))
    (declare (single-float exponent))
    (setq exponent (cond ((> exponent 20.0) 20.0) ((< exponent -10.0) -10.0)
			 (t exponent)))
    (* base-rate (exp exponent))))

(defun beta (voltage v-half base-rate valence gamma)
  (declare (single-float voltage v-half base-rate valence gamma))
  (let ((exponent (/ (* (-  v-half voltage) 1.0e-3 valence *faraday (- 1.0 gamma)) (* *R *temperature*))))
    (declare (single-float exponent))
    (setq exponent (cond ((> exponent 20.0) 20.0)
			 ((< exponent -10.0) -10.0)
			 (t exponent)))
    (* base-rate (exp exponent))))


;;;    Generic Inward Anamolous Rectifier Current
;;;
;;;  AR current as reported by Hotson, Prince, and Schwartzkroin. Initially we shall assume that this is a mixed
;;;  Na-Ca current, that it is non-inactivating, and that the time constant is negligible.

;;; E-IAR For now reflects contribution of Na .
(defvar *e-iar 50.0)

;;;; conductance in micro-siemans
(defvar *gbar-iar .0070)

;;;; T-X-IAR Time constant for activation
(defun t-x-iar (voltage)
  (declare (ignore voltage))
  .000001)

;;;; X-IAR-INF Steady state value for the activation variable.
(defvar *x-iar-base)
(defun x-iar-inf (voltage)
  (let ((midpoint -55.0)(steepness 5.0))
    (+ (/ (- 1.0 *x-iar-base) (+ 1.0 (exp (/ (- midpoint voltage) steepness))))
       *x-iar-base)))

;;;; IAR-CURRENT
(defun iar-current (x-iar voltage)
  (* *gbar-iar x-iar (- voltage *e-iar)))



;;;      I-C-current *******************
;;; 
;;;   The Ca dependent K-current
;;;
;;; For now make it analogous to the A current, except that the C current is faster and is dependent on the
;;; concentration of Ca++ in the shell in the same way as the AHP current.

;;;  Conductance in micro-siemans
(defvar *gbar-c 0.40)
(defvar *g-c-dens 0)

(defvars-w-value (*v-half-cx -65.0) (*alpha-base-rate-cx 0.007)  (*valence-cx 25.0) (*gamma-cx 0.20)
		 (*v-half-cy -60.0) (*alpha-base-rate-cy 0.003) (*valence-cy 20.0) (*gamma-cy 0.2)
		 (*base-tcx 0.25)(*base-tcy 15))

(defvars-w-value (*alpha-c 1000.0)  (*beta-c 0.125))
(defvars-w-value (*tau-alpha-c 0.0001)  (*tau-beta-c 8.0))


;;; W-C-INF w-c is calcium-dependent gating variable for C-current
(defun w-c-inf (calc-conc-shell)
  (/ (* *alpha-c calc-conc-shell calc-conc-shell calc-conc-shell)
      (+ *beta-c (* *alpha-c calc-conc-shell calc-conc-shell calc-conc-shell ))))

;;; T-W-C
(defun t-w-c (calc-conc-shell)
  (let ((tau  (/ 1.0 (+ *beta-c (* *alpha-c calc-conc-shell calc-conc-shell calc-conc-shell)))))
    (* *qten-factor-at-27 (if (< tau 0.2) 0.20 
    tau))))

;;; A-X-C
(defun a-x-c (voltage)
  (alpha voltage *v-half-cx *alpha-base-rate-cx *valence-cx *gamma-cx))

;;; B-X-C
(defun b-x-c (voltage)
  (beta voltage *v-half-cx *alpha-base-rate-cx *valence-cx *gamma-cx))

;;; A-Y-C
(defun a-y-c (voltage)
  (beta voltage *v-half-cy *alpha-base-rate-cy *valence-cy *gamma-cy))

;;; B-Y-C
(defun b-y-c (voltage)
  (alpha voltage *v-half-cy *alpha-base-rate-cy *valence-cy *gamma-cy))

;;; X-C-INF
;;; 	x-inf is activation variable for C-current 
(defvar *x-c-inf-midpoint 0.0)
(defun x-c-inf (voltage)
  (/ (a-x-c voltage) (+ (a-x-c voltage) (b-x-c voltage))))
 
;;; Y-C-INF
;;; 	y-inf is inactivation variable for c-current
(defvar *y-c-inf-midpoint 5.0)
(defun y-c-inf (voltage)
  (/ (a-y-c voltage) (+ (a-y-c voltage) (b-y-c voltage))))
;  (let ((steepness 2.0))	;Segal and Barker; Segal, Rogawski, and Barker
;    (/ 1.0	 (+ 1.0 (exp (/ (- voltage *y-c-inf-midpoint) steepness))))))

;;; T-X-C
;;; 	tau-C-current(activation) - msec	(estimate)
(defvar *t-x-c .50)
(defun t-x-c (&optional voltage)
  (let ((tx (/ 1.0 (+ (a-x-c voltage) (b-x-c voltage)))))
    (* *qten-factor-at-27 (if (< tx *base-tcx) *base-tcx tx))))
;(defun t-x-c (voltage)				;tau tail current (Brown and Griffith) (msec)
;  (cond ((< voltage -30.0) (* 20.0 (exp (/ (+ voltage 40.0) 18.0))))
;	(t (* 20.0 (exp (/ (- 40.0 (+ voltage 60.0)) 18.0))))))

;;; T-Y-C
;;; 	tau-C-current(inactivation) - msec
(defvar *t-y-c 1.0)
(defun t-y-c (&optional voltage)
  (let ((ty (/ 1.0 (+ (a-y-c voltage) (b-y-c voltage)))))
    (* *qten-factor-at-27 (if (< ty *base-tcy) *base-tcy ty))))



(defun modify-c ()
  (setq *alpha-c (/ 1.0 *tau-alpha-c)	
	*beta-c (/ 1.0 *tau-beta-c)))

;;; C-CURRENT Function to calcuate the C current.
(defun c-current (x-c y-c w-c v)
  (* (g-c *gbar-c x-c y-c w-c)
     (- v *e-k)))

;;; G-C
(defun g-c (gbar-c x-c y-c w-c)
  (if (< x-c 0.01) (setq x-c 0.0))
  (* gbar-c x-c  x-c x-c x-c y-c   w-c))

;;;X-C-EFF, Y-C-EFF
(defun x-c-eff (x-c)
  (if (< x-c 0.01) 0.0
  (expt x-c 4.0)))
(defun y-c-eff (y-c)
  (expt y-c 1.0))



;;;       I-M current   ******************
;;; 
;;;    The muscarinic -sensitive K current of Paul Adams


(defvars-w-value
  (*m-block 1.0)  (*base-tmx 10)    (*v-half-mx -45.0) (*base-rate-mx 0.0015) (*valence-mx 5) (*gamma-mx .5))

;;;  I-M conductance - Only activate between -70mv and -30mv (micro-siemans)
(defvar *gbar-m .005)
(defvar *g-m-dens 0)


;;; M-CURRENT
(defun m-current (x-m v)
  (* *gbar-m x-m (- v *e-k)))


;;;  I-M time constant - from two values given by Paul (msec)
;;;  Constanti says ~125 ms @ -40 mv (olfactory cortical cells)
;;; T-X-M
(defun t-x-m (voltage)
  (let* ((b (alpha voltage *v-half-mx *base-rate-mx *valence-mx *gamma-mx))
	(a (beta voltage *v-half-mx *base-rate-mx *valence-mx *gamma-mx))
	(tx (/ *qten-factor-at-25 (+ a b))))
      (if (< tx (* *qten-factor-at-25 *base-tmx)) (* *qten-factor-at-25  *base-tmx) tx)))

;;; X-M-INF	x-inf is activation variable for M-current
(defun x-m-inf (voltage)
  (let ((b (beta voltage *v-half-mx *base-rate-mx *valence-mx *gamma-mx))
	(a (alpha voltage *v-half-mx *base-rate-mx *valence-mx *gamma-mx)))
    (/ a (+ a b))))


;;;     I-Q current   *******************
;;
;;; This is the outward "anomalous rectifier" current that is activated by hyperpolarizing the cell.
;;; Ref. - Segal and Barker, Halliwell and Adams

;;; E-Q      Q current may be a mixed conductance.
(defvar *e-q -65.0)

;;;  I-Q conductance (micro-siemans)
(defvar *gbar-q .002)				;About 2nS at full activation (Paul)
(defvar *g-q-dens 0)


;;; Q-CURRENT
(defun q-current (x-q v)
  (* *gbar-q x-q (- v *e-q)))

;;; X-Q-INF
(defun x-q-inf (v)
  (/ 1 (+ 1 (exp (/ (+ v 84.0) 4.0)))))

;;; T-X-Q
(defun t-x-q (v)
  (* *qten-factor-at-25-m			;Paul reports Q-10 for both M and Q currents to be ~5.
     (* 1200.0 (+ (/ 1 (+ 1 (exp (/ (+ v 85.0) -6.0)))) .1))))




;;; 	DR-current ****************

(defvars-w-value
  (*dr-block 1.0)
  (*base-txdr 0.50)  (*base-tydr 6.0)
  (*v-half-drx -28.0) 
  (*base-rate-drx 0.008) (*valence-drx 12) (*gamma-drx .95)
  (*v-half-dry -45.0) 
  (*base-rate-dry 0.0004) (*valence-dry 9) (*gamma-dry 0.2))

(defvar *e-dr -73.50)				;I-DR reversal potential

;;; DR conductance (microsiemans)
(defvar *gbar-dr 0.7)				;Segal reports 0.350

(defvar *g-dr-dens 20)				;No real reason for this particular value




;;; Y-DR-INF	y-inf is inactivation variable for DR-current
;;; 		Segal and Barker
(defun y-dr-inf (voltage)
  (let ((b (alpha voltage *v-half-dry *base-rate-dry *valence-dry *gamma-dry))
	(a (beta voltage *v-half-dry *base-rate-dry *valence-dry *gamma-dry)))
    (/ a (+ a b))))

;;; T-Y-DR	tau-DR-current(inactivation) - 
;;; 		Segal and Barker 40 msec
(defun t-y-dr (voltage)
  (let* ((b (alpha voltage *v-half-dry *base-rate-dry *valence-dry *gamma-dry))
	(a (beta voltage *v-half-dry *base-rate-dry *valence-dry *gamma-dry))
	(ty (/ *qten-factor-at-30 (+ a b))))
      (if (< ty (* *qten-factor-at-30 *base-tydr)) (* *qten-factor-at-30  *base-tydr) ty)))

;;; X-DR-INF
;;; 	x-inf is activation variable for DR-current
;;; 		Segal and Barker
(defun x-dr-inf (voltage)
    (let ((a (alpha voltage *v-half-drx *base-rate-drx *valence-drx *gamma-drx))	;
	  (b (beta voltage *v-half-drx *base-rate-drx *valence-drx *gamma-drx)))
      (/ a (+ a b))))

;;; T-X-DR
;;; 	tau-DR-current(activation) - msec
;;; 	Segal and Barker 180 ms < -30mv,6 ms else

(defun t-x-dr (voltage)
    (let* ((a (alpha voltage *v-half-drx *base-rate-drx *valence-drx *gamma-drx))
	  (b (beta voltage *v-half-drx *base-rate-drx *valence-drx *gamma-drx))
	  (tx (/ *qten-factor-at-30 (+ a b))))
      (if (< tx (* *qten-factor-at-30 *base-txdr)) (* *qten-factor-at-30 *base-txdr) tx)))






;;; DR-CURRENT
(defun dr-current (x-dr y-dr v)
  (* (g-dr *gbar-dr x-dr y-dr) (- v *e-dr)))

;;; G-DR
(defun g-dr (gbar-dr x-dr y-dr)
  (* gbar-dr *dr-block 
     x-dr x-dr   x-dr y-dr ))

;;;X-DR-EFF, Y-DR-EFF
(defun x-dr-eff (x-dr)
  (expt x-dr 3.0))
(defun y-dr-eff (y-dr)
  (expt y-dr 1.0))



;;; 	AHP-current ****************
;;;
;;; Iahp will have two voltage-dependent  inactivation particles, y and z, and
;;; a calcium-dependent gating particle, w.

;;; AHP conductance (microsiemans)
(defvar *gbar-ahp  0.35)	
(defvar *g-ahp-dens 0)


;;; AHP-CURRENT
(defun ahp-current (z-ahp y-ahp w-ahp  v)
  (* (g-ahp *gbar-ahp  z-ahp y-ahp w-ahp)
     (- v *e-k)))			

;;; G-AHP - new version
(defun g-ahp (gbar-ahp  z-ahp y-ahp w-ahp)
  (* gbar-ahp 1.0  y-ahp y-ahp w-ahp z-ahp))

(defun y-ahp-eff (y-ahp)
  (expt y-ahp 2.0))

(defvar *alpha-ahp 1.0e5)
(defvar *beta-ahp (/ 1.0 200))
(defvars-w-value (*tau-alpha-ahp 1.0e-5)  (*tau-beta-ahp 200.0))
(defvars-w-value (*v-half-ahpz -72.0) (*alpha-base-rate-ahpz 2.0e-4)
  (*valence-ahpz 12.0) (*gamma-ahpz 0)
  (*v-half-ahpy -50.0) (*alpha-base-rate-ahpy 0.015)
  (*valence-ahpy 15.0) (*gamma-ahpy 0.8)
  (*base-tahpz 120.0)(*base-tahpy 2.5))

;;; W-AHP-INF w-ahp is calcium-dependent gating variable for AHP-current
(defun w-ahp-inf (calc-conc-shell )
  (/ (* *alpha-ahp calc-conc-shell calc-conc-shell calc-conc-shell )
      (+ *beta-ahp (* *alpha-ahp calc-conc-shell calc-conc-shell calc-conc-shell ))))

;;; T-W-AHP
(defun t-w-ahp (calc-conc-shell)
  (let ((tau  (/ 1.0 (+ *beta-ahp (* *alpha-ahp calc-conc-shell calc-conc-shell calc-conc-shell)))))
    (* *qten-factor-at-27 (if (< tau 0.002) 0.0020 
    tau))))

;;; Y-AHP-INF	y-inf is inactivation variable for AHP-current
(defun y-ahp-inf (voltage)
  (let ((b (alpha voltage *v-half-ahpy *alpha-base-rate-ahpy *valence-ahpy *gamma-ahpy))
	(a (beta voltage *v-half-ahpy *alpha-base-rate-ahpy *valence-ahpy *gamma-ahpy)))
    (/ a (+ a b))))

;;; T-Y-AHP	tau-AHP-current(inactivation) - msec
(defun t-y-ahp (voltage)
  (let* ((b (alpha voltage *v-half-ahpy *alpha-base-rate-ahpy *valence-ahpy *gamma-ahpy))
	(a (beta voltage *v-half-ahpy *alpha-base-rate-ahpy *valence-ahpy *gamma-ahpy))
	(ty (/ 1.0 (+ a b))))
      (if (< ty *base-tahpy) *base-tahpy ty)))


;;; Z-AHP-INF
;;; 	z-inf is activation variable for AHP-current
(defun z-ahp-inf (voltage)
    (let ((b (alpha voltage *v-half-ahpz *alpha-base-rate-ahpz *valence-ahpz *gamma-ahpz))	;
	  (a (beta voltage *v-half-ahpz *alpha-base-rate-ahpz *valence-ahpz *gamma-ahpz)))
      (/ a (+ a b))))

;;; T-Z-AHP
;;; 	tau-AHP-current(activation) - msec
(defun t-z-ahp (voltage)
    (let* ((b (alpha voltage *v-half-ahpz *alpha-base-rate-ahpz *valence-ahpz *gamma-ahpz))
	  (a (beta voltage *v-half-ahpz *alpha-base-rate-ahpz *valence-ahpz *gamma-ahpz))
	  (tz (/ 1.0 (+ a b))))
      (if (< tz *base-tahpz) *base-tahpz tz)))



(defun modify-ahp ()
  (setq *alpha-ahp (/ 1.0 *tau-alpha-ahp)	
	*beta-ahp (/ 1.0 *tau-beta-ahp)))



;;; 	A-current *************
;;;
;;; Zbicz and Weight report that I-a activates in <10ms and decays over 
;;; several hundred ms (380ms @-50 to -40mv) (32 degreesC). However, 4-AP sensitive tail
;;; currents which have
;;; time constants of a few hundred ms in reponse to hyperpolarizing pulses to -54mv suddenly
;;; disappear when the clamp is below -54mv, suggesting that the time constant for inactivation
;;; is very rapid for potentials below -54mv, i.e. "The failure to observe any 4-AP-sensitive
;;; tail currents negative to -54mv suggests that the 4-AP-sensitive transient current
;;; deactivates very rapidly upon hyperpolerization."



(defvars-w-value
  (*base-txa 1.0)  (*base-tya 24.0)
  (*v-half-ax -52.0) (*base-rate-ax 0.2) (*valence-ax 3.5) (*gamma-ax 0.8)
  (*v-half-ay -72.0) (*base-rate-ay 0.0015) (*valence-ay 7) (*gamma-ay 0.4))

;;; A-current conductance (microsiemans)
(defvar *gbar-a  .50 )	
(defvar *g-a-dens 10)				;no real reason for this particular value

;;; X-A-INF
;;; 	x-inf is activation variable for A-current (- not confirmed sigmoid)
(defun x-a-inf (voltage)
;Segal and Barker; Segal, Rogawski, and Barker -  z=3.67,vhalf=-30
;Zbicz and Weight - z=8.5,vhalf=-45
    (let ((a (alpha voltage *v-half-ax *base-rate-ax *valence-ax *gamma-ax))	;
	  (b (beta voltage *v-half-ax *base-rate-ax *valence-ax *gamma-ax)))
      (/ a (+ a b))))

;;; Y-A-INF
;;; 	y-inf is inactivation variable for A-current
(defun y-a-inf (voltage)
;Segal and Barker; Segal, Rogawski, and Barker - z=4.28,vhalf=-70
;Z & W = z=8,vhalf=-55
  (let ((b (alpha voltage *v-half-ay *base-rate-ay *valence-ay *gamma-ay))
	(a (beta voltage *v-half-ay *base-rate-ay *valence-ay *gamma-ay)))
    (/ a (+ a b))))

;;; T-X-A
;;; 	tau-A-current(activation) - msec	(estimate)
(defvars-w-value (*t-x-a-1 3.0)(*t-x-a-2 5.0)(*t-y-a-1 5.0))
(defun t-x-a (&optional voltage)
;Segal and Barker; Segal, Rogawski, and Barker. Measured from V-holding = -70mv to steps up to -20mv
;Z  & W  Probably more of an estimate, i.e. "< 10ms"".
    (let* ((a (alpha voltage *v-half-ax *base-rate-ax *valence-ax *gamma-ax))
	  (b (beta voltage *v-half-ax *base-rate-ax *valence-ax *gamma-ax))
	  (tx (/ *qten-factor-at-30 (+ a b))))
      (if (< tx (* *qten-factor-at-30 *base-txa)) (* *qten-factor-at-30 *base-txa) tx)))

;;; T-Y-A
;;; 	tau-A-current(inactivation) - msec
(defun t-y-a (&optional voltage)
;Segal and Barker; Segal, Rogawski, and Barker
;Z & W Supposedly very rapid below -54mv(5ms) -~380 ms otherwise.
  (let* ((b (alpha voltage *v-half-ay *base-rate-ay *valence-ay *gamma-ay))
	(a (beta voltage *v-half-ay *base-rate-ay *valence-ay *gamma-ay))
	(ty (/ *qten-factor-at-30 (+ a b))))
      (if (< ty (* *qten-factor-at-30 *base-tya)) (* *qten-factor-at-30  *base-tya) ty)))


;;; G-A 
(defun g-a ( gbar-a x-a y-a)
  (* gbar-a x-a  x-a x-a y-a  ))

;;;X-A-EFF, Y-A-EFF
(defun x-a-eff (x-a)
  (expt x-a 3.0))
(defun y-a-eff (y-a)
  (expt y-a 1.0))


;;;    Fast Na-current ***********

;;; Original estimates for the parameters of the three conductances are derived from single Na only spike record
;;; (24 degrees C) and the Na only repetitive records (27 degrees C and 32 degrees C). All Qten's are derived
;;; from a reference of 24 degrees C. Gating particle kinetics have a Qten of 5; conductance Qten's are set to
;;; 1.5.

(defvar *g-na1-dens 400.0)			;conductance density, pS/micron-sq
(defvar *g-na2-dens 10)			;conductance density, pS/micron-sq
(defvar *g-na3-dens 350.0)			;conductance density, pS/micron-sq
(defvar *g-nad-dens 200.0)			;dendrite conductance density, pS/micron-sq


(defvar *gbar-na1 .5794977)			;conductance mS
(defvar *gbar-na2 0.0144874435)			;conductance mS
(defvar *gbar-na3 .5070605)			;conductance mS



(defvars-w-value (*na-choose 3))

(defvars-w-value (*v-half-m1 -47.0) (*base-rate-m1 0.3)  (*valence-m1 20.0) (*gamma-m1 0.50)
		 (*v-half-h1 -54.0) (*base-rate-h1 0.003)  (*valence-h1 30.0) (*gamma-h1 0.2)

		 (*v-half-m2 -5.0) (*base-rate-m2 0.025)  (*valence-m2 8) (*gamma-m2 .95)
		 (*v-half-h2 -47) (*base-rate-h2 0.0016667)  (*valence-h2 6) (*gamma-h2 0.2)

		 (*v-half-m3 -34.0) (*base-rate-m3 0.6667)  (*valence-m3 6.0) (*gamma-m3 0.50)
		 (*v-half-h3 -42.50) (*base-rate-h3 0.0023333)  (*valence-h3 30.0) (*gamma-h3 0.17)

		 (*base-tm1 0.50)(*base-th1 2.0) (*base-tm2 5)(*base-th2 3.00) (*base-tm3 0.40)(*base-th3 3.0))

;
;
;;;; A-M-NA
;(defun a-m-na (flag voltage)
;  (cond ((and (= flag 1)(= *na-choose 3))	 (a-m-na1-hippo  voltage ))
;	((and (= flag 2)(= *na-choose 3))	 (a-m-na2-hippo  voltage ))
;	((and (= flag 3)(= *na-choose 3))	 (a-m-na3-hippo  voltage ))))
;
;;;; B-M-NA
;(defun b-m-na (flag voltage)
;  (cond ((and (= flag 1)(= *na-choose 3))	 (b-m-na1-hippo  voltage ))
;	((and (= flag 2)(= *na-choose 3))	 (b-m-na2-hippo  voltage ))
;	((and (= flag 3)(= *na-choose 3))	 (b-m-na3-hippo  voltage ))))
;
;;;; A-H-NA
;(defun a-h-na (flag voltage)
;  (cond ((and (= flag 1)(= *na-choose 3))	 (a-h-na1-hippo  voltage ))
;	((and (= flag 2)(= *na-choose 3))	 (a-h-na2-hippo  voltage ))
;	((and (= flag 3)(= *na-choose 3))	 (a-h-na3-hippo  voltage ))))
;
;;;; B-H-NA
;(defun b-h-na (flag voltage)
;  (cond ((and (= flag 1)(= *na-choose 3))	 (b-h-na1-hippo  voltage ))
;	((and (= flag 2)(= *na-choose 3))	 (b-h-na2-hippo  voltage ))
;	((and (= flag 3)(= *na-choose 3))	 (b-h-na3-hippo  voltage ))))
;
;;;; M-NA-INF
;(defun m-na-inf (flag)
;  (cond ((= 1 flag)	 (/ *a-m-na1  (+ *a-m-na1 *b-m-na1)))
;	((= 2 flag)	 (/ *a-m-na2  (+ *a-m-na2 *b-m-na2)))
;	((= 3 flag)	 (/ *a-m-na3  (+ *a-m-na3 *b-m-na3)))))
;
;;;; T-M-NA
;(defun t-m-na (flag)
;  (let ((tm
;	  (cond ((= 1 flag) (/   1.0    (+ *a-m-na1 *b-m-na1)))
;		((= 2 flag) (/   1.0    (+ *a-m-na2 *b-m-na2)))
;		((= 3 flag) (/   1.0    (+ *a-m-na3 *b-m-na3))))))
;    (* *qten-factor-at-24  (cond ((= 1 flag) (if (< tm *base-tm1) *base-tm1 tm))
;				 ((= 2 flag) (if (< tm *base-tm2) *base-tm2 tm))
;				 ((= 3 flag) (if (< tm *base-tm3) *base-tm3 tm))))))
;
;;;; H-NA-INF
;(defun h-na-inf (flag)
;  (cond ((= 1 flag) (/ *a-h-na1 (+ *a-h-na1 *b-h-na1)))
;	((= 2 flag) (/ *a-h-na2 (+ *a-h-na2 *b-h-na2)))
;	((= 3 flag) (/ *a-h-na3 (+ *a-h-na3 *b-h-na3)))))
;
;;;; T-H-NA
;(defun t-h-na (flag)
;  (let ((th
;	  (cond ((= 1 flag) (/   1.0  (+ *a-h-na1 *b-h-na1)))
;		((= 2 flag) (/   1.0  (+ *a-h-na2 *b-h-na2)))
;		((= 3 flag) (/   1.0  (+ *a-h-na3 *b-h-na3))))))
;    (* *qten-factor-at-24  (cond ((= 1 flag) (if (< th *base-th1) *base-th1 th))
;				 ((= 2 flag) (if (< th *base-th2) *base-th2 th))
;				 ((= 3 flag) (if (< th *base-th3) *base-th3 th))))))
;
;;;; GBAR-NA
;(defun gbar-na (flag area)				;total na-channel conductance (microS)
;  (* *qten-g-24 (cond ((= 1 flag) (* *gbar-na1-dens area 1.0e3))
;		      ((= 2 flag) (* *gbar-na2-dens area 1.0e3))
;		      ((= 3 flag) (* *gbar-na3-dens area 1.0e3)))))
;
;
;;;; GBAR-NAD
;(defun gbar-nad (area)				;total dendrite na-channel conductance (microS)
;  (* *gbar-nad-dens area 1.0e3))
;
;
;
;;;;   WHEN EDITING POWERS OF GATING PARTICLES, ALSO EDIT APPROPRIATE M-EFF AND H-EFF FUNCTIONS
;
;;;; NA1-CURRENT
;(defun na1-current (gbar-na m-na h-na v)  (* (g-na1 gbar-na m-na h-na) (- v *e-na)))
;
;;;; G-NA1
;(defun g-na1 (gbar-na m-na h-na)  (* gbar-na   m-na  h-na h-na))
;
;;;; M-EFF-NA1, H-EFF-NA1
;(defun m-eff-na1 (m-na)  (expt m-na 1.0))
;(defun h-eff-na1 (h-na)  (expt h-na 2.0))
;
;
;;;; NA2-CURRENT
;(defun na2-current (gbar-na m-na h-na v)  (* (g-na2 gbar-na m-na  h-na) (- v *e-na)))
;
;;;; G-NA2
;(defun g-na2 (gbar-na m-na h-na )  (* gbar-na   m-na  h-na))
;
;;;; M-EFF-NA2, H-EFF-NA2
;(defun m-eff-na2 (m-na)  (expt m-na 1.0))
;(defun h-eff-na2 (h-na)  (expt h-na  1.0))
;
;
;;;; NA3-CURRENT
;(defun na3-current (gbar-na m-na h-na v)  (* (g-na3 gbar-na m-na h-na) (- v *e-na)))
;
;;;; G-NA3
;(defun g-na3 (gbar-na m-na h-na)  (* gbar-na   m-na m-na h-na h-na h-na ))
;
;;;; M-EFF-NA3, H-EFF-NA3
;(defun m-eff-na3 (m-na)  (expt m-na 2.0))
;(defun h-eff-na3 (h-na)  (expt h-na 3.0))
;
;
;
;
;
;;;; A-M-NA1-HIPPO
;(defun a-m-na1-hippo (voltage)
;  (alpha voltage *v-half-m1 *base-rate-m1 *valence-m1 *gamma-m1))
;
;;;; B-M-NA1-HIPPO
;(defun b-m-na1-hippo (voltage)
;  (beta voltage *v-half-m1 *base-rate-m1 *valence-m1 *gamma-m1))
;
;;;; A-H-NA1-HIPPO
;(defun a-h-na1-hippo (voltage)
;  (beta voltage *v-half-h1 *base-rate-h1 *valence-h1 *gamma-h1))
;
;;;; B-H-NA1-HIPPO
;(defun b-h-na1-hippo (voltage)
;  (alpha voltage *v-half-h1 *base-rate-h1 *valence-h1 *gamma-h1))
;
;;;; A-M-NA2-HIPPO
;(defun a-m-na2-hippo (voltage)
;  (alpha voltage *v-half-m2 *base-rate-m2 *valence-m2 *gamma-m2))
;
;;;; B-M-NA2-HIPPO
;(defun b-m-na2-hippo (voltage)
;  (beta voltage *v-half-m2 *base-rate-m2 *valence-m2 *gamma-m2))
;
;;;; A-H-NA2-HIPPO
;(defun a-h-na2-hippo (voltage)
;  (beta voltage *v-half-h2 *base-rate-h2 *valence-h2 *gamma-h2))
;
;;;; B-H-NA2-HIPPO
;(defun b-h-na2-hippo (voltage)
;  (alpha voltage *v-half-h2 *base-rate-h2 *valence-h2 *gamma-h2))
;
;;;; A-M-NA3-HIPPO
;(defun a-m-na3-hippo (voltage)
;  (alpha voltage *v-half-m3 *base-rate-m3 *valence-m3 *gamma-m3))
;
;;;; B-M-NA3-HIPPO
;(defun b-m-na3-hippo (voltage)
;  (beta voltage *v-half-m3 *base-rate-m3 *valence-m3 *gamma-m3))
;
;;;; A-H-NA3-HIPPO
;(defun a-h-na3-hippo (voltage)
;  (beta voltage *v-half-h3 *base-rate-h3 *valence-h3 *gamma-h3))
;
;;;; B-H-NA3-HIPPO
;(defun b-h-na3-hippo (voltage)
;  (alpha voltage *v-half-h3 *base-rate-h3 *valence-h3 *gamma-h3))
;


;;;  SOMATIC AND DENDRITIC Ca-CURRENT ***********


(defvar *gbar-Ca-dens 50.0)			;conductance density, mS/(cm-squared)
(defvar *gbar-Cad-dens 20.0)			;dendrite conductance density, mS/(cm-squared)
(defvar *base-tsca 2.0)
(defvar *base-twca 5.0)
(defvars-w-value (*v-half-s -24.0) (*base-rate-s .10)  (*valence-s 4.0) (*gamma-s 0.5)
		 (*v-half-w -35.0) (*base-rate-w 0.001)  (*valence-w 12.0) (*gamma-w 0.2))

(defvar *gbar-ca .5237074 )
(defvar *g-ca-dens 0)

(defun modify-ca ()
  (setq *gbar-ca (gbar-ca (surf-area *soma-radius))))


;;; K1-S-CA
(defun k1-s-ca (voltage)
  (alpha voltage *v-half-s *base-rate-s *valence-s *gamma-s))

;;; K2-S-CA
(defun k2-s-ca (voltage)
  (beta voltage *v-half-s *base-rate-s *valence-s *gamma-s))

;;; A-W-CA
(defun a-w-ca (voltage)
  (beta voltage *v-half-w *base-rate-w *valence-w *gamma-w))

;;; B-W-CA
(defun b-w-ca (voltage)
  (alpha voltage *v-half-w *base-rate-w *valence-w *gamma-w))




;;; GBAR-CA
(defun gbar-Ca (area)				;total Ca-channel conductance (microS)
  (* *gbar-Ca-dens area 1.0e3))

;;; GBAR-CAD
(defun gbar-Cad (area)				;total dendrite Ca-channel conductance (microS)
  (* *gbar-Cad-dens area 1.0e3))

;;; S-CA-INF
(defun s-ca-inf (v)
    (/ (k1-s-ca v) (+ (k1-s-ca v)(k2-s-ca v))))

;;; T-S-CA
(defun t-s-ca (v)
  (let	 ((tau (/   1.0 (+ (k1-s-ca v)(k2-s-ca v)))))
    (* *qten-factor-at-32 (if (< tau *base-tsca) *base-tsca tau))))

;;; W-CA-INF
(defun w-ca-inf (v)
    (/ (a-w-ca v) (+ (a-w-ca v)(b-w-ca v))))

;;; T-W-CA
(defun t-w-ca (v)
  (let	((tau (/  1.0 (+ (a-w-ca v)(b-w-ca v)))))
    (* *qten-factor-at-32 (if (< tau *base-twca) *base-twca tau))))

;;; CA-CURRENT
(defun ca-current (gbar-ca s-ca w-ca v)
  (* (g-ca gbar-ca s-ca w-ca)
     (- v *e-ca)))

;;; G-CA
(defun g-ca (gbar-ca s-ca w-ca)
  (if (< w-ca 0.001) (setq w-ca 0.0))
  (* gbar-Ca  s-ca s-ca w-ca w-ca w-ca w-ca))

;;;S-CA-EFF, W-CA-EFF
(defun s-ca-eff (s-ca)
  (if (< s-ca 0.001) 0.0
      (expt s-ca 2.0)))
(defun w-ca-eff (w-ca)
  (if (< w-ca 0.001) 0.0
  (expt w-ca 4.0)))



;;;    Persistant Calcium Current
;;;
;;;  Ca,slow current as reported by Johnston, Hablitz, and Wilson. 
;;;
;;; From deriving the IV curves of JH&W, it is determined that this current is due to a non-inactivating inward
;;; current with a reversal potential around 0mV. Thus it is unclear as to what species are actually comprising this
;;; current.

;;; E-CAS Empirically-derived reversal potential for the so-called slow "Ca" current.
(defvar *e-cas 0)

;;; conductance in micro-siemans
(defvar *gbar-cas .080)

;;; T-X-CAS Time constant for activation - ranges between 50 and 100 ms.
(defun t-x-cas (voltage)
  (declare (ignore voltage))
  75.0)

;;; X-CAS-INF Steady state value for the activation variable.
(defun x-cas-inf (voltage)
  (let ((midpoint -30.0)(steepness 3.60)) 
    (/ 1.0 (+ 1.0 (exp (/ (- midpoint voltage) steepness))))))


;;; CAS-CURRENT
(defun cas-current (x-cas voltage)
  (* *gbar-cas x-cas (- voltage *e-cas)))


;;;  Persistant Slow Na current ****************
;;;
;;; As reported by French and Gage, 1985
;;;
;;; For cat neocortical cells, Staftstrom Schwindt Chubb and Crill (1985) report Inap
;;;    Activates within 2 to 4 mS, over the range measured (~-70 to ~-30mV)
;;;    Activates at about 3-4mV above rest.
;
(defvar *gbar-nap .01)				;Max. conductance [microS], as measured by French and Gage.
;
(defvars-w-value (*v-half-napx -49.0) (*alpha-base-rate-napx 0.04)
  (*beta-base-rate-napx 0.04) (*valence-napx 6.0) (*gamma-napx 0.30)(*base-txnap 1.0))

;
;;;; GBAR-NAP
(defvar *gbar-nap-dens)
(defun gbar-nap (area)				;total nap-channel conductance (microS)	
  (* *gbar-nap-dens area 1.0e3))		;(area is in sq-cm)
;
;;;; X-NAP-INF
(defun x-nap-inf (voltage)
    (let ((a (alpha voltage *v-half-napx *alpha-base-rate-napx *valence-napx *gamma-napx))	;
	  (b (beta voltage *v-half-napx *alpha-base-rate-napx *valence-napx *gamma-napx)))
      (/ a (+ a b))))

;;;; T-X-NAP
(defun t-x-nap (voltage)
    (let* ((a (alpha voltage *v-half-napx *alpha-base-rate-napx *valence-napx *gamma-napx))
	  (b (beta voltage *v-half-napx *alpha-base-rate-napx *valence-napx *gamma-napx))
	  (tx (/ 1.0 (+ a b))))
      (if (< tx *base-txnap) *base-txnap tx)))
;  (cond ((> voltage 0.0)
;	 (* *qten-factor-at-22 1.0))
;	((> voltage -24.0)
;	 (* *qten-factor-at-22 4.0))
;	(t
;	 (* *qten-factor-at-22 40.0)))	;approx. 18mS - F&G fig.1
;
;;;; NAP-CURRENT
(defun nap-current (gbar-nap x-nap v)
  (* gbar-nap x-nap  (- v *e-na)))


;;;; **********  Rate functions are from Traub et al.
;
;;;; A-M-NA-TRAUB
;(defun a-m-na-traub 	(voltage)
;  (if (= (- voltage -70.0) 13.0) (setq voltage (+ 13.01 -70.0)))
;  (/ (* .4 (- 13.0 (- voltage -70.0)))(- (exp (/ (- 13.0 (- voltage -70.0)) 5.0)) 1.0)))
;
;;;; A-M-NA-TRAUB-IS
;(defun a-m-na-traub-is 	(voltage)
;  (if (= (- voltage -70.0) 7.0) (setq voltage (+ 7.01 -70.0)))
;  (/ (* .4 (- 7.0 (- voltage -70.0)))(- (exp (/ (- 7.0 (- voltage -70.0)) 5.0)) 1.0)))
;
;;;; B-M-NA-TRAUB
;(defun b-m-na-traub 	(voltage)
;  (if (= (- voltage -70.0) 45.0) (setq voltage (+ 45.01 -70.0)))
;  (/ (* .4 (- (- voltage -70.0) 45.0))(- (exp (/ (- (- voltage -70.0) 45.0) 5.0)) 1.0)))
;
;;;; B-M-NA-TRAUB-IS
;(defun b-m-na-traub-is 	(voltage)
;  (if (= (- voltage -70.0) 40.0) (setq voltage (+ 40.01 -70.0)))
;  (/ (* .4 (- (- voltage -70.0) 40.0))(- (exp (/ (- (- voltage -70.0) 40.0) 5.0)) 1.0)))
;
;
;;;; A-H-NA-TRAUB
;;;;    Same for initial seg.
;(defun a-h-na-traub (voltage)
;  (if (= (- voltage -70.0) 7.5) (setq voltage (+ 7.51 -70.0)))
;  (* .28 (exp (/ (- 7.5 (- voltage -70.0)) 20.0))))
;
;;;; B-H-NA-TRAUB
;;;;    Same for initial seg.
;(defun b-h-na-traub (voltage)
;  (if (= (- voltage -70.0) 37.5) (setq voltage (+ 37.51 -70.0)))
;  (/ 4 (+ (exp (/ (- 37.5 (- voltage -70.0)) 10.0)) 1.0)))
;
;
;;;; ************* Chiu and Ritchie Na kinetics
;
;;;; M-NA-INF2
;(defun m-na-inf2 (voltage)
;  (let ((midpoint -57.0)(steepness -4.0))
;    (if (< voltage 0) (/ 1.0 (+ 1.0 (exp (/ (- voltage midpoint) steepness)))) 1.0)))
;
;;;; H-NA-INF2
;(defun h-na-inf2 (voltage)
;  (let ((midpoint -75.0)(steepness 4.5))
;    (if (< voltage 0) (/ 1.0 (+ 1.0 (exp (/ (- voltage midpoint) steepness)))) 0)))
;
;;;; A-M-NA2
;(defun a-m-na2 (voltage)
;  (/ (+ 10.1 (* voltage 0.029))
;      (+ 1.0 (exp (* -0.19 (+ voltage 49.0))))))
;
;;;; B-H-NA2
;(defun b-h-na2 (voltage)
;  (/ 1.25
;      (+ 1.0 (exp (* -0.1 (+ voltage 56.0))))))
;
;;;; T-H-NA2
;(defun t-h-na2 (voltage)
;  (/ (* 2.0 *qten-factor-at-14
;	 (- 1.0 (h-na-inf2 voltage)))
;      (b-h-na2 voltage)))
;
;;;; T-M-NA2
;(defun t-m-na2 (voltage)
;  (/ (* 2.0 *qten-factor-at-14
;	 (m-na-inf2 voltage))
;      (* (a-m-na2 voltage) (- 2.0 (m-na-inf2 voltage)))))
;
;
;
;
;
;
;
;;;; A-M-NAD
;(defvar *nad-m-shift)
;(defun a-m-nad (voltage)
;  (let ((v (+ voltage *e-na *nad-m-shift)))
;    (cond ((= v 13.0) 10.25)
;	  (T (/ (* 5.0 (- 13.0 v))(- (exp (/ (- 13.0 v) 2.0)) 1.0))))))	;;orignal steepness = 5.0
;
;;;; B-M-NAD
;
;(defun b-m-nad (voltage)
;  (let ((v (+ voltage *e-na *nad-m-shift)))
;    (cond ((= v 45.0) 1.95)
;	  (T (/ (* 1.0 (- v 45.0))(- (exp (/ (- v 45.0) 2.0)) 1.0))))))	;;orignal steepness = 5.0
;
;
;;;; A-H-NAD
;(defvar *nad-h-shift)
;(defun a-h-nad (voltage)
;  (let ((v (+ voltage *e-na *nad-h-shift)))
;    (cond ((= v 7.5) .016)
;	  (T   (* .28 (exp (/ (- 7.5 v) 6.0)))))))	;orignal steepness = 20.0
;
;;;; B-H-NAD
;(defun b-h-nad (voltage)
;  (let ((v (+ voltage *e-na *nad-h-shift)))
;    (cond ((= v 37.5) 3.99)
;	  (T (/ 4 (+ (exp (/ (- 37.5 v) 3.0)) 1.0))))))	;;orignal steepness = 10.0
;
;
;;;; **********  Rate functions are from Traub et al.
;
;;;; ************************** Activation
;;;; K1-S-CA
;;(defun k1-s-ca 	(v)
;;  (cond((= (- v *e-ca) 13.0) 2.0)	(T (/ (* .4 (- 13.0 (- v *e-ca)))(- (exp (/ (- 13.0 (- v *e-ca)) 5.0)) 1.0)))))
;
;;  (if (=  v (+ *e-ca 35.0))
;;      (setq v (+ *e-ca 35.01)))
;;    (/ (* .05 (- 35.0 (- v *e-ca)))
;;	(- (exp (/ (- 35.0 (- v *e-ca)) 10.0)) 1.0)))
;
;;;; K2-S-CA
;;(defun k2-s-ca 	(v)
;;  (cond ((= (- v *e-ca) 45.0) 2.0)	(T (/ (* .4 (- (- v *e-ca) 45.0))(- (exp (/ (- (- v *e-ca) 45.0) 5.0)) 1.0)))))
;
;;  (if (=  v (+ *e-ca 15.0))			;
;;      (setq v (+ *e-ca 15.01)))
;;    (/ (* .01 (- (- v *e-ca) 15.0))
;;	(- (exp (/ (- (- v *e-ca) 15.0) 10.0)) 1.0)))
;
;;;; ************************** Inactivation
;;;; A-W-CA
;;(defun a-w-ca (v)
;;  (cond ((= (- v *e-ca) -10.0) .28) (T   (* .28 (exp (/ (- -10.0 (- v *e-ca)) 10.0))))))
; 
;; (* .014 (exp (/ (- -25.0 (- v *e-ca)) 20.0))))
;
;;;; B-W-CA
;;(defun b-w-ca (v)
;;  (cond ((= (- v *e-ca) 7.5) 2.0)	(T (/ 4 (+ (exp (/ (- 7.5 (- v *e-ca)) 5.0)) 1.0)))))
;
;;  (if (=  v (+ *e-ca 45.0))
;;      (setq v (+ *e-ca 45.01)))
;;    (/ 0.2 (+ (exp (/ (- 45.0 (- v *e-ca)) 10.0)) 1.0)))




