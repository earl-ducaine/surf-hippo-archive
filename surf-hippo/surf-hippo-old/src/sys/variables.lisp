;;; -*- Mode: Lisp; Syntax: Common-lisp; Package: #-PARALLEL SURF #+PARALLEL *SURF; Base: 10; -*-
;;;
;;;               SURF VARIABLES FILE by LBG
;;;
;;;   Other variables are defined (primarily) in the HIPPO variables file, and the SURF declare file, among others.

;;;
;;;   This file holds variables that are only used by SURF options, e.g. not those that may be used by both HIPPO and 
;;;   SURF, such as those referring to currents.

;;;        **** Define all the global variables and arrays ****
;;;
;;; The naming convention for the variables is as follows - 
;;;  
;;;   variable-name     = local variable
;;;   *variable-name    = global variable

(in-package #+parallel '*surf #-parallel 'surf)


;;;***** From SURF alone *****

;;; CMU CL is very uptight about number types, especially with respect
;;; to structure slots. We are keeping everything in single precision,
;;; so we will define a single precision pi internal to the SURF package.
(defconstant pi-single (COERCE user::PI 'SINGLE-FLOAT))

(defvar *automatic-run nil "This run is completely under computer control")


(defvar *simulation-file-name* )
(defvar *save-simulation nil)


(defvar *output-stream t "Output for simulator info")
(defvar  *hard-copy-screen nil)
(defvar *modify-globals nil)
(defvar  *print-circuit t)
(defvar *modify-synapses t)
(defvar *include-synapses t)


(defvar *V-REV-EX 0.0)				;Reversal potential of excitatory synapse.
(defvar *V-REV-IN -100.0)			;Reversal potential of inhibitory synapse.

(defvar *real-time 0 "Time during simulation (msec)")


;;; Original values from original simulator.
(defvar *r-mem 20000.0 "Default value of membrane resistance (ohms cm^2)")
(defvar *r-a 200.0 "Default value of segment axial resistance (ohms cm)")
(defvar *cap-mem 1.0 "Default value of membrane capacitance (uF/cm^2)")

(defvar *r-mem-soma 1000.0 "Default value of soma membrane resistance (ohms cm^2)")

(defvar *r-ex-mem 100000.0)
(defvar *r-in-mem 100000.0)

(defvars-w-value (*g-bar-excitatory-1 1.0)(*g-bar-inhibitory-1 1)(*e-rev-excitatory-1 0.0)
		 (*e-rev-inhibitory-1 -70))


(defvar *g-ex-mem 1 "Conductance of an excitory synapse in S/cm^2")
(defvar *G-IN-MEM *G-EX-MEM "Inhibitory synaptic membrane conductance (ohm-cm-cm)^-1")


(defvar *soma-radius 17.5)			;micrometers 

;;;  ************** PASSIVE COMPONENTS

(defvar *temperature 27.0)
(defvar *e-na 50.0)				;mvolts
(defvar *e-ca 110.0)				;mvolts
(defvar *e-k  -85.0)				;mvolts
(defvar *e-holding -70.0)				;mvolts

(defvar *e-l -70.0)				;constant leakage battery (mV)
(defvar *ed-l -70.0)

(defvar *faraday 9.648e4)			;Coulombs/mole
(defvar *R 8.314)				;Gas constant -- (Volts*Coulombs)/(DegreesKelvin*mole)

(defvar *ca-conc-extra 1.8)			;Extra-cellular Ca++ concentration [mmol/liter]
						;Hille says 1.5 mM Ca out, <10e-7 mM in.
						;Segal and Barker, 1986 use 4.0 mM Ca out
						;Madison and Nicoll, 1982 use 2.5 mM Ca out
						;Blaxter et al, 1986 use ACSF with 3.25 mM Ca
						;Wong and Prince, 1981 use 2.0 mM Ca
;;; Electrode shunt resistance (Mohm)
(defvar *r-electrode 10000000.0)
;;;  Soma input resistance (Mohm)
(defvar *a-l 39.0)
;;; Soma membrane resistance (ohm-cm-cm)
(defvar *rs-mem 850.0)				

;;; Dendrite membrane resistance (ohm-cm-cm)
(defvar *rd-mem 40000.0)				

;;; Dendrite axoplasmic resistance (ohm-cm)
(defvar *rd-int 200.0)

;;; Dendrite membrane capacitance (microfarads/sq-cm)
(defvar *capd-mem 1.0)				

;;;  Input capacitance of soma (nf)
(defvar *caps-in 0.150)				
;;;  Soma membrane capacitance (microfarads/sq-cm)
(defvar *caps-mem 1.0)				
;;;  Total capacity of soma (nF)
(defvar *caps)				


(defvar *qten-ionic 1.5)


(defvar *synaptic-time-resolution 0.5
  "Number of ms per element in the synaptic waveform array")	;;;; Not currently used.

(defvar *distal-direction t)

(defvar *syn-start-time 0.0)
(defvar *syn-duration)
(defvar *f1-ex 1.0)
(defvar *f2-ex 10.0)
(defvar *f1-in 1.0)
(defvar *f2-in 1.0)
(defvar *syn-spatial-freq (/ (* 1 3.14) 500.0))
(defvar *syn-temporal-freq (/ (* 5 2 3.14) 1000.0))
(defvar *tau-alpha-ex 25)
(defvar *tau-alpha-in 50)

(defvar *bar-speed 1.0)

(defvar *synapse-weight-offset 1.0)

(defvar *synapse-weight-slope 1000.0)


(defvar *lambda-factor 1.0 "Modifies lambda (length const) in computing synapse-weight")

(defvar *cells* '() "List of names of cells")



(defvar *spines* NIL)				; A list of spine segments.


(defvars *dummy1 *dummy2 *dummy3 *dummy4)



(defvar *modify-drawing t)

(defvar *modify-cell t)


(defvar *draw-cells nil)

(defvar *modify-plot-parameters t)
(defvar *automatic-dendrite-voltage-plot-scaling t)
(defvar  *dendrite-voltage-plot-min -90)
(defvar *dendrite-voltage-plot-max -40)

(defvar *automatic-soma-voltage-plot-scaling t)
(defvar  *soma-voltage-plot-min -90)
(defvar *soma-voltage-plot-max -40)

(defvar  *distal-nodes* nil)

(defvar *include-sources nil)



(defvars *plot-list1 *label-list1 *plot-list2 *label-list2 *plot-list3 *label-list3 *plot-list4 *label-list4
	 *plot-list5 *label-list5 *plot-list6 *label-list6 *plot-list7 *label-list7)



(defvars-w-value (*plot-aa1 nil)(*plot-aa2 nil)(*plot-aa3 nil)(*plot-aa4 nil)(*plot-aa5 nil))

(defvars-w-value (*i-stim-1 0.0)(*i-stim-2 0.0)(*i-stim-3 0.0)(*i-stim-4 0.0)(*i-stim-5 0.0)
		 (*t-stim-1 0.0)(*t-stim-2 0.0)(*t-stim-3 0.0)(*t-stim-4 0.0)(*t-stim-5 0.0)
		 (*i-den-stim-1 0.0)(*i-den-stim-2 0.0)(*i-den-stim-3 0.0)(*i-den-stim-4 0.0)
		 (*i-den-stim-5 0.0)(*i-den-stim-6 0.0)(*i-den-stim-7 0.0)(*i-den-stim-8 0.0)
		 (*i-den-stim-9 0.0)(*i-den-stim-10 0.0)(*t-den-stim-1 0.0)(*t-den-stim-2 0.0)
		 (*t-den-stim-3 0.0)(*t-den-stim-4 0.0)(*t-den-stim-5 0.0)(*t-den-stim-6 0.0)
		 (*t-den-stim-7 0.0)(*t-den-stim-8 0.0)(*t-den-stim-9 0.0)(*t-den-stim-10 0.0)  
		 (*current-stimulus-segment 5) (*i-stim 0.0)(*i-den-stim 0.0)
		 (*time-step 0)(*duration 50)(*include-soma-current t)(*include-dendrite-current nil)
		 (*plot-dendrite t)(*calculate-steady-state t)(*first-run t)(*steady-state-run nil)
		 (*vclamp-command-flag 1)(*voltage-command* nil)(*iclamp-command-flag 1)(*current-command* nil)
		 (*stim-seg 4)(*syn-seg 4))
(defvar *qten 3.0)				;Temperature dependance of rate constants. The rate constants
						;are multiplied by *QTEN raised to (T-Tbase)/10, where T is the     
						;temperature of the simulation, and Tbase is the temperature of
						;the experiment that measured the rate constants.


(defvar *qten-m 5.0)				;as reported by Paul

(defvars-w-value (*qten-factor-at-25 1.0) (*qten-factor-at-24 1.0)
		 (*qten-factor-at-22 1.0) (*qten-factor-at-37 1.0)
		 (*qten-factor-at-14 1.0) (*qten-factor-at-25-m 1.0)
		 (*qten-g-24 1.0)(*qten-factor-at-27 1.0)
		 (*qten-g-30 1.0)(*qten-factor-at-30 1.0)
		 (*qten-g-32 1.0)(*qten-factor-at-32 1.0))


;;; Miscellaneous flags
(defvar *modify-stimulus t)


(defvars-w-value (*do-clamp t)			
  (*modify-soma-passive-components t)(*modify-soma-stimulus t)(*modify-soma-synapse nil)
		 (*modify-soma-currents t)(*modify-soma t)(*modify-dendrite t)
		 (*segments-all-the-same t)(*modify-overall-parameters t)(*include-soma-synapse nil)
		 (*specify-initial-node-voltages nil)
		 (*plot-voltages-solid t)(*overlay-simulations nil)
		  (*change-plotted-nodes t) (*change-plotted-currents t) (*change-plotted-conductances t)
		 		 (*plot-results t)		 (*plot-results-frame-2 t)
		 )

(defvar  *modify-cell-type nil)
(defvar *last-circuit nil)


;;; Flags for the currents
(defvars-w-value (*include-na1 T) (*include-na2 T) (*include-na3 T)
		 (*include-nap nil)(*include-a T)(*include-ahp nil)(*include-cas nil)
		 (*include-dr T)(*include-k nil)(*include-c nil)(*include-m nil)(*include-kinetics nil)
		 (*include-ca nil)(*include-q nil)(*include-shunt t))

(defvars-w-value (*na1-mod T) (*na2-mod T) (*na3-mod T)
		 (*nap-mod nil)(*a-mod T)(*ahp-mod nil)(*cas-mod nil)
		 (*dr-mod T)(*k-mod nil)(*c-mod nil)(*m-mod nil)
		 (*ca-mod nil)(*q-mod nil))

(defvars-w-value (*plot-step 1)(*point-index 1)(*plot-points 1000)
		 (*clamp-type "Current clamp"))


