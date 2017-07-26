;;; -*- Mode: Lisp; Syntax: Common-lisp; Package: #-PARALLEL SURF #+PARALLEL *SURF; Base: 10; -*-
;;;  (c) Copyright 1990, Lyle Borg-Graham, MIT Center for Biological Information Processing



;;; MENU-FOR-SOMA-GEOMETRY-AND-PASSIVE-COMPONENTS
;(defun menu-for-soma-geometry-and-passive-components ()
;  (tv:choose-variable-values
;    '((*soma-radius      "Soma sphere radius [micrometers] " :number)
;      (*e-l   "Leakage battery [mV]   " :number)
;      (*e-na  "Na reversal potential [mV]   " :number)
;      (*e-k   "K reversal potential [mV]   " :number)
;      (*e-ca   "Ca reversal potential [mV]   " :number)
;      (*c-calc "Calculate C-mem from geometry (yes) or use input capacitance (no)" :boolean)
;      (*caps-mem   "Membrane capacitance [microfarads/sq-cm]   " :number)
;      (*caps-in "Input capacity [nF] " :number)
;      (*r-calc "Calculate *RS-MEM from geometry (yes) or use input impedance (no)" :boolean)
;      (*rs-mem  "Membrane resistance [ohm-cm-cm] " :number)
;      (*a-l      "Input impedance [MOhm] (used to substitute for soma and dendrite Rin only) " :number)
;      (*temperature "Temperature of experiment [Celsius]" :number)
;      (*qten  "Q-10 [Rate constant coefficient per 10 degrees]" :number)
;      (*qten-ionic  "Q-10 [Ionic conductance coefficient per 10 degrees]" :number)
;      (*include-shunt "Include electrode shunt conductance (if no the g-shunt will be ignored)?" :boolean)
;      (*r-electrode "Electrode shunt [Mohms]" :number)
;      (*i-constant-injection "Constant current injected [nA]" :number))
;    :label    "Passive components")
;  (setq *soma-area (* 1.0e8 (surf-area *soma-radius))
;	*gs-l (if *r-calc (/ (surf-area *soma-radius) (* *rs-mem 1.0e-6))
;		  (/ 1.0 *a-l ))
;	*g-electrode (/ 1.0 *r-electrode)
;	*caps (if *c-calc (* (surf-area *soma-radius) *caps-mem 1.0e3) *caps-in))
;  (update-qtens)
;)

;;; MENU-FOR-C-CURRENT
(defvar *c-shift 0.0)
(defun menu-for-c-current ()
  (tv:choose-variable-values
    '((*gbar-c "C-current conductance [micro-S]" :number)
      ""
      " X Variable Kinetics "
      ""
      (*v-half-cx "V/12 for x" :number)
      (*alpha-base-rate-cx "Alpha-base value for x at V1/2" :number
			   :documentation "Increase makes it faster")
      (*valence-cx "Valence for x" :number)
      (*gamma-cx "Gamma for x" :number)
      (*base-tcx "Minimum value for time constant [ms]" :number)
      ""
      " Y Variable Kinetics "
      ""
      (*v-half-cy "V/12 for y" :number)
      (*alpha-base-rate-cy "Alpha-base value for y at V1/2" :number
			   			   :documentation "Increase makes it faster")
      (*Valence-cy "Valence for y" :number)
      (*gamma-cy "Gamma for Na 1 y" :number)
      (*base-tcy "Minimum value for time constant [ms]" :number)
      ""
      
      ""
      ""
      " W Variable Kinetics "
      ""
      (*tau-alpha-c "Forward time constant for Ca++-binding to W particle" :number)
      (*tau-beta-c "Backward time constant for Ca++-binding to W particle" :number)
      ))
  (modify-c))

;;; MENU-FOR-M-CURRENT
(defun menu-for-m-current ()
  (tv:choose-variable-values
    '((*gbar-m "M-current absolute conductance [micro-S]" :number)
      (*m-block "Block some fraction of absolute conductance [0-1]" :number)
      ""
            " ** X Variable Kinetics ** "
      ""
      (*v-half-mx "V/12 for M x" :number)
      (*base-rate-mx "Alpha-base value for M x at V1/2" :number)
      (*valence-mx "Valence for M x" :number)
      (*gamma-mx "Gamma for M x" :number)
      (*base-tmx "Minimum value for time constant [ms]" :number)
      "")
      :label "M Potassium Current"
      ))

;;; MENU-FOR-Q-CURRENT
(defun menu-for-q-current ()
  (tv:choose-variable-values
    '((*gbar-q "Q-current conductance [micro-S]" :number)
      (*e-q "Q current reversal potential [mV]" :number))))

;;; MENU-FOR-DR-CURRENT
(defun menu-for-dr-current ()
  (tv:choose-variable-values
    '((*gbar-dr "DR-current absolute conductance [micro-S]" :number)
      (*dr-block "Block some fraction of absolute conductance [0-1]" :number)
      ""
            " ** X Variable Kinetics ** "
      ""
      (*v-half-drx "V/12 for Dr x" :number)
      (*base-rate-drx "Alpha-base value for Dr x at V1/2" :number)
      (*valence-drx "Valence for Dr x" :number)
      (*gamma-drx "Gamma for Dr x" :number)
      (*base-txdr "Minimum value for time constant [ms]" :number)
      ""
      " ** Y Variable Kinetics **"
      ""
      (*v-half-dry "V/12 for Dr y" :number)
      (*base-rate-dry "Alpha-base value for Dr y at V1/2" :number)
      (*valence-dry "Valence for Dr y" :number)
      (*gamma-dry "Gamma for Dr y" :number)
      (*base-tydr "Minimum value for time constant [ms]" :number)
      "")
      :label "Delayed-Rectifier Potassium Current"
      ))


;;; MENU-FOR-AHP-CURRENT
(defun menu-for-ahp-current ()
  (tv:choose-variable-values
    '((*gbar-ahp "AHP-current conductance [micro-S]" :number)
      ""
      " Z Variable Kinetics "
      ""
      (*v-half-ahpz "V/12 for Na 1 m" :number)
      (*alpha-base-rate-ahpz "Alpha-base value for Na 1 m at V1/2" :number
			     :documentation "Increase speeds up gating particle")
      (*valence-ahpz "Valence for Na 1 m" :number)
      (*gamma-ahpz "Gamma for Na 1 m" :number)
      (*base-tahpz "Minimum value of time constant [msec]" :number)
      ""
      ""
      " Y Variable Kinetics "
      ""
      (*v-half-ahpy "V/12 for Na 1 h" :number)
      (*alpha-base-rate-ahpy "Alpha-base value for Na 1 h at V1/2" :number
			     :documentation "Increase speeds up gating particle")
      (*valence-ahpy "Valence for Na 1 h" :number)
      (*gamma-ahpy "Gamma for Na 1 h" :number)
      (*base-tahpy "Minimum value of time constant [msec]" :number)
      
      ""
      ""
      " W Variable Kinetics "
      ""
      (*tau-alpha-ahp "Forward time constant for Ca++-binding to W particle" :number)
      (*tau-beta-ahp "Backward time constant for Ca++-binding to W particle" :number)
      ))
  (modify-ahp))

;;; MENU-FOR-a-CURRENT
(defun menu-for-a-current ()
  (tv:choose-variable-values
    '((*gbar-a "a-current conductance [micro-S]" :number)
      ""
            " X Variable Kinetics "
      ""
      (*v-half-ax "V/12 for a x (sb=-30,zw=-45)" :number)
      (*base-rate-ax "Alpha-base value for a x at V1/2" :number)
      (*valence-ax "Valence for a x (sb=3.67,zw=8.5)" :number)
      (*gamma-ax "Gamma for a x" :number)
      (*base-txa "Minimum value for time constant [ms]" :number)
      ""
      " Y Variable Kinetics "
      ""
      (*v-half-ay "V/12 for a y (sb=-70,zw=-55)" :number)
      (*base-rate-ay "Alpha-base value for a y at V1/2" :number)
      (*valence-ay "Valence for a y (sb=4.28,zw=8)" :number)
      (*gamma-ay "Gamma for a y" :number)
      (*base-tya "Minimum value for time constant [ms]" :number)
      ""
      )))


;;; MENU-FOR-NA1-CURRENT
(defun menu-for-Na1-current ()
  (tv:choose-variable-values
    '(""
      "  ****************** NA Trig (1) CURRENT *****************"
      ""
      (*g-na1-dens "Na 1 current conductance density (std =350) [pS/sq-uM]" :number)
      ""
      " M Variable Kinetics "
      ""
      (*v-half-m1 "V/12 for Na 1 m" :number)
      (*base-rate-m1 "Alpha-base value for Na 1 m at V1/2" :number)
      (*valence-m1 "Valence for Na 1 m" :number)
      (*gamma-m1 "Gamma for Na 1 m" :number)
      (*base-tm1 "Minimum value of time constant [msec]" :number)
      ""
      " H Variable Kinetics "
      ""
      (*v-half-h1 "V/12 for Na 1 h" :number)
      (*base-rate-h1 "Alpha-base value for Na 1 h at V1/2" :number)
      (*valence-h1 "Valence for Na 1 h" :number)
      (*gamma-h1 "Gamma for Na 1 h" :number)
      (*base-th1 "Minimum value of time constant [msec]" :number)
     "")
    ':label "Standard-spike A; Na1=trigger, Na2=slow tail, Na3=rep."))

;;; MENU-FOR-NA2-CURRENT
(defun menu-for-Na2-current ()
  (tv:choose-variable-values
    '(""
      "  ******************* NA Tail (2) CURRENT *****************"
      ""
      (*g-na2-dens "Na 2 current conductance density (std =10) [pS/sq-uM]" :number)
      ""
      " M Variable Kinetics "
      ""
      (*v-half-m2 "V/12 for Na 2 m" :number)
      (*base-rate-m2 "Alpha-base value for Na 2 m at V1/2" :number)
      (*valence-m2 "Valence for Na 2 m" :number)
      (*gamma-m2 "Gamma for Na 2 m" :number)
      (*base-tm2 "Minimum value of time constant [msec]" :number)
      ""
      " H Variable Kinetics "
      ""
      (*v-half-h2 "V/12 for Na 2 h" :number)
      (*base-rate-h2 "Alpha-base value for Na 2 h at V1/2" :number)
      (*valence-h2 "Valence for Na 2 h" :number)
      (*gamma-h2 "Gamma for Na 2 h" :number)
      (*base-th2 "Minimum value of time constant [msec]" :number)
      "")
    ':label "Standard-spike A; Na1=trigger, Na2=slow tail, Na3=rep."))

;;; MENU-FOR-NA3-CURRENT
(defun menu-for-Na3-current ()
  (tv:choose-variable-values
    '(""
      "  ****************** NA Rep (3) CURRENT *****************"
      ""
      (*g-na3-dens "Na 3 current conductance density (std =350) [pS/sq-uM]" :number)
      ""
      " M Variable Kinetics "
      ""
      (*v-half-m3 "V/12 for Na 3 m" :number)
      (*base-rate-m3 "Alpha-base value for Na 3 m at V1/2" :number)
      (*valence-m3 "Valence for Na 3 m" :number)
      (*gamma-m3 "Gamma for Na 3 m" :number)
      (*base-tm3 "Minimum value of time constant [msec]" :number)
      ""
      " H Variable Kinetics "
      ""
      (*v-half-h3 "V/12 for Na 3 h" :number)
      (*base-rate-h3 "Alpha-base value for Na 3 h at V1/2" :number)
      (*valence-h3 "Valence for Na 3 h" :number)
      (*gamma-h3 "Gamma for Na 3 h" :number)
      (*base-th3 "Minimum value of time constant [msec]" :number))
    ':label "Standard-spike A; Na1=trigger, Na2=slow tail, Na3=rep."))


;;; MENU-FOR-CA-CURRENT
(defun menu-for-Ca-current ()
  (tv:choose-variable-values
    '((*gbar-Ca-dens "Ca current conductance density [mS/sq-cm]" :number)
      ""
      " S Variable Kinetics "
      ""
      (*v-half-s "V/12 for Ca  s" :number)
      (*base-rate-s "Alpha-base value for Ca  s at V1/2" :number)
      (*valence-s "Valence for Ca  s" :number)
      (*gamma-s "Gamma for Ca  s" :number)
      (*base-tsca "Minimum value of activation time constant [msec]" :number)
      ""
      " W Variable Kinetics "
      ""
      (*v-half-w "V/12 for Ca  w" :number)
      (*base-rate-w "Alpha-base value for Ca  w at V1/2" :number
		    :documentation "Increase makes gating particle faster")
      (*valence-w "Valence for Ca  w" :number)
      (*gamma-w "Gamma for Ca  w" :number)
      (*base-twca "Minimum value of inactivation time constant [msec]"
		  :number)))
    (modify-ca))



