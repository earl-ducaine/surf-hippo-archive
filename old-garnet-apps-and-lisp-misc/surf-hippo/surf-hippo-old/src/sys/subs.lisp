;;; -*- Mode: LISP; Syntax: Common-lisp; Base: 10; Syntax: Common-lisp; package: #-PARALLEL SURF #+PARALLEL *surf -*-
;;; (c) Copyright 1985, Don Webber, Thinking Machines Corporation, Inc.
;;; alterations (c) Copyright 1990, Lyle Borg-Graham, MIT Center for Biological Information Processing


#-parallel
(in-package "SURF-HIPPO"
	    :use '("COMMON-LISP-USER" "COMMON-LISP")
	    :nicknames '("SURF"))
#+parallel 
(in-package "*SURF")


;;; compiler substitute symbols for serial version
(defvar core-capacitor-capacitance 'core-capacitor-capacitance)

(defvar core-capacitor-node-1-pointp 'core-capacitor-node-1-pointp)
(defvar core-capacitor-node-1-point 'core-capacitor-node-1-point)
(defvar core-capacitor-node-1-const 'core-capacitor-node-1-const)
(defvar core-capacitor-node-1-voltage 'core-capacitor-node-1-voltage)

(defvar core-capacitor-charge 'core-capacitor-charge)

(defvar core-capacitor-node-2-pointp 'core-capacitor-node-2-pointp)
(defvar core-capacitor-node-2-point 'core-capacitor-node-2-point)
(defvar core-capacitor-node-2-const 'core-capacitor-node-2-const)
(defvar core-capacitor-node-2-voltage 'core-capacitor-node-2-voltage)

(defvar core-capacitor-mat-12-valid 'core-capacitor-mat-12-valid)
(defvar core-capacitor-mat-12-point 'core-capacitor-mat-12-point)
(defvar core-capacitor-mat-12-value 'core-capacitor-mat-12-value)
(defvar core-capacitor-mat-21-valid 'core-capacitor-mat-21-valid)
(defvar core-capacitor-mat-21-point 'core-capacitor-mat-21-point)
(defvar core-capacitor-mat-21-value 'core-capacitor-mat-21-value)


;;; compiler substitute symbols for parallel version
#+parallel
(setq core-capacitor-capacitance 'device-little-float-1
      core-capacitor-node-1-pointp 'core-device-node1-valid
      core-capacitor-node-1-point 'core-device-node1-point
      core-capacitor-node-1-const 'core-device-node1-const
      core-capacitor-node-1-voltage 'core-device-node1-voltage

      core-capacitor-charge 'core-device-node1-charge
      core-capacitor-node-2-pointp 'core-device-node2-valid
      core-capacitor-node-2-point 'core-device-node2-point
      core-capacitor-node-2-const 'core-device-node2-const
      core-capacitor-node-2-voltage 'core-device-node2-voltage

      core-capacitor-mat-12-valid 'core-device-mat-12-valid
      core-capacitor-mat-12-point 'core-device-mat-12-point
      core-capacitor-mat-12-value 'core-device-mat-12-value
      core-capacitor-mat-21-valid 'core-device-mat-21-valid
      core-capacitor-mat-21-point 'core-device-mat-21-point
      core-capacitor-mat-21-value 'core-device-mat-21-value)


;;; compiler substitute symbols for serial version
(defvar core-mos1-w 'core-mos1-w)
(defvar core-mos1-l 'core-mos1-l)
(defvar core-mos1-fixed-vto 'core-mos1-fixed-vto)
(defvar core-mos1-vfb 'core-mos1-vfb)
(defvar core-mos1-phi 'core-mos1-phi)
(defvar core-mos1-beta 'core-mos1-beta)
(defvar core-mos1-lambda 'core-mos1-lambda)

(defvar core-mos1-node-d-pointp 'core-mos1-node-d-pointp) 
(defvar core-mos1-node-d-point 'core-mos1-node-d-point)

(defvar core-mos1-node-d-const 'core-mos1-node-d-const)
(defvar core-mos1-node-g-const 'core-mos1-node-g-const)
(defvar core-mos1-node-s-const 'core-mos1-node-s-const)
(defvar core-mos1-node-b-const 'core-mos1-node-b-const)
(defvar core-mos1-node-d-voltage 'core-mos1-node-d-voltage)
(defvar core-mos1-node-g-voltage 'core-mos1-node-g-voltage)
(defvar core-mos1-node-s-voltage 'core-mos1-node-s-voltage)
(defvar core-mos1-node-b-voltage 'core-mos1-node-b-voltage)

(defvar core-mos1-charge-d 'core-mos1-charge-d)
(defvar core-mos1-ids 'core-mos1-ids)
(defvar core-mos1-cond-d 'core-mos1-cond-d)

(defvar core-mos1-node-g-pointp 'core-mos1-node-g-pointp)
(defvar core-mos1-node-g-point 'core-mos1-node-g-point)
(defvar core-mos1-charge-g 'core-mos1-charge-g)
(defvar core-mos1-cond-g 'core-mos1-cond-g)

(defvar core-mos1-node-s-pointp 'core-mos1-node-s-pointp)
(defvar core-mos1-node-s-point 'core-mos1-node-s-point)
(defvar core-mos1-charge-s 'core-mos1-charge-s)
(defvar core-mos1-cond-s 'core-mos1-cond-s)

(defvar core-mos1-node-b-pointp 'core-mos1-node-b-pointp)
(defvar core-mos1-node-b-point 'core-mos1-node-b-point)
(defvar core-mos1-charge-b 'core-mos1-charge-b)
(defvar core-mos1-cond-b 'core-mos1-cond-b)

(defvar core-mos1-mat-dg-valid 'core-mos1-mat-dg-valid)
(defvar core-mos1-mat-dg-point 'core-mos1-mat-dg-point)
(defvar core-mos1-mat-dg-value 'core-mos1-mat-dg-value)
(defvar core-mos1-mat-ds-valid 'core-mos1-mat-ds-valid)
(defvar core-mos1-mat-ds-point 'core-mos1-mat-ds-point)
(defvar core-mos1-mat-ds-value 'core-mos1-mat-ds-value)
(defvar core-mos1-mat-db-valid 'core-mos1-mat-db-valid)
(defvar core-mos1-mat-db-point 'core-mos1-mat-db-point)
(defvar core-mos1-mat-db-value 'core-mos1-mat-db-value)

(defvar core-mos1-mat-gd-valid 'core-mos1-mat-gd-valid)
(defvar core-mos1-mat-gd-point 'core-mos1-mat-gd-point)
(defvar core-mos1-mat-gd-value 'core-mos1-mat-gd-value)
(defvar core-mos1-mat-gs-valid 'core-mos1-mat-gs-valid)
(defvar core-mos1-mat-gs-point 'core-mos1-mat-gs-point)
(defvar core-mos1-mat-gs-value 'core-mos1-mat-gs-value)
(defvar core-mos1-mat-gb-valid 'core-mos1-mat-gb-valid)
(defvar core-mos1-mat-gb-point 'core-mos1-mat-gb-point)
(defvar core-mos1-mat-gb-value 'core-mos1-mat-gb-value)

(defvar core-mos1-mat-sd-valid 'core-mos1-mat-sd-valid)
(defvar core-mos1-mat-sd-point 'core-mos1-mat-sd-point)
(defvar core-mos1-mat-sd-value 'core-mos1-mat-sd-value)
(defvar core-mos1-mat-sg-valid 'core-mos1-mat-sg-valid)
(defvar core-mos1-mat-sg-point 'core-mos1-mat-sg-point)
(defvar core-mos1-mat-sg-value 'core-mos1-mat-sg-value)
(defvar core-mos1-mat-sb-valid 'core-mos1-mat-sb-valid)
(defvar core-mos1-mat-sb-point 'core-mos1-mat-sb-point)
(defvar core-mos1-mat-sb-value 'core-mos1-mat-sb-value)

(defvar core-mos1-mat-bd-valid 'core-mos1-mat-bd-valid)
(defvar core-mos1-mat-bd-point 'core-mos1-mat-bd-point)
(defvar core-mos1-mat-bd-value 'core-mos1-mat-bd-value)
(defvar core-mos1-mat-bg-valid 'core-mos1-mat-bg-valid)
(defvar core-mos1-mat-bg-point 'core-mos1-mat-bg-point)
(defvar core-mos1-mat-bg-value 'core-mos1-mat-bg-value)
(defvar core-mos1-mat-bs-valid 'core-mos1-mat-bs-valid)
(defvar core-mos1-mat-bs-point 'core-mos1-mat-bs-point)
(defvar core-mos1-mat-bs-value 'core-mos1-mat-bs-value)

;;; compiler substitute symbols for parallel version
#+parallel
(setq core-mos1-w 'device-little-float-1
      core-mos1-l 'device-little-float-2
      core-mos1-fixed-vto 'device-little-float-3
      core-mos1-vfb 'device-little-float-4
      core-mos1-phi 'device-little-float-5
      core-mos1-beta 'device-little-float-6
      core-mos1-lambda 'device-little-float-7

      core-mos1-node-d-pointp 'core-device-node1-valid
      core-mos1-node-d-point 'core-device-node1-point
      core-mos1-charge-d 'core-device-node1-charge
      core-mos1-ids 'core-device-node1-current
      core-mos1-cond-d 'core-device-node1-conductance
      
      core-mos1-node-g-pointp 'core-device-node2-valid
      core-mos1-node-g-point 'core-device-node2-point
      core-mos1-charge-g 'core-device-node2-charge
      core-mos1-cond-g 'core-device-node2-conductance
      
      core-mos1-node-s-pointp 'core-device-node3-valid
      core-mos1-node-s-point 'core-device-node3-point
      core-mos1-charge-s 'core-device-node3-charge
      core-mos1-cond-s 'core-device-node3-conductance
      
      core-mos1-node-b-pointp 'core-device-node4-valid
      core-mos1-node-b-point 'core-device-node4-point
      core-mos1-charge-b 'core-device-node4-charge
      core-mos1-cond-b 'core-device-node4-conductance

      core-mos1-node-d-const 'core-device-node1-const
      core-mos1-node-g-const 'core-device-node2-const
      core-mos1-node-s-const 'core-device-node3-const
      core-mos1-node-b-const 'core-device-node4-const

      core-mos1-node-d-voltage 'core-device-node1-voltage
      core-mos1-node-g-voltage 'core-device-node2-voltage
      core-mos1-node-s-voltage 'core-device-node3-voltage
      core-mos1-node-b-voltage 'core-device-node4-voltage
      
      core-mos1-mat-dg-valid 'core-device-mat-dg-valid
      core-mos1-mat-dg-point 'core-device-mat-dg-point
      core-mos1-mat-dg-value 'core-device-mat-dg-value
      core-mos1-mat-ds-valid 'core-device-mat-ds-valid
      core-mos1-mat-ds-point 'core-device-mat-ds-point
      core-mos1-mat-ds-value 'core-device-mat-ds-value
      core-mos1-mat-db-valid 'core-device-mat-db-valid
      core-mos1-mat-db-point 'core-device-mat-db-point
      core-mos1-mat-db-value 'core-device-mat-db-value

      core-mos1-mat-gd-valid 'core-device-mat-gd-valid
      core-mos1-mat-gd-point 'core-device-mat-gd-point
      core-mos1-mat-gd-value 'core-device-mat-gd-value
      core-mos1-mat-gs-valid 'core-device-mat-gs-valid
      core-mos1-mat-gs-point 'core-device-mat-gs-point
      core-mos1-mat-gs-value 'core-device-mat-gs-value
      core-mos1-mat-gb-valid 'core-device-mat-gb-valid
      core-mos1-mat-gb-point 'core-device-mat-gb-point
      core-mos1-mat-gb-value 'core-device-mat-gb-value

      core-mos1-mat-sd-valid 'core-device-mat-sd-valid
      core-mos1-mat-sd-point 'core-device-mat-sd-point
      core-mos1-mat-sd-value 'core-device-mat-sd-value
      core-mos1-mat-sg-valid 'core-device-mat-sg-valid
      core-mos1-mat-sg-point 'core-device-mat-sg-point
      core-mos1-mat-sg-value 'core-device-mat-sg-value
      core-mos1-mat-sb-valid 'core-device-mat-sb-valid
      core-mos1-mat-sb-point 'core-device-mat-sb-point
      core-mos1-mat-sb-value 'core-device-mat-sb-value

      core-mos1-mat-bd-valid 'core-device-mat-bd-valid
      core-mos1-mat-bd-point 'core-device-mat-bd-point
      core-mos1-mat-bd-value 'core-device-mat-bd-value
      core-mos1-mat-bs-valid 'core-device-mat-bs-valid
      core-mos1-mat-bs-point 'core-device-mat-bs-point
      core-mos1-mat-bs-value 'core-device-mat-bs-value
      core-mos1-mat-bg-valid 'core-device-mat-bg-valid
      core-mos1-mat-bg-point 'core-device-mat-bg-point
      core-mos1-mat-bg-value 'core-device-mat-bg-value)
 
(defvar core-resistor-node-1-pointp 'core-resistor-node-1-pointp)
(defvar core-resistor-node-2-pointp 'core-resistor-node-2-pointp)
(defvar core-resistor-conductance 'core-resistor-conductance)
(defvar core-resistor-node-1-point 'core-resistor-node-1-point)
(defvar core-resistor-node-2-point 'core-resistor-node-2-point)

(defvar core-resistor-current 'core-resistor-current)

(defvar core-resistor-node-1-const 'core-resistor-node-1-const)
(defvar core-resistor-node-2-const 'core-resistor-node-2-const)
(defvar core-resistor-node-1-voltage 'core-resistor-node-1-voltage)
(defvar core-resistor-node-2-voltage 'core-resistor-node-2-voltage)

(defvar core-resistor-mat-12-valid 'core-resistor-mat-12-valid)
(defvar core-resistor-mat-12-point 'core-resistor-mat-12-point)
(defvar core-resistor-mat-12-value 'core-resistor-mat-12-value)
(defvar core-resistor-mat-21-valid 'core-resistor-mat-21-valid)
(defvar core-resistor-mat-21-point 'core-resistor-mat-21-point)
(defvar core-resistor-mat-21-value 'core-resistor-mat-21-value)

#+parallel
(setf core-resistor-node-1-pointp 'core-device-node1-valid
      core-resistor-node-2-pointp 'core-device-node2-valid
      core-resistor-conductance 'core-device-node1-conductance
      core-resistor-node-1-point 'core-device-node1-point
      core-resistor-node-2-point 'core-device-node2-point

      core-resistor-current 'core-device-node1-current

      core-resistor-node-1-const 'core-device-node1-const
      core-resistor-node-2-const 'core-device-node2-const
      core-resistor-node-1-voltage 'core-device-node1-voltage
      core-resistor-node-2-voltage 'core-device-node2-voltage
      
      core-resistor-mat-12-valid 'core-device-mat-12-valid
      core-resistor-mat-12-point 'core-device-mat-12-point
      core-resistor-mat-12-value 'core-device-mat-12-value
      core-resistor-mat-21-valid 'core-device-mat-21-valid
      core-resistor-mat-21-point 'core-device-mat-21-point
      core-resistor-mat-21-value 'core-device-mat-21-value)



(defvar core-isource-node-1-pointp 'core-isource-node-1-pointp)
(defvar core-isource-node-2-pointp 'core-isource-node-2-pointp)
(defvar core-isource-node-1-point 'core-isource-node-1-point)
(defvar core-isource-node-2-point 'core-isource-node-2-point)
(defvar core-isource-current 'core-isource-current)

(defvar vsource-return-current 'vsource-return-current)

#+parallel
(setf core-isource-node-1-pointp 'core-device-node1-valid
	 core-isource-node-2-pointp 'core-device-node2-valid
	 core-isource-node-1-point 'core-device-node1-point
	 core-isource-node-2-point 'core-device-node2-point
	 core-isource-current 'core-device-node1-current)

(defvar core-soma-g-shunt 'core-soma-g-shunt)
(defvar core-soma-capacitance 'core-soma-capacitance)
(defvar core-soma-g-leak 'core-soma-g-leak)
(defvar core-soma-v-leak 'core-soma-v-leak)

(defvar core-soma-node-pointp 'core-soma-node-pointp)
(defvar core-soma-node-point 'core-soma-node-point)
(defvar core-soma-node-const 'core-soma-node-const)
(defvar core-soma-node-voltage 'core-soma-node-voltage)

(defvar core-soma-current 'core-soma-current)
(defvar core-soma-charge 'core-soma-charge)
(defvar core-soma-jacobian 'core-soma-jacobian)

#+parallel
(setf core-soma-g-shunt 'device-little-float-1
      core-soma-capacitance 'device-little-float-2
      core-soma-g-leak 'device-little-float-3
      core-soma-v-leak 'device-little-float-4

      core-soma-node-pointp 'core-device-node1-valid
      core-soma-node-point 'core-device-node1-point
      core-soma-node-const 'core-device-node1-const
      core-soma-node-voltage 'core-device-node1-voltage

      core-soma-current 'core-device-node1-current
      core-soma-charge 'core-device-node1-charge
      core-soma-jacobian 'core-device-node1-conductance
)


(defvar core-synapse-automatic 'core-synapse-automatic) 
(defvar core-synapse-function-list 'core-synapse-function-list) 
(defvar core-synapse-tau 'core-synapse-tau) 
(defvar core-synapse-tau-power 'core-synapse-tau-power)
(defvar core-synapse-start-time 'core-synapse-start-time)
(defvar core-synapse-gbar 'core-synapse-gbar)
(defvar core-synapse-v-reversal 'core-synapse-v-reversal)
(defvar core-synapse-ca-conc-extra 'core-synapse-ca-conc-extra)
(defvar core-synapse-Eca-nearst-eqn-const 'core-synapse-Eca-nearst-eqn-const)
(defvar core-synapse-node-pointp 'core-synapse-node-pointp)
(defvar core-synapse-node-point 'core-synapse-node-point)
(defvar core-synapse-node-const 'core-synapse-node-const)
(defvar core-synapse-node-voltage 'core-synapse-node-voltage)
(defvar core-synapse-ca-shell-pointp 'core-synapse-ca-shell-pointp)
(defvar core-synapse-ca-shell-point 'core-synapse-ca-shell-point)
(defvar core-synapse-ca-shell-value 'core-synapse-ca-shell-value)
(defvar core-synapse-ca-shell-const 'core-synapse-ca-shell-const)
(defvar core-synapse-pre-synaptic-pointp 'core-synapse-pre-synaptic-pointp)
(defvar core-synapse-pre-synaptic-point 'core-synapse-pre-synaptic-point)
(defvar core-synapse-pre-synaptic-value 'core-synapse-pre-synaptic-value)
(defvar core-synapse-pre-synaptic-const 'core-synapse-pre-synaptic-const)

(defvar core-synapse-current 'core-synapse-current)
(defvar core-synapse-conductance 'core-synapse-conductance)

#+parallel
(setf core-synapse-gbar 'device-little-float-1
      core-synapse-v-reversal 'device-little-float-2
      core-synapse-ca-conc-extra 'device-little-float-3
      core-synapse-Eca-nearst-eqn-const 'device-little-float-4

      core-synapse-node-pointp 'core-device-node1-valid
      core-synapse-node-point 'core-device-node1-point
      core-synapse-node-const 'core-device-node1-const
      core-synapse-node-voltage 'core-device-node1-voltage
      core-synapse-ca-shell-pointp 'core-device-node5-valid
      core-synapse-ca-shell-point 'core-device-node5-point
      core-synapse-ca-shell-value 'core-device-node5-voltage
      core-synapse-ca-shell-const 'core-device-node5-const
      core-synapse-pre-synaptic-pointp 'core-device-node6-valid
      core-synapse-pre-synaptic-point 'core-device-node6-point
      core-synapse-pre-synaptic-value 'core-device-node6-voltage
      core-synapse-pre-synaptic-const 'core-device-node6-const
      core-synapse-current 'core-device-node1-current
      core-synapse-conductance 'core-device-node1-conductance
)


(defvar core-channel-gbar 'core-channel-gbar)
(defvar core-channel-v-reversal 'core-channel-v-reversal)
(defvar core-channel-ca-conc-extra 'core-channel-ca-conc-extra)
(defvar core-channel-Eca-nearst-eqn-const 'core-channel-Eca-nearst-eqn-const)

(defvar core-channel-node-pointp 'core-channel-node-pointp)
(defvar core-channel-node-point 'core-channel-node-point)
(defvar core-channel-node-const 'core-channel-node-const)
(defvar core-channel-node-voltage 'core-channel-node-voltage)

(defvar core-channel-a-pointp 'core-channel-a-pointp)
(defvar core-channel-a-point 'core-channel-a-point)
(defvar core-channel-a-value 'core-channel-a-value)
(defvar core-channel-a-const 'core-channel-a-const)
(defvar core-channel-i-pointp 'core-channel-i-pointp)
(defvar core-channel-i-point 'core-channel-i-point)
(defvar core-channel-i-value 'core-channel-i-value)
(defvar core-channel-i-const 'core-channel-i-const)
(defvar core-channel-c-pointp 'core-channel-c-pointp)
(defvar core-channel-c-point 'core-channel-c-point)
(defvar core-channel-c-value 'core-channel-c-value)
(defvar core-channel-c-const 'core-channel-c-const)
(defvar core-channel-ca-shell-pointp 'core-channel-ca-shell-pointp)
(defvar core-channel-ca-shell-point 'core-channel-ca-shell-point)
(defvar core-channel-ca-shell-value 'core-channel-ca-shell-value)
(defvar core-channel-ca-shell-const 'core-channel-ca-shell-const)
(defvar core-channel-pre-synaptic-pointp 'core-channel-pre-synaptic-pointp)
(defvar core-channel-pre-synaptic-point 'core-channel-pre-synaptic-point)
(defvar core-channel-pre-synaptic-value 'core-channel-pre-synaptic-value)
(defvar core-channel-pre-synaptic-const 'core-channel-pre-synaptic-const)

(defvar core-channel-current 'core-channel-current)
(defvar core-channel-conductance 'core-channel-conductance)

#+parallel
(setf core-channel-gbar 'device-little-float-1
      core-channel-v-reversal 'device-little-float-2
      core-channel-ca-conc-extra 'device-little-float-3
      core-channel-Eca-nearst-eqn-const 'device-little-float-4

      core-channel-node-pointp 'core-device-node1-valid
      core-channel-node-point 'core-device-node1-point
      core-channel-node-const 'core-device-node1-const
      core-channel-node-voltage 'core-device-node1-voltage

      core-channel-a-pointp 'core-device-node2-valid
      core-channel-a-point 'core-device-node2-point
      core-channel-a-value 'core-device-node2-voltage
      core-channel-a-const 'core-device-node2-const
      core-channel-i-pointp 'core-device-node3-valid
      core-channel-i-point 'core-device-node3-point
      core-channel-i-value 'core-device-node3-voltage
      core-channel-i-const 'core-device-node3-const
      core-channel-c-pointp 'core-device-node4-valid
      core-channel-c-point 'core-device-node4-point
      core-channel-c-value 'core-device-node4-voltage
      core-channel-c-const 'core-device-node4-const
      core-channel-ca-shell-pointp 'core-device-node5-valid
      core-channel-ca-shell-point 'core-device-node5-point
      core-channel-ca-shell-value 'core-device-node5-voltage
      core-channel-ca-shell-const 'core-device-node5-const
      core-channel-pre-synaptic-pointp 'core-device-node6-valid
      core-channel-pre-synaptic-point 'core-device-node6-point
      core-channel-pre-synaptic-value 'core-device-node6-voltage
      core-channel-pre-synaptic-const 'core-device-node6-const

      core-channel-current 'core-device-node1-current
      core-channel-conductance 'core-device-node1-conductance
)

(defvar core-particle-z 'core-particle-z)
(defvar core-particle-gamma 'core-particle-gamma)
(defvar core-particle-alpha-0 'core-particle-alpha-0)
(defvar core-particle-v-half 'core-particle-v-half)
(defvar core-particle-tau-0 'core-particle-tau-0)
(defvar core-particle-Q10 'core-particle-Q10)

(defvar core-particle-node-pointp 'core-particle-node-pointp)
(defvar core-particle-node-point 'core-particle-node-point)
(defvar core-particle-node-const 'core-particle-node-const)
(defvar core-particle-node-voltage 'core-particle-node-voltage)

(defvar core-particle-vnode-point 'core-particle-vnode-point)
(defvar core-particle-vnode-pointp 'core-particle-vnode-pointp)
(defvar core-particle-vnode-value 'core-particle-vnode-value)
(defvar core-particle-vnode-const 'core-particle-vnode-const)

(defvar core-particle-current 'core-particle-current)
(defvar core-particle-charge 'core-particle-charge)
(defvar core-particle-conductance 'core-particle-conductance)

#+parallel
(setf core-particle-z 'device-little-float-1
      core-particle-gamma 'device-little-float-2
      core-particle-alpha-0 'device-little-float-3
      core-particle-v-half 'device-little-float-4
      core-particle-tau-0 'device-little-float-5
      core-particle-Q10 'device-little-float-6

      core-particle-node-pointp 'core-device-node1-valid
      core-particle-node-point 'core-device-node1-point
      core-particle-node-const 'core-device-node1-const
      core-particle-node-voltage 'core-device-node1-voltage

      core-particle-vnode-point 'core-device-node2-point
      core-particle-vnode-pointp 'core-device-node2-valid
      core-particle-vnode-value 'core-device-node2-voltage
      core-particle-vnode-const 'core-device-node2-const

      core-particle-current 'core-device-node1-current
      core-particle-charge 'core-device-node1-charge
      core-particle-conductance 'core-device-node1-conductance
)

(defvar core-conc-part-alpha 'core-conc-part-alpha)
(defvar core-conc-part-beta 'core-conc-part-beta)
(defvar core-conc-part-Q10 'core-conc-part-Q10)

(defvar core-conc-part-node-pointp 'core-conc-part-node-pointp)
(defvar core-conc-part-node-point 'core-conc-part-node-point)
(defvar core-conc-part-node-const 'core-conc-part-node-const)
(defvar core-conc-part-node-voltage 'core-conc-part-node-voltage)

(defvar core-conc-part-cnode-pointp 'core-conc-part-cnode-pointp)
(defvar core-conc-part-cnode-point 'core-conc-part-cnode-point)
(defvar core-conc-part-cnode-value 'core-conc-part-cnode-value)
(defvar core-conc-part-cnode-const 'core-conc-part-cnode-const)

(defvar core-conc-part-current 'core-conc-part-current)
(defvar core-conc-part-charge 'core-conc-part-charge)
(defvar core-conc-part-conductance 'core-conc-part-conductance)

#+parallel
(setf core-conc-part-alpha 'device-little-float-1
      core-conc-part-beta 'device-little-float-2
      core-conc-part-Q10 'device-little-float-3

      core-conc-part-node-pointp 'core-device-node1-valid
      core-conc-part-node-point 'core-device-node1-point
      core-conc-part-node-const 'core-device-node1-const
      core-conc-part-node-voltage 'core-device-node1-voltage

      core-conc-part-cnode-pointp 'core-device-node2-valid
      core-conc-part-cnode-point 'core-device-node2-point
      core-conc-part-cnode-value 'core-device-node2-voltage
      core-conc-part-cnode-const 'core-device-node2-const

      core-conc-part-current 'core-device-node1-current
      core-conc-part-charge 'core-device-node1-charge
      core-conc-part-conductance 'core-device-node1-conductance
)

;;; compiler substitute symbols for serial version
(defvar core-conc-int-D-11 'core-conc-int-D-11)
(defvar core-conc-int-D-12 'core-conc-int-D-12)
(defvar core-conc-int-D-21 'core-conc-int-D-21)
(defvar core-conc-int-D-22 'core-conc-int-D-22)
(defvar core-conc-int-conc-core 'core-conc-int-conc-core)
;(defvar core-conc-int-B 'core-conc-int-B)
(defvar core-conc-int-k 'core-conc-int-k)

(defvar core-conc-int-node-1-pointp 'core-conc-int-node-1-pointp)
(defvar core-conc-int-node-1-point 'core-conc-int-node-1-point)
(defvar core-conc-int-node-1-const 'core-conc-int-node-1-const)
(defvar core-conc-int-node-1-voltage 'core-conc-int-node-1-voltage)

(defvar core-conc-int-current-1 'core-conc-int-current-1)
(defvar core-conc-int-current-2 'core-conc-int-current-2)

(defvar core-conc-int-node-2-pointp 'core-conc-int-node-2-pointp)
(defvar core-conc-int-node-2-point 'core-conc-int-node-2-point)
(defvar core-conc-int-node-2-const 'core-conc-int-node-2-const)
(defvar core-conc-int-node-2-voltage 'core-conc-int-node-2-voltage)

(defvar core-conc-int-mat-12-valid 'core-conc-int-mat-12-valid)
(defvar core-conc-int-mat-12-point 'core-conc-int-mat-12-point)
(defvar core-conc-int-mat-12-value 'core-conc-int-mat-12-value)
(defvar core-conc-int-mat-21-valid 'core-conc-int-mat-21-valid)
(defvar core-conc-int-mat-21-point 'core-conc-int-mat-21-point)
(defvar core-conc-int-mat-21-value 'core-conc-int-mat-21-value)


;;; compiler substitute symbols for parallel version
#+parallel
(setq core-conc-int-D-11 'device-little-float-1
      core-conc-int-D-12 'device-little-float-2
      core-conc-int-D-21 'device-little-float-3
      core-conc-int-D-22 'device-little-float-4
      core-conc-int-conc-core 'device-little-float-5
      core-conc-int-B 'device-little-float-6
      core-conc-int-k 'device-little-float-7

      core-conc-int-node-1-pointp 'core-device-node1-valid
      core-conc-int-node-1-point 'core-device-node1-point
      core-conc-int-node-1-const 'core-device-node1-const
      core-conc-int-node-1-voltage 'core-device-node1-voltage

      core-conc-int-current-1 'core-device-node1-current
      core-conc-int-current-2 'core-device-node2-current

      core-conc-int-node-2-pointp 'core-device-node2-valid
      core-conc-int-node-2-point 'core-device-node2-point
      core-conc-int-node-2-const 'core-device-node2-const
      core-conc-int-node-2-voltage 'core-device-node2-voltage

      core-conc-int-mat-12-valid 'core-device-mat-12-valid
      core-conc-int-mat-12-point 'core-device-mat-12-point
      core-conc-int-mat-12-value 'core-device-mat-12-value
      core-conc-int-mat-21-valid 'core-device-mat-21-valid
      core-conc-int-mat-21-point 'core-device-mat-21-point
      core-conc-int-mat-21-value 'core-device-mat-21-value)

(defvar core-segment-g-axial 'core-segment-g-axial)
(defvar core-segment-g-leak 'core-segment-g-leak)
(defvar core-segment-v-leak 'core-segment-v-leak)
(defvar core-segment-capacitance 'core-segment-capacitance)

(defvar core-segment-node-1-pointp 'core-segment-node-1-pointp)
(defvar core-segment-node-2-pointp 'core-segment-node-2-pointp)
(defvar core-segment-node-1-point 'core-segment-node-1-point)
(defvar core-segment-node-2-point 'core-segment-node-2-point)

(defvar core-segment-current 'core-segment-current)
(defvar core-segment-conductance 'core-segment-conductance)

(defvar core-segment-node-1-const 'core-segment-node-1-const)
(defvar core-segment-node-2-const 'core-segment-node-2-const)
(defvar core-segment-node-1-voltage 'core-segment-node-1-voltage)
(defvar core-segment-node-2-voltage 'core-segment-node-2-voltage)

(defvar core-segment-mat-12-valid 'core-segment-mat-12-valid)
(defvar core-segment-mat-12-point 'core-segment-mat-12-point)
(defvar core-segment-mat-12-value 'core-segment-mat-12-value)
(defvar core-segment-mat-21-valid 'core-segment-mat-21-valid)
(defvar core-segment-mat-21-point 'core-segment-mat-21-point)
(defvar core-segment-mat-21-value 'core-segment-mat-21-value)

#+parallel
(setf core-segment-g-axial 'device-little-float-1
      core-segment-g-leak 'device-little-float-2
      core-segment-v-leak 'device-little-float-3
      core-segment-capacitance 'device-little-float-4

      core-segment-node-1-pointp 'core-device-node1-valid
      core-segment-node-2-pointp 'core-device-node2-valid
      core-segment-node-1-point 'core-device-node1-point
      core-segment-node-2-point 'core-device-node2-point

      core-segment-current 'core-device-node1-current
      core-segment-conductance 'core-device-node1-conductance

      core-segment-node-1-const 'core-device-node1-const
      core-segment-node-2-const 'core-device-node2-const
      core-segment-node-1-voltage 'core-device-node1-voltage
      core-segment-node-2-voltage 'core-device-node2-voltage
      
      core-segment-mat-12-valid 'core-device-mat-12-valid
      core-segment-mat-12-point 'core-device-mat-12-point
      core-segment-mat-12-value 'core-device-mat-12-value
      core-segment-mat-21-valid 'core-device-mat-21-valid
      core-segment-mat-21-point 'core-device-mat-21-point
      core-segment-mat-21-value 'core-device-mat-21-value)
