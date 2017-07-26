;;; -*- Mode: LISP; Syntax: Common-lisp; package: #-parallel surf #+parallel *surf; Base: 10; -*-
;;; (c) Copyright 1985, Don Webber, Thinking Machines Corporation, Inc.
;;; alterations (c) Copyright 1990, Lyle Borg-Graham, MIT Center for Biological Information Processing

(in-package #+parallel '*surf #-parallel 'surf)

#+parallel
(defun send-voltages ()
  "Send and latch the voltages for all the elements."
  (*select-type (core-node)
    (*set fanout-value core-node-voltage-n+1))
  (*when fanout-valid
    (*set fanout-value (scan!! fanout-value 'copy!! :direction :forward
				  :segment-pvar fanout-seg-forward)))
  (*select-type (core-mos1 core-capacitor core-resistor core-isource core-soma
			   core-channel core-particle core-segment core-conc-part core-conc-int)
    (*when core-device-node1-valid
      (*set core-device-node1-voltage (pref!! fanout-value core-device-node1-point)))
    (*when core-device-node2-valid
      (*set core-device-node2-voltage (pref!! fanout-value core-device-node2-point)))
    (*when core-device-node3-valid
      (*set core-device-node3-voltage (pref!! fanout-value core-device-node3-point)))
    (*when core-device-node4-valid
      (*set core-device-node4-voltage (pref!! fanout-value core-device-node4-point)))
    (*when core-device-node5-valid
      (*set core-device-node5-voltage (pref!! fanout-value core-device-node5-point)))
    (*when core-device-node6-valid
      (*set core-device-node6-voltage (pref!! fanout-value core-device-node6-point)))
    ))

#+parallel
(defun return-current ()
  (*set fanout-value (!! zero))
  (*select-type (core-mos1 core-capacitor core-resistor core-isource core-soma core-channel
			   core-particle core-segment core-conc-part core-conc-int)
    (*when core-device-node1-valid
      (*pset :overwrite core-device-node1-current fanout-value core-device-node1-point))
    (*when core-device-node2-valid
      (*pset :overwrite core-device-node2-current fanout-value core-device-node2-point))
    (*when core-device-node3-valid
      (*pset :overwrite core-device-node3-current fanout-value core-device-node3-point))
    (*when core-device-node4-valid
      (*pset :overwrite core-device-node4-current fanout-value core-device-node4-point)))
  (*when fanout-valid
    (*set fanout-value (scan!! fanout-value '+!! :direction :backward
			       :segment-pvar fanout-seg-backward)))
  (*select-type (core-node)
    (node-latch-current)))

#+parallel
(defun return-charge ()
  (*set fanout-value (!! zero))
  (*select-type (core-mos1 core-capacitor core-resistor core-isource core-soma core-channel core-particle core-segment core-conc-part core-conc-int)
    (*when core-device-node1-valid
      (*pset :overwrite core-device-node1-charge fanout-value core-device-node1-point))
    (*when core-device-node2-valid
      (*pset :overwrite core-device-node2-charge fanout-value core-device-node2-point))
    (*when core-device-node3-valid
      (*pset :overwrite core-device-node3-charge fanout-value core-device-node3-point))
    (*when core-device-node4-valid
      (*pset :overwrite core-device-node4-charge fanout-value core-device-node4-point)))

  (*when fanout-valid
    (*set fanout-value (scan!! fanout-value '+!! :direction :backward
			       :segment-pvar fanout-seg-backward)))
  (*select-type (core-node)
    (node-latch-charge)))

#+parallel
(defun return-conductance ()
  (*set fanout-value (!! zero))
  (*select-type (core-mos1 core-capacitor core-resistor core-isource core-soma core-channel core-particle core-segment core-conc-part core-conc-int)
    (*when core-device-node1-valid
      (*pset :overwrite core-device-node1-conductance fanout-value core-device-node1-point))
    (*when core-device-node2-valid
      (*pset :overwrite core-device-node2-conductance fanout-value core-device-node2-point))
    (*when core-device-node3-valid
      (*pset :overwrite core-device-node3-conductance fanout-value core-device-node3-point))
    (*when core-device-node4-valid
      (*pset :overwrite core-device-node4-conductance fanout-value core-device-node4-point)))
  (*when fanout-valid
    (*set fanout-value (scan!! fanout-value '+!! :direction :backward
			       :segment-pvar fanout-seg-backward)))
  (*select-type (core-node)
    (node-latch-conductance)))

