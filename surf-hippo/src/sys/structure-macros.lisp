;; -*- mode: lisp; Syntax: Common-lisp; package: surf ; base: 10 ; -*-
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


;;; SYS Source file: structure-macros.lisp

;;;
;;; More structure slot macros.
;;;

(in-package "SURF-HIPPO")



;;;;; For all the models in order to easily access the respective hash tables.

(defmacro NODE-HASH-TABLE ()
  `(model-hash-table (get 'node 'model)))

(defmacro ELECTRODE-HASH-TABLE ()
  `(model-hash-table (get 'electrode 'model)))

(defmacro BUFFER-TYPE-HASH-TABLE ()
  `(model-hash-table (get 'buffer-type 'model)))

(defmacro PUMP-TYPE-HASH-TABLE ()
  `(model-hash-table (get 'pump-type 'model)))

(defmacro CHANNEL-TYPE-HASH-TABLE ()
  `(model-hash-table (get 'channel-type 'model)))

(defmacro CONC-INT-TYPE-HASH-TABLE ()
  `(model-hash-table (get 'conc-int-type 'model)))

(defmacro PARTICLE-TYPE-HASH-TABLE ()
  `(model-hash-table (get 'particle-type 'model)))

(defmacro CONC-PARTICLE-TYPE-HASH-TABLE ()
  `(model-hash-table (get 'conc-particle-type 'model)))

(defmacro SYNAPSE-TYPE-HASH-TABLE ()
  `(model-hash-table (get 'synapse-type 'model)))

(defmacro AXON-TYPE-HASH-TABLE ()
  `(model-hash-table (get 'axon-type 'model)))

(defmacro CELL-TYPE-HASH-TABLE ()
  `(model-hash-table (get 'CELL-TYPE 'model)))

(defmacro BUFFER-HASH-TABLE ()
  `(model-hash-table (get 'buffer 'model)))

(defmacro PUMP-HASH-TABLE ()
  `(model-hash-table (get 'pump 'model)))

(defmacro ISOURCE-HASH-TABLE ()
  `(model-hash-table (get 'isource 'model)))

(defmacro CELL-HASH-TABLE ()
  `(model-hash-table (get 'cell 'model)))

(defmacro SOMA-HASH-TABLE ()
  `(model-hash-table (get 'soma 'model)))

(defmacro CHANNEL-HASH-TABLE ()
  `(model-hash-table (get 'channel 'model)))

(defmacro CONC-INT-HASH-TABLE ()
  `(model-hash-table (get 'conc-int 'model)))

(defmacro PARTICLE-HASH-TABLE ()
  `(model-hash-table (get 'particle 'model)))

(defmacro CONC-PARTICLE-HASH-TABLE ()
  `(model-hash-table (get 'conc-particle 'model)))

(defmacro SYNAPSE-HASH-TABLE ()
  `(model-hash-table (get 'synapse 'model)))

(defmacro AXON-HASH-TABLE ()
  `(model-hash-table (get 'axon 'model)))

(defmacro SEGMENT-HASH-TABLE ()
  `(model-hash-table (get 'segment 'model)))

(defmacro VSOURCE-HASH-TABLE ()
  `(model-hash-table (get 'vsource 'model)))

(defmacro EXTRACELLULAR-ELECTRODE-HASH-TABLE ()
  `(model-hash-table (get 'extracellular-electrode 'model)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Nodes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro node-iterator (body)
  `(loop for nd being the hash-value of (NODE-HASH-TABLE) do ,body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Membrane element-iv-parameters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; These two are convenient accessors for the format of the :CONC-INT-TYPE-PARAMS in the
;; MEMBRANE-ELEMENT-IV-PARAMETERS structure (inherited by channel and synapse types).

(defmacro element-type-conc-int-type-param-permeability (conc-int-type-param)
  `(cadadr ,conc-int-type-param))

(defmacro element-type-conc-int-type-param-shell (conc-int-type-param)
  `(caadr ,conc-int-type-param))


(defmacro element-conc-int-param-cint (conc-int-param)
  `(first ,conc-int-param))

(defmacro element-conc-int-param-permeability (conc-int-param)
  `(the df (third ,conc-int-param)))

(defmacro element-conc-int-param-shell (conc-int-param)
  `(the fn (second ,conc-int-param)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Somas
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; If an integer, used as color index. SETFable.
(defmacro soma-color-index (soma)
  `(node-color-index (soma-node ,soma)))

#|
(defmacro soma-membrane-resistivity (soma)
  `(the sf (or (unless (soma-inherit-parameters-from-type ,soma)
		 (get-a-value `membrane-resistivity (soma-parameters ,soma)))
	    (cell-type-soma-resistivity (cell-type (soma-cell ,soma))))))

(defmacro soma-specific-capacitance (soma)
  `(the sf (or (unless (soma-inherit-parameters-from-type ,soma)
		 (get-a-value `specific-capacitance (soma-parameters ,soma)))
	    (cell-type-soma-specific-capacitance (cell-type (soma-cell ,soma))))))

(defmacro soma-v-leak (soma)
  `(the sf (or (unless (soma-inherit-parameters-from-type ,soma)
		 (get-a-value `v-leak (soma-parameters ,soma)))
	    (cell-type-soma-v-leak (cell-type (soma-cell ,soma))))))

|#


;;; Segments

;; If an integer, used as color index. SETFable.
(defmacro segment-color-index (seg)
  `(node-color-index (segment-node-2 ,seg)))

#|
(defmacro segment-membrane-resistivity (seg)
  `(the sf (or (unless (segment-inherit-parameters-from-type ,seg)
		 (get-a-value `membrane-resistivity (segment-parameters ,seg)))
	    (cell-type-membrane-resistivity (cell-type (segment-cell ,seg))))))

(defmacro segment-specific-capacitance (seg)
  `(the sf (or (unless (segment-inherit-parameters-from-type ,seg)
		 (get-a-value `specific-capacitance (segment-parameters ,seg)))
	    (cell-type-dendrite-specific-capacitance (cell-type (segment-cell ,seg))))))

(defmacro segment-cytoplasmic-resistivity (seg)
  `(the sf (or (unless (segment-inherit-parameters-from-type ,seg)
		 (get-a-value 'cytoplasmic-resistivity (segment-parameters ,seg)))
	    (cell-type-cytoplasmic-resistivity (cell-type (segment-cell ,seg))))))

(defmacro segment-v-leak (seg)
  `(the sf (or (unless (segment-inherit-parameters-from-type ,seg)
		 (get-a-value `v-leak (segment-parameters ,seg)))
	    (cell-type-dendrite-v-leak (cell-type (segment-cell ,seg))))))
  
(defmacro segment-cytoplasmic-resistivity-coeff (seg)
  `(s-flt (or
	   (get-a-value `ri-coeff (segment-parameters ,seg))
	   (get-a-value `cytoplasmic-resistivity-coeff (segment-parameters ,seg))
	    (get-a-value `ra-coeff (segment-parameters ,seg))
	    1.0)))

(defmacro segment-ra-coeff (seg)
  `(s-flt (or
	   (get-a-value `ri-coeff (segment-parameters ,seg))
	   (get-a-value `cytoplasmic-resistivity-coeff (segment-parameters ,seg))
	   (get-a-value `ra-coeff (segment-parameters ,seg))
	   1.0)))

(defmacro segment-ri-coeff (seg)
  `(s-flt (or
	   (get-a-value `ri-coeff (segment-parameters ,seg))
	   (get-a-value `cytoplasmic-resistivity-coeff (segment-parameters ,seg))
	   (get-a-value `ra-coeff (segment-parameters ,seg))
	   1.0)))
|#
  
(defmacro segment-iterator (body)
  `(loop for seg being the hash-value of (SEGMENT-HASH-TABLE) do ,body))

;;; Conc-int types

(defmacro conc-int-type-number-of-states (type)
  `(system-of-differential-equations-number-of-states (conc-int-type-system-of-differential-equations ,type)))

(defmacro conc-int-type-equation-coefficients-array (type)
  `(system-of-differential-equations-coefficients-array (conc-int-type-system-of-differential-equations ,type)))

;;; Conc-ints

(defmacro conc-int-core-conc (cint)
  `(conc-int-type-core-conc (conc-int-type ,cint)))

(defmacro conc-int-core-conc-double (cint)
  `(conc-int-type-core-conc-double (conc-int-type ,cint)))


(defmacro conc-int-shell-pore (conc-int-shell-pore-list)
  `(car ,conc-int-shell-pore-list))

(defmacro conc-int-shell-pore-perm (conc-int-shell-pore-list)
  `(the df (cadr ,conc-int-shell-pore-list)))



;;; Buffer types

(defmacro buffer-type-number-of-states (type)
  `(system-of-differential-equations-number-of-states (buffer-type-system-of-differential-equations ,type)))

(defmacro buffer-type-equation-coefficients-array (type)
  `(system-of-differential-equations-coefficients-array (buffer-type-system-of-differential-equations ,type)))


;;; Pump types

(defmacro pump-type-number-of-states (type)
  `(system-of-differential-equations-number-of-states (pump-type-system-of-differential-equations ,type)))

(defmacro pump-type-equation-coefficients-array (type)
  `(system-of-differential-equations-coefficients-array (pump-type-system-of-differential-equations ,type)))


  
;; Channels	

(defmacro channel-type-iv-relation (type)
  `(membrane-element-iv-parameters-iv-relation (channel-type-iv-parameters ,type)))

(defmacro channel-type-gbar-source (type)
  `(membrane-element-iv-parameters-gbar-source (channel-type-iv-parameters ,type)))
(defmacro channel-type-gbar-ref (type)
  `(membrane-element-iv-parameters-gbar-ref (channel-type-iv-parameters ,type)))
(defmacro channel-type-gbar-density (type)
  `(membrane-element-iv-parameters-gbar-density (channel-type-iv-parameters ,type)))

(defmacro channel-type-pbar-source (type)
  `(membrane-element-iv-parameters-gbar-source (channel-type-iv-parameters ,type)))
(defmacro channel-type-pbar-ref (type)
  `(membrane-element-iv-parameters-gbar-ref (channel-type-iv-parameters ,type)))
(defmacro channel-type-pbar-density (type)
  `(membrane-element-iv-parameters-gbar-density (channel-type-iv-parameters ,type)))

(defmacro channel-type-inherit-parameters-from-type (type)
  `(membrane-element-iv-parameters-inherit-parameters-from-type (channel-type-iv-parameters ,type)))
(defmacro channel-type-use-defined-e-rev (type)
  `(membrane-element-iv-parameters-use-defined-e-rev (channel-type-iv-parameters ,type)))
(defmacro channel-type-e-rev (type)
  `(membrane-element-iv-parameters-e-rev (channel-type-iv-parameters ,type)))
(defmacro channel-type-ion-permeabilities (type)
  `(membrane-element-iv-parameters-ion-permeabilities (channel-type-iv-parameters ,type)))
(defmacro channel-type-conc-int-type-params (type)
  `(membrane-element-iv-parameters-conc-int-type-params (channel-type-iv-parameters ,type)))
(defmacro channel-type-variable-e-rev (type)
  `(membrane-element-iv-parameters-variable-e-rev (channel-type-iv-parameters ,type)))
(defmacro channel-type-block (type)
  `(membrane-element-iv-parameters-block (channel-type-iv-parameters ,type)))
(defmacro channel-type-q10 (type)
  `(membrane-element-iv-parameters-q10 (channel-type-iv-parameters ,type)))
(defmacro channel-type-reference-temp (type)
  `(membrane-element-iv-parameters-reference-temp (channel-type-iv-parameters ,type)))

(defmacro channel-iv-relation (ch) `(channel-type-iv-relation (channel-type ,ch)))
(defmacro channel-use-defined-e-rev (ch) `(channel-type-use-defined-e-rev (channel-type ,ch)))
(defmacro channel-ion-permeabilities (ch) `(channel-type-ion-permeabilities (channel-type ,ch)))
(defmacro channel-conc-int-type-params (ch) `(channel-type-conc-int-type-params (channel-type ,ch)))
(defmacro channel-variable-e-rev (ch) `(channel-type-variable-e-rev (channel-type ,ch)))
(defmacro channel-q10 (ch) `(channel-type-q10 (channel-type ,ch)))
(defmacro channel-reference-temp (ch) "Degrees C" `(channel-type-reference-temp (channel-type ,ch)))

(defmacro channel-conductance (ch) "uS or cm3/s" `(memb-elt-iv-conductance (channel-iv-values ,ch)))
(defmacro channel-gbar (ch) "uS or cm3/s" `(memb-elt-iv-gbar (channel-iv-values ,ch)))
(defmacro channel-e-rev (ch) "mV" `(memb-elt-iv-e-rev (channel-iv-values ,ch)))
(defmacro channel-current (ch) "nA" `(memb-elt-iv-current (channel-iv-values ,ch)))
(defmacro channel-gbar/perm-reference-value (ch) "uS or cm3/s"
  `(memb-elt-iv-gbar/perm-reference-value (channel-iv-values ,ch)))



(defmacro CHANNEL-TYPE-DO ((ch ch-type) &rest body)
  "Iterates over all channels of a given type. CH is the loop symbol, CH-TYPE may be any form which
evaluates to a channel type."  
  (let ((type (gensym)))
    `(let ((,type ,ch-type))
      (do ((,ch (channel-type-first-channel ,type) (channel-next-channel ,ch)))
	  ((null ,ch))
	,@body))))

(defmacro CHANNEL-TYPE-ITERATOR ((var type) &rest body)
  `(loop with ,var = (channel-type-first-channel ,type)
    while ,var ,@body
    do (setq ,var (channel-next-channel ,var))))

(defmacro CHANNEL-TYPE-ITERATOR-FAST ((var type) &rest body)
  `(loop with ,var = (channel-type-first-channel ,type)
    ,@body
    (setq ,var (channel-next-channel ,var))
    unless ,var do (return)))



;; Synapses

(defmacro synapse-type-iv-relation (type)
  `(membrane-element-iv-parameters-iv-relation (synapse-type-iv-parameters ,type)))

(defmacro synapse-type-gbar-source (type)
  `(membrane-element-iv-parameters-gbar-source (synapse-type-iv-parameters ,type)))
(defmacro synapse-type-gbar-ref (type)
  `(membrane-element-iv-parameters-gbar-ref (synapse-type-iv-parameters ,type)))
(defmacro synapse-type-gbar-density (type)
  `(membrane-element-iv-parameters-gbar-density (synapse-type-iv-parameters ,type)))

(defmacro synapse-type-pbar-source (type)
  `(membrane-element-iv-parameters-gbar-source (synapse-type-iv-parameters ,type)))
(defmacro synapse-type-pbar-ref (type)
  `(membrane-element-iv-parameters-gbar-ref (synapse-type-iv-parameters ,type)))
(defmacro synapse-type-pbar-density (type)
  `(membrane-element-iv-parameters-gbar-density (synapse-type-iv-parameters ,type)))


(defmacro synapse-type-inherit-parameters-from-type (type)
  `(membrane-element-iv-parameters-inherit-parameters-from-type (synapse-type-iv-parameters ,type)))
(defmacro synapse-type-use-defined-e-rev (type)
  `(membrane-element-iv-parameters-use-defined-e-rev (synapse-type-iv-parameters ,type)))
(defmacro synapse-type-e-rev (type)
  `(membrane-element-iv-parameters-e-rev (synapse-type-iv-parameters ,type)))
(defmacro synapse-type-ion-permeabilities (type)
  `(membrane-element-iv-parameters-ion-permeabilities (synapse-type-iv-parameters ,type)))
(defmacro synapse-type-conc-int-type-params (type)
  `(membrane-element-iv-parameters-conc-int-type-params (synapse-type-iv-parameters ,type)))
(defmacro synapse-type-variable-e-rev (type)
  `(membrane-element-iv-parameters-variable-e-rev (synapse-type-iv-parameters ,type)))
(defmacro synapse-type-block (type)
  `(membrane-element-iv-parameters-block (synapse-type-iv-parameters ,type)))
(defmacro synapse-type-q10 (type)
  `(membrane-element-iv-parameters-q10 (synapse-type-iv-parameters ,type)))
(defmacro synapse-type-reference-temp (type)
  `(membrane-element-iv-parameters-reference-temp (synapse-type-iv-parameters ,type)))

(defmacro synapse-iv-relation (syn) `(synapse-type-iv-relation (synapse-type ,syn)))
(defmacro synapse-use-defined-e-rev (syn) `(synapse-type-use-defined-e-rev (synapse-type ,syn)))
(defmacro synapse-ion-permeabilities (syn) `(synapse-type-ion-permeabilities (synapse-type ,syn)))
(defmacro synapse-conc-int-type-params (syn) `(synapse-type-conc-int-type-params (synapse-type ,syn)))
(defmacro synapse-variable-e-rev (syn) `(synapse-type-variable-e-rev (synapse-type ,syn)))
(defmacro synapse-q10 (syn) `(synapse-type-q10 (synapse-type ,syn)))
(defmacro synapse-reference-temp (syn) "Degrees C" `(synapse-type-reference-temp (synapse-type ,syn)))

(defmacro synapse-conductance (syn) "uS or cm3/s" `(memb-elt-iv-conductance (synapse-iv-values ,syn)))
(defmacro synapse-gbar (syn) "uS or cm3/s" `(memb-elt-iv-gbar (synapse-iv-values ,syn)))
(defmacro synapse-e-rev (syn) "mV" `(memb-elt-iv-e-rev (synapse-iv-values ,syn)))
(defmacro synapse-current (syn) "nA" `(memb-elt-iv-current (synapse-iv-values ,syn)))
(defmacro synapse-gbar/perm-reference-value (syn) "uS or cm3/s"
  `(memb-elt-iv-gbar/perm-reference-value (synapse-iv-values ,syn)))


(defmacro fast-syn-event-followers (syn)
  `(get-a-value 'event-followers (synapse-parameters ,syn)))

(defmacro fast-set-syn-event-followers (syn value)
  `(set-element-parameter-fast ,syn 'event-followers ,value (synapse-parameters ,syn)))


(defmacro SYNAPSE-TYPE-DO ((syn syn-type) &rest body)
  "Iterates over all synapses of a given type. SYN is the loop symbol, SYN-TYPE may be any form which
evaluates to a synapse type."
  (let ((type (gensym)))
    `(let ((,type ,syn-type))
      (do ((,syn (synapse-type-first-synapse ,type) (synapse-next-synapse ,syn)))
	  ((null ,syn))
	,@body))))


(defmacro SYNAPSE-TYPE-ITERATOR ((var type) &rest body)
  `(loop with ,var = (synapse-type-first-synapse ,type)
    while ,var ,@body
    do (setq ,var (synapse-next-synapse ,var))))

(defmacro SYNAPSE-TYPE-ITERATOR-FAST ((var type) &rest body)
  `(loop with ,var = (synapse-type-first-synapse ,type)
    ,@body
    (setq ,var (synapse-next-synapse ,var))
    unless ,var do (return)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Particles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro PARTICLE-TYPE-ITERATOR ((var type) &rest body)
  `(loop with ,var = (particle-type-first-particle ,type)
    while ,var ,@body
    do (setq ,var (particle-next-particle ,var))))


(defmacro PARTICLE-TYPE-ITERATOR-FAST ((var type) &rest body)
  `(loop with ,var = (particle-type-first-particle ,type)
    ,@body
    (setq ,var (particle-next-particle ,var))
    unless ,var do (return)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Conc Particles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro CONC-PARTICLE-TYPE-ITERATOR ((var type) &rest body)
  `(loop with ,var = (conc-particle-type-first-particle ,type)
    while ,var ,@body
    do (setq ,var (conc-particle-next-particle ,var))))


(defmacro CONC-PARTICLE-TYPE-ITERATOR-FAST ((var type) &rest body)
  `(loop with ,var = (conc-particle-type-first-particle ,type)
    ,@body
    (setq ,var (conc-particle-next-particle ,var))
    unless ,var do (return)))

