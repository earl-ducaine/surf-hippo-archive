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


;;; SYS Source file: models.lisp
					
;; The create model functions.


(in-package "SURF-HIPPO")

(defmacro model-output-wrapper (model &body body)
  `(let* ((model-arg ,model)
	  (model (and model-arg (or (gethash (model-output-model-name model-arg) *model-hash-table*) model-arg))))
    ,@body))


(defun model-output-data-keys (model)
  (mapcar #'(lambda (DATA-TYPE-AND-ACCESS-FUNCTIONS)
	      (cadr (assoc 'DATA-PARAM-KEY (cdr DATA-TYPE-AND-ACCESS-FUNCTIONS))))
	  (model-DATA-TYPES-AND-ACCESS-INFO model)))

(defun model-output-data-types (model)
  (model-output-wrapper
   model
   (mapcar #'(lambda (DATA-TYPE-AND-ACCESS-FUNCTIONS)
	       (car DATA-TYPE-AND-ACCESS-FUNCTIONS))
	   (model-DATA-TYPES-AND-ACCESS-INFO model))))

;; The first entry in DATA-TYPES-AND-ACCESS-INFO gives the default data type. If there are synonyms,
;; take the first one.
(defun model-output-default-data-type (model)
  (let ((first-type (car (model-output-data-types model))))
    (typecase first-type
      (cons (car first-type))
      (t first-type))))

(defun model-output-data-type-info (model data-type)
  (loop for type-info in (model-DATA-TYPES-AND-ACCESS-INFO model) do
	(let ((result (typecase (car type-info)
			(cons (member data-type (car type-info)))
			(t (equal data-type (car type-info))))))
	  (when result (return type-info)))))

(defun model-output-data-info (model data-type info-key)
  (cadr (assoc info-key
	       (cdr (model-output-data-type-info
		     model
		     (or data-type (model-output-default-data-type model)))))))
  
(defun model-output-data-key (model &optional data-type)
  (model-output-data-info model data-type 'data-param-key))

(defun model-output-data-units (model &optional data-type)
  (model-output-data-info model data-type 'units))

(defun model-output-sparse-data-key (model &optional data-type)
  (model-output-data-info model data-type 'sparse-data-param-key))

(defun model-output-ordered-sparse-data-key (model &optional data-type)
  (model-output-data-info model data-type 'ordered-sparse-data-param-key))

(defun model-output-current-value-function (model &optional data-type)
  (model-output-data-info model data-type 'current-value-function))

(defun model-output-saved-data-function (model &optional data-type)
  (model-output-data-info model data-type 'saved-data-function))	       
    

(defun create-model (name &key
			  output-model-name
			  parameter-type-library
			  save-output-data-routine
			  output-data-structure-variables
			  DATA-TYPES-AND-ACCESS-INFO 
			  edit-routine
			  eval-routine
			  print-routine
			  short-print-routine
			  document-routine
			  create-routine
			  parent-model)
  (let* ((name-sym (typecase name
		     (string (read-from-string name))
		     (t name)))
	 (model (make-model :name name
			    :hash-table (make-hash-table :test #'equal)
			    :parameter-type-library parameter-type-library
			    :save-output-data-routine save-output-data-routine
			    :output-data-structure-variables output-data-structure-variables
			    :DATA-TYPES-AND-ACCESS-INFO DATA-TYPES-AND-ACCESS-INFO
			    :output-model-name output-model-name
			    :edit-routine edit-routine
			    :eval-routine eval-routine
			    :print-routine print-routine
			    :short-print-routine short-print-routine
			    :document-routine document-routine
			    :create-routine create-routine
			    :parent-model parent-model)))
    (setf (get name-sym 'model) model)
    (setf (gethash (string name) *model-hash-table*) model)))



(defun create-node-model ()
  (create-model "node"))

(defun create-channel-type-model ()
  (create-model "channel-type"
		:output-model-name "channel"
		:parameter-type-library '()
		:print-routine 'print-channel-type
		:short-print-routine 'print-channel-type-brief
		:edit-routine 'edit-channel-type
		:create-routine 'create-channel-type
		:document-routine 'document-channel-type))

(defun create-channel-model ()
  (create-model "channel"
		:parent-model (get 'channel-type 'model)
		:save-output-data-routine 'save-channel-data
		:output-data-structure-variables '(*plot-channel-currents-structures*
						   *plot-channel-reversal-potentials-structures*
						   *plot-channel-conductances-structures*)
		:DATA-TYPES-AND-ACCESS-INFO 
		'((CURRENT
		   (UNITS "nA")
		   (DATA-PARAM-KEY :CURRENT-DATA)
		   (SPARSE-DATA-PARAM-KEY :SPARSE-CURRENT-DATA)
		   (ORDERED-SPARSE-DATA-PARAM-KEY :ORDERED-SPARSE-CURRENT-DATA)
		   (CURRENT-VALUE-FUNCTION CHANNEL-CURRENT-VALUE)
		   (SAVED-DATA-FUNCTION CHANNEL-CURRENT-DATA))
		  (REVERSAL-POTENTIAL
		   (UNITS "mV")
		   (DATA-PARAM-KEY :REVERSAL-POTENTIAL-DATA)
		   (SPARSE-DATA-PARAM-KEY :SPARSE-REVERSAL-POTENTIAL-DATA)
		   (ORDERED-SPARSE-DATA-PARAM-KEY :ORDERED-SPARSE-REVERSAL-POTENTIAL-DATA)
		   (CURRENT-VALUE-FUNCTION CHANNEL-REVERSAL-POTENTIAL-VALUE)
		   (SAVED-DATA-FUNCTION CHANNEL-REVERSAL-POTENTIAL-DATA))
		  (CONDUCTANCE 
		   (UNITS "uS")
		   (DATA-PARAM-KEY :CONDUCTANCE-DATA)
		   (SPARSE-DATA-PARAM-KEY :SPARSE-CONDUCTANCE-DATA)
		   (ORDERED-SPARSE-DATA-PARAM-KEY :ORDERED-SPARSE-CONDUCTANCE-DATA)
		   (CURRENT-VALUE-FUNCTION CHANNEL-CONDUCTANCE-VALUE)
		   (SAVED-DATA-FUNCTION CHANNEL-CONDUCTANCE-DATA)))
		:print-routine 'print-channel
		:document-routine 'document-channel
		:edit-routine 'edit-channel
		:create-routine 'create-channel))

(defun create-synapse-type-model ()
  (create-model "synapse-type"
		:output-model-name "synapse"
		:parameter-type-library '()
		:print-routine 'print-synapse-type
		:short-print-routine 'print-synapse-type-brief
		:edit-routine 'edit-synapse-type
		:create-routine 'create-synapse-type
		:document-routine 'document-synapse-type))

(defun create-synapse-model ()
  (create-model "synapse"
		:parent-model (get 'synapse-type 'model)
		:save-output-data-routine 'save-synapse-data
		:output-data-structure-variables '(*plot-synapse-currents-structures*
						   *plot-synapse-reversal-potentials-structures*
						   *plot-synapse-conductances-structures*)
		:DATA-TYPES-AND-ACCESS-INFO 
		'((CURRENT 
		   (UNITS "nA")
		   (DATA-PARAM-KEY :CURRENT-DATA)
		   (SPARSE-DATA-PARAM-KEY :SPARSE-CURRENT-DATA)
		   (ORDERED-SPARSE-DATA-PARAM-KEY :ORDERED-SPARSE-CURRENT-DATA)
		   (CURRENT-VALUE-FUNCTION SYNAPSE-CURRENT-VALUE)
		   (SAVED-DATA-FUNCTION SYNAPSE-CURRENT-DATA))
		  (REVERSAL-POTENTIAL 
		   (UNITS "mV")
		   (DATA-PARAM-KEY :REVERSAL-POTENTIAL-DATA)
		   (SPARSE-DATA-PARAM-KEY :SPARSE-REVERSAL-POTENTIAL-DATA)
		   (ORDERED-SPARSE-DATA-PARAM-KEY :ORDERED-SPARSE-REVERSAL-POTENTIAL-DATA)
		   (CURRENT-VALUE-FUNCTION SYNAPSE-REVERSAL-POTENTIAL-VALUE)
		   (SAVED-DATA-FUNCTION SYNAPSE-REVERSAL-POTENTIAL-DATA))
		  (CONDUCTANCE 
		   (UNITS "uS")
		   (DATA-PARAM-KEY :CONDUCTANCE-DATA)
		   (SPARSE-DATA-PARAM-KEY :SPARSE-CONDUCTANCE-DATA)
		   (ORDERED-SPARSE-DATA-PARAM-KEY :ORDERED-SPARSE-CONDUCTANCE-DATA)
		   (CURRENT-VALUE-FUNCTION SYNAPSE-CONDUCTANCE-VALUE)
		   (SAVED-DATA-FUNCTION SYNAPSE-CONDUCTANCE-DATA)))
		:print-routine 'print-synapse
		:document-routine 'document-synapse
		:edit-routine 'edit-synapse
		:create-routine 'create-synapse))

(defun create-particle-type-model ()
  (create-model "particle-type"
		:output-model-name "particle"
		:parameter-type-library '()
		:print-routine 'print-particle-type
		:edit-routine 'edit-particle-type
		:document-routine 'document-particle-type
		:create-routine 'create-particle-type))

(defun create-particle-model ()
  (create-model "particle"
		:parent-model (get 'particle-type 'model)
		:save-output-data-routine 'save-particle-data
		:output-data-structure-variables '(*plot-particles-structures*
						   *plot-markov-particles-structures*)
		:DATA-TYPES-AND-ACCESS-INFO 
		'((STATE 
		   (UNITS "State")
		   (DATA-PARAM-KEY :STATE-DATA)
		   (SPARSE-DATA-PARAM-KEY :SPARSE-STATE-DATA)
		   (ORDERED-SPARSE-DATA-PARAM-KEY :ORDERED-SPARSE-STATE-DATA)
		   (CURRENT-VALUE-FUNCTION PARTICLE-STATE-VALUE)
		   (SAVED-DATA-FUNCTION PARTICLE-STATE-DATA)))
		:print-routine 'print-particle
		:create-routine 'create-particle))

(defun create-conc-particle-type-model ()
  (create-model "conc-particle-type"
		:output-model-name "conc-particle"
		:parameter-type-library '()
		:print-routine 'print-conc-particle-type
		:edit-routine 'edit-conc-particle-type
		:document-routine 'document-conc-particle-type
		:create-routine 'create-conc-particle-type))

(defun create-conc-particle-model ()
  (create-model "conc-particle"
		:parent-model (get 'conc-particle-type 'model)
		:save-output-data-routine 'save-conc-particle-data
		:output-data-structure-variables '(*plot-conc-particles-structures*)
		:DATA-TYPES-AND-ACCESS-INFO 
		'((STATE 
		   (UNITS "State")
		   (DATA-PARAM-KEY :STATE-DATA)
		   (SPARSE-DATA-PARAM-KEY :SPARSE-STATE-DATA)
		   (ORDERED-SPARSE-DATA-PARAM-KEY :ORDERED-SPARSE-STATE-DATA)
		   (CURRENT-VALUE-FUNCTION CONC-PARTICLE-STATE-VALUE)
		   (SAVED-DATA-FUNCTION CONC-PARTICLE-STATE-DATA)))
		:print-routine 'print-conc-particle
		:create-routine 'create-conc-particle))

(defun create-conc-int-type-model ()
  (create-model "conc-int-type"
		:output-model-name "conc-int"
		:parameter-type-library '()
		:print-routine 'print-conc-int-type
		:edit-routine 'edit-conc-int-type
		:document-routine 'document-conc-int-type
		:create-routine 'create-conc-int-type))

(defun create-conc-int-model ()
  (create-model "conc-int"
		:parent-model (get 'conc-int-type 'model)
		:save-output-data-routine 'save-conc-int-data
		:output-data-structure-variables '(*plot-conc-1-ints-structures*
						   *plot-conc-2-ints-structures*
						   *plot-conc-3-ints-structures*
						   *plot-conc-ints-structures*)
		:DATA-TYPES-AND-ACCESS-INFO 
		'(((TOTAL TOTAL-CONCENTRATION) 
		   (UNITS "mM")
		   (DATA-PARAM-KEY :TOTAL-DATA)
		   (SPARSE-DATA-PARAM-KEY :SPARSE-TOTAL-DATA)
		   (ORDERED-SPARSE-DATA-PARAM-KEY :ORDERED-SPARSE-TOTAL-DATA)
		   (CURRENT-VALUE-FUNCTION CONC-INT-TOTAL-VALUE)
		   (SAVED-DATA-FUNCTION CONC-INT-TOTAL-DATA))
		  ((1 CONCENTRATION-1 SHELL-1) 
		   (UNITS "mM")
		   (DATA-PARAM-KEY :SHELL-1-DATA)
		   (SPARSE-DATA-PARAM-KEY :SPARSE-SHELL-1-DATA)
		   (ORDERED-SPARSE-DATA-PARAM-KEY :ORDERED-SPARSE-SHELL-1-DATA)
		   (CURRENT-VALUE-FUNCTION CONC-INT-SHELL-1-VALUE)
		   (SAVED-DATA-FUNCTION CONC-INT-SHELL-1-DATA))
		  ((2 CONCENTRATION-2 SHELL-2) 
		   (UNITS "mM")
		   (DATA-PARAM-KEY :SHELL-2-DATA)
		   (SPARSE-DATA-PARAM-KEY :SPARSE-SHELL-2-DATA)
		   (ORDERED-SPARSE-DATA-PARAM-KEY :ORDERED-SPARSE-SHELL-2-DATA)
		   (CURRENT-VALUE-FUNCTION CONC-INT-SHELL-2-VALUE)
		   (SAVED-DATA-FUNCTION CONC-INT-SHELL-2-DATA))
		  ((3 CONCENTRATION-3 SHELL-3) 
		   (UNITS "mM")
		   (DATA-PARAM-KEY :SHELL-3-DATA)
		   (SPARSE-DATA-PARAM-KEY :SPARSE-SHELL-3-DATA)
		   (ORDERED-SPARSE-DATA-PARAM-KEY :ORDERED-SPARSE-SHELL-3-DATA)
		   (CURRENT-VALUE-FUNCTION CONC-INT-SHELL-3-VALUE)
		   (SAVED-DATA-FUNCTION CONC-INT-SHELL-3-DATA)))
		:print-routine 'print-conc-int
		:create-routine 'create-conc-int))

(defun create-isource-model ()
  (create-model "isource"
		:save-output-data-routine 'save-isource-data
		:output-data-structure-variables '(*plot-isource-currents-structures*)
		:DATA-TYPES-AND-ACCESS-INFO 
		'((CURRENT 
		   (UNITS "nA")
		   (DATA-PARAM-KEY :CURRENT-DATA)
		   (SPARSE-DATA-PARAM-KEY :SPARSE-CURRENT-DATA)
		   (ORDERED-SPARSE-DATA-PARAM-KEY :ORDERED-SPARSE-CURRENT-DATA)
		   (CURRENT-VALUE-FUNCTION ISOURCE-CURRENT-VALUE)
		   (SAVED-DATA-FUNCTION ISOURCE-CURRENT-DATA)))
		:eval-routine 'eval-isource
		:edit-routine 'edit-isource
		:print-routine 'print-isource
		:document-routine 'document-isource
		:create-routine 'create-pwl-isource))

(defun create-vsource-model ()
  (create-model "vsource"
		:save-output-data-routine 'save-vsource-data
		:output-data-structure-variables '(*plot-vsource-currents-structures* *plot-vsource-voltages-structures*)
		:DATA-TYPES-AND-ACCESS-INFO 
		'((CURRENT 
		   (UNITS "nA")
		   (DATA-PARAM-KEY :CURRENT-DATA)
		   (SPARSE-DATA-PARAM-KEY :SPARSE-CURRENT-DATA)
		   (ORDERED-SPARSE-DATA-PARAM-KEY :ORDERED-SPARSE-CURRENT-DATA)
		   (CURRENT-VALUE-FUNCTION VSOURCE-CURRENT-VALUE)
		   (SAVED-DATA-FUNCTION VSOURCE-CURRENT-DATA))
 		  (VOLTAGE 
		   (UNITS "mV")
		   (DATA-PARAM-KEY :VOLTAGE-DATA)
		   (SPARSE-DATA-PARAM-KEY :SPARSE-VOLTAGE-DATA)
		   (ORDERED-SPARSE-DATA-PARAM-KEY :ORDERED-SPARSE-VOLTAGE-DATA)
		   (CURRENT-VALUE-FUNCTION VSOURCE-VOLTAGE-VALUE)
		   (SAVED-DATA-FUNCTION VSOURCE-VOLTAGE-DATA)))
		:eval-routine 'eval-vsource
		:edit-routine 'edit-vsource
		:print-routine 'print-vsource
		:document-routine 'document-vsource
		:create-routine 'create-pwl-vsource))

(defun create-axon-type-model ()
  (create-model "axon-type"
		:output-model-name "axon"
		:parameter-type-library '()
		:print-routine 'print-axon-type
		:edit-routine 'edit-axon-type))

(defun create-axon-model ()
  (create-model "axon"
		:parent-model (get 'axon-type 'model)
		:save-output-data-routine 'save-axon-data
		:output-data-structure-variables '(*plot-axons-structures*)
		:DATA-TYPES-AND-ACCESS-INFO 
		'((VOLTAGE 
		   (UNITS "mV")
		   (DATA-PARAM-KEY :VOLTAGE-DATA)
		   (SPARSE-DATA-PARAM-KEY :SPARSE-VOLTAGE-DATA)
		   (ORDERED-SPARSE-DATA-PARAM-KEY :ORDERED-SPARSE-VOLTAGE-DATA)
		   (CURRENT-VALUE-FUNCTION AXON-VOLTAGE-VALUE)
		   (SAVED-DATA-FUNCTION AXON-VOLTAGE-DATA)))
		:eval-routine 'eval-axon
		:print-routine 'print-axon
		:create-routine 'create-axon))

(defun create-buffer-type-model ()
  (create-model "buffer-type"
		:output-model-name "buffer"
		:parameter-type-library '()
		:print-routine nil	; 'print-buffer-type
		:create-routine nil	; 'create-buffer-type
		))

(defun create-buffer-model ()
  (create-model "buffer"
		:parent-model (get 'buffer-type 'model)
		:save-output-data-routine 'save-buffer-data
		:output-data-structure-variables '(*plot-buffers-structures*)
		:DATA-TYPES-AND-ACCESS-INFO 
		'((CONCENTRATION 
		   (UNITS "mM")
		   (DATA-PARAM-KEY :CONCENTRATION-DATA)
		   (SPARSE-DATA-PARAM-KEY :SPARSE-CONCENTRATION-DATA)
		   (ORDERED-SPARSE-DATA-PARAM-KEY :ORDERED-SPARSE-CONCENTRATION-DATA)
		   (CURRENT-VALUE-FUNCTION BUFFER-CONCENTRATION-VALUE)
		   (SAVED-DATA-FUNCTION BUFFER-CONCENTRATION-DATA)))
		:eval-routine nil	; 'eval-buffer
		:print-routine nil	; 'print-buffer
		:create-routine nil	; 'create-buffer
		))

(defun create-cell-type-model ()
  (create-model "cell-type"
		:parameter-type-library '()
		:short-print-routine 'print-cell-type-brief
		:edit-routine 'edit-cell-type
		:print-routine 'print-cell-type
		:create-routine 'create-celltype)) ; CREATE-CELL-TYPE is old version

(defun create-cell-model ()
  (create-model "cell"
		:parent-model (get 'cell-type 'model)
		:short-print-routine 'print-cell
		:print-routine 'print-cell
		:create-routine 'create-cell))


(defun create-extracellular-electrode-model ()
  (create-model "extracellular-electrode"
		:save-output-data-routine 'save-extracellular-electrode-data
		:output-data-structure-variables '(*plot-field-potentials*)
		:DATA-TYPES-AND-ACCESS-INFO 
		'((FIELD-POTENTIAL 
		   (UNITS "mV")
		   (DATA-PARAM-KEY :ELECTRODE-FIELD-POTENTIAL-DATA)
		   (SPARSE-DATA-PARAM-KEY :SPARSE-ELECTRODE-FIELD-POTENTIAL-DATA)
		   (ORDERED-SPARSE-DATA-PARAM-KEY :ORDERED-SPARSE-ELECTRODE-FIELD-POTENTIAL-DATA)
		   (CURRENT-VALUE-FUNCTION EXTRACELLULAR-ELECTRODE-FIELD-POTENTIAL-VALUE)
		   (SAVED-DATA-FUNCTION EXTRACELLULAR-ELECTRODE-FIELD-POTENTIAL-DATA)))
		:print-routine 'print-extracellular-electrode
		:document-routine 'document-extracellular-electrode
		:edit-routine 'edit-extracellular-electrode
		:create-routine 'create-extracellular-electrode))

(defun create-electrode-model ()
  (create-model "electrode"
		:print-routine 'print-electrode
		:edit-routine 'edit-electrode))

(defun create-pump-type-model ()
  (create-model "pump-type"
		:output-model-name "pump"
		:document-routine 'document-pump-type
		:parameter-type-library '()
		:print-routine 'print-pump-type
		:edit-routine 'edit-pump-type
		:create-routine 'create-pump-type))

(defun create-pump-model ()
  (create-model "pump"
		:parent-model (get 'pump-type 'model)
		:save-output-data-routine 'save-pump-data
		:output-data-structure-variables '(*plot-pumps-structures*)
		:DATA-TYPES-AND-ACCESS-INFO 
		'((CURRENT 
		   (UNITS "nA")
		   (DATA-PARAM-KEY :CURRENT-DATA)
		   (SPARSE-DATA-PARAM-KEY :SPARSE-CURRENT-DATA)
		   (ORDERED-SPARSE-DATA-PARAM-KEY :ORDERED-SPARSE-CURRENT-DATA)
		   (CURRENT-VALUE-FUNCTION PUMP-CURRENT-VALUE)
		   (SAVED-DATA-FUNCTION PUMP-CURRENT-DATA)))
		:eval-routine 'eval-pump
		:print-routine 'print-pump
		:create-routine  'create-pump))

(defun create-segment-model ()
  (create-model "segment"
		:save-output-data-routine 'save-segment-data
		:output-data-structure-variables nil
		:DATA-TYPES-AND-ACCESS-INFO 
		'((VOLTAGE 
		   (UNITS "mV")
		   (DATA-PARAM-KEY :VOLTAGE-DATA)
		   (SPARSE-DATA-PARAM-KEY :SPARSE-VOLTAGE-DATA)
		   (ORDERED-SPARSE-DATA-PARAM-KEY :ORDERED-SPARSE-VOLTAGE-DATA)
		   (CURRENT-VALUE-FUNCTION RECORDED-ELEMENT-VOLTAGE)
		   (SAVED-DATA-FUNCTION SEGMENT-VOLTAGE-DATA))
		  ((DVDT VOLTAGE-DERIVATIVE NODE-VOLTAGE-DERIVATIVE) 
		   (UNITS "mV/ms")
		   (DATA-PARAM-KEY :VOLTAGE-DERIVATIVE-DATA)
		   (SPARSE-DATA-PARAM-KEY :SPARSE-VOLTAGE-DERIVATIVE-DATA)
		   (ORDERED-SPARSE-DATA-PARAM-KEY :ORDERED-SPARSE-VOLTAGE-DERIVATIVE-DATA)
		   (CURRENT-VALUE-FUNCTION ELEMENT-DVDT)
		   (SAVED-DATA-FUNCTION SEGMENT-VOLTAGE-DERIVATIVE-DATA)))
		:eval-routine 'eval-segment
		:print-routine 'print-segment
		:edit-routine 'edit-segment
		:create-routine 'create-segment))

(defun create-soma-model ()
  (create-model "soma"
		:save-output-data-routine 'save-soma-data
		:output-data-structure-variables '(*all-save-voltage-nodes*
						   *all-save-dvdt-nodes*
						   *plot-soma-dendrite-currents-structures*)
		:DATA-TYPES-AND-ACCESS-INFO 
		'((VOLTAGE 
		   (UNITS "mV")
		   (DATA-PARAM-KEY :VOLTAGE-DATA)
		   (SPARSE-DATA-PARAM-KEY :SPARSE-VOLTAGE-DATA)
		   (ORDERED-SPARSE-DATA-PARAM-KEY :ORDERED-SPARSE-VOLTAGE-DATA)
		   (CURRENT-VALUE-FUNCTION RECORDED-ELEMENT-VOLTAGE)
		   (SAVED-DATA-FUNCTION SOMA-VOLTAGE-DATA))
		  ((DVDT VOLTAGE-DERIVATIVE NODE-VOLTAGE-DERIVATIVE) 
		   (UNITS "mV/ms")
		   (DATA-PARAM-KEY :VOLTAGE-DERIVATIVE-DATA)
		   (SPARSE-DATA-PARAM-KEY :SPARSE-VOLTAGE-DERIVATIVE-DATA)
		   (ORDERED-SPARSE-DATA-PARAM-KEY :ORDERED-SPARSE-VOLTAGE-DERIVATIVE-DATA)
		   (CURRENT-VALUE-FUNCTION ELEMENT-DVDT)
		   (SAVED-DATA-FUNCTION SOMA-VOLTAGE-DERIVATIVE-DATA))
		  (DENDRITE-CURRENT 
		   (UNITS "nA")
		   (DATA-PARAM-KEY :DENDRITE-CURRENT-DATA)
		   (SPARSE-DATA-PARAM-KEY :SPARSE-DENDRITE-CURRENT-DATA)
		   (ORDERED-SPARSE-DATA-PARAM-KEY :ORDERED-SPARSE-DENDRITE-CURRENT-DATA)
		   (CURRENT-VALUE-FUNCTION SOMA-DENDRITE-CURRENT-VALUE)
		   (SAVED-DATA-FUNCTION SOMA-DENDRITE-CURRENT-DATA)))
		:eval-routine 'eval-soma
		:print-routine 'print-soma
		:edit-routine 'edit-soma
		:create-routine 'create-soma))




