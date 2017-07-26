;; -*- Mode: Lisp; Syntax: Common-lisp; Package: SURF ; Base: 10; -*-
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


;;; SYS Source file: cell.lisp


(in-package "SURF-HIPPO")

;;; This is the cell file.


(defun print-cell-type-concentrations (&optional cell-type)
  (loop for cell-type in (coerce-to-list (or cell-type (cell-types))) do
	(format t "   [Na+] in/out[mM]: ~a/~a~%"
		(simple-format-number (cell-type-na-conc-intra cell-type))
		(simple-format-number (cell-type-na-conc-extra cell-type)))
	(format t "   [K+] in/out[mM]: ~a/~a~%"
		(simple-format-number (cell-type-k-conc-intra cell-type))
		(simple-format-number (cell-type-k-conc-extra cell-type)))
	(format t "   [Cl-] in/out[mM]: ~a/~a~%"
		(simple-format-number (cell-type-cl-conc-intra cell-type))
		(simple-format-number (cell-type-cl-conc-extra cell-type)))
	(format t "   [Ca++] in/out[mM]: ~a/~a~%"
		(simple-format-number (cell-type-ca-conc-intra cell-type))
		(simple-format-number (cell-type-ca-conc-extra cell-type)))))

(defun update-cell-type-ionic-parameters (&optional type)
  (loop for type in (if type (list type) (cell-types))
	do
	(case (cell-type-na-conc-extra-dependence type)
	  (:follows-global (setf (cell-type-na-conc-extra type) *na-conc-extra*)))
	(case (cell-type-e-na-dependence type)
	  (:follows-global (setf (cell-type-e-na type) *e-k*))
	  (:follows-concentration (setf (cell-type-e-na type)
					(nernst-potential (cell-type-na-conc-intra type)
							  (cell-type-na-conc-extra type)
							  1.0))))
	(case (cell-type-k-conc-extra-dependence type)
	  (:follows-global (setf (cell-type-k-conc-extra type) *k-conc-extra*)))
	(case (cell-type-e-k-dependence type)
	  (:follows-global (setf (cell-type-e-k type) *e-k*))
	  (:follows-concentration (setf (cell-type-e-k type)
					(nernst-potential (cell-type-k-conc-intra type)
							  (cell-type-k-conc-extra type)
							  1.0))))
	(case (cell-type-ca-conc-extra-dependence type)
	  (:follows-global (setf (cell-type-ca-conc-extra type) *ca-conc-extra*)))
	(case (cell-type-e-ca-dependence type)
	  (:follows-global (setf (cell-type-e-ca type) *e-ca*))
	  (:follows-concentration (setf (cell-type-e-ca type)
					(nernst-potential (cell-type-ca-conc-intra type)
							  (cell-type-ca-conc-extra type)
							  2.0))))
	(case (cell-type-cl-conc-extra-dependence type)
	  (:follows-global (setf (cell-type-cl-conc-extra type) *cl-conc-extra*)))
	(case (cell-type-e-cl-dependence type)
	  (:follows-global (setf (cell-type-e-cl type) *e-cl*))
	  (:follows-concentration (setf (cell-type-e-cl type)
					(nernst-potential (cell-type-cl-conc-intra type)
							  (cell-type-cl-conc-extra type)
							  -1.0))))))
	


(defun update-linear-z-in-cell-type (cell-type)
  (loop for cell in (cell-type-cells cell-type) do (update-linear-z-in cell)))

(defun update-linear-z-in-cells ()
  (loop for cell in (cells) do (update-linear-z-in cell)))

(defun update-linear-z-in (cell)
  (setf (cell-max-g-in cell) (max-g-in cell))

  ;; Z-TREE-DISCRETE-IN-CELL and Z-TREE-CABLE-IN-CELL store the associated distal tree impedance in
  ;; each trunk segment. These values are then referenced by the subsequent calls to
  ;; Z-TREE-DISCRETE-IN-CELL-FROM-STORED-VALUES and Z-TREE-CABLE-IN-CELL-FROM-STORED-VALUES.
  
  (setf (cell-z-tree-discrete-in-cell cell) (z-tree-discrete-in-cell cell nil))
  (setf (cell-z-discrete-in-cell cell)
	(z-discrete-in-cell cell (z-tree-discrete-in-cell-from-stored-values cell t)))

  (setf (cell-z-tree-cable-in-cell cell) (z-tree-cable-in-cell cell nil))
  (setf (cell-z-cable-in-cell cell)
	(z-cable-in-cell cell (Z-TREE-CABLE-IN-CELL-FROM-STORED-VALUES cell t))))
						     
;; old version
(defun set-cell-type-param (element param &optional value update)
  (cell-type-param element param value update))

(defun cell-type-param (element param &optional value update)
  "When VALUE is a number sets the PARAM of cell type associated with ELEMENT, otherwise returns
the current value of the parameter. PARAM can be one of the following symbols:

This sets both the soma and the dendritic values -
 'RM (ohms-cm2)

This sets only the dendritic values-
 'RMDEND (ohms-cm2)

 'RMSOMA (ohms-cm2)
 'SOMA-SHUNT (ohms)
 'VL (mV)
 'SVL (mV)
 'DVL (mV)
 'CMSOMA (uF/cm2)
 'CMDEND (uF/cm2)
 'CM (uF/cm2)
 'RI (ohms-cm)

If CM is specified, this value is assigned to both the somatic and dendritic slots. Likewise, if VL
is specified, then this value is assigned to both the :SOMA-V-LEAK and :DENDRITE-V-LEAK slots.  Note
that the cell type parameters will not be propagated to the segments and soma until
SET-CIRCUIT-ELEMENTS-PARAMETERS is called."
  (let ((type (element-type (element-cell element)))
	(return-value nil)
	(param-found t))
    ;; Is this necessary???
    (when value (setq *enable-segment-membrane-parameter-update* t
		      *enable-soma-membrane-parameter-update* t))
    (setq value (when (numberp value) (coerce value 'single-float)))
    (when type
      (when value (clear-all-z-cable-in type))	
      (setq return-value
	    (case param
	      ((rmsoma soma-resistivity)
	       (if value (setf (cell-type-soma-resistivity type) value)
		   (cell-type-soma-resistivity type)))
	      (soma-shunt (if value (setf (cell-type-soma-shunt type) value)
			      (cell-type-soma-shunt type)))
	      ((svl soma-v-leak)
	       (if value (setf (cell-type-soma-v-leak type) value)
		   (cell-type-soma-v-leak type)))
	      ((dvl dendrite-v-leak)
	       (if value (setf (cell-type-dendrite-v-leak type) value)
		   (cell-type-dendrite-v-leak type)))
	      ((vl v-leak)
	       (if value (setf (cell-type-soma-v-leak type) value
			       (cell-type-dendrite-v-leak type) value)
		   (cell-type-soma-v-leak type)))
	      ((rmdend dendrite-resistivity)
	       (if value (setf (cell-type-membrane-resistivity type) value)
		   (cell-type-membrane-resistivity type)))
	      ((rm membrane-resistivity)
	       (if value (progn (setf (cell-type-membrane-resistivity type) value)
				(setf (cell-type-soma-resistivity type) value))
		   (cell-type-membrane-resistivity type)))
	      ((spcapsoma soma-specific-capacitance cmsoma)
	       (if value (setf (cell-type-soma-specific-capacitance type) value)
		   (cell-type-soma-specific-capacitance type)))

	      ((spcapden dendrite-specific-capacitance CMDEND)
	       (if value (setf (cell-type-dendrite-specific-capacitance type) value)
		   (cell-type-dendrite-specific-capacitance type)))
	      ;; If only CM, SPCAP, SPECIFIC-CAPACITANCE or MEMBRANE-CAPACITANCE is specified, assign it to
	      ;; both the somatic and dendritic slots.
	      ((cm spcap specific-capacitance membrane-capacitance)
	       (if value
		   (setf (cell-type-soma-specific-capacitance type) value
			 (cell-type-dendrite-specific-capacitance type) value)
		   (cell-type-soma-specific-capacitance type)))
	      ((ri cytoplasmic-resistivity)
	       (if value (setf (cell-type-cytoplasmic-resistivity type) value)
		   (cell-type-cytoplasmic-resistivity type)))
	      (t (setq param-found nil))))
      (when (and param-found value)
	(if update
	    (set-circuit-elements-parameters)
	    (setq *recheck-circuit-elements-parameters* t)))
      (unless param-found (format t "~%ERROR: Cell types do not have parameter ~a!~%" param)))
    (unless type (format t "~%ERROR: Cell type ~a doesn't exist!~%" type))
    

    (or value return-value)))

(defun print-cell-types ()
  (PRINT-MODEL-PARAMETERS "cell-type"))

(defun create-celltype (type-symbol &optional actual-type-symbol update-parameters)
  "TYPE-SYMBOL is a symbol or a cell type; in the former case it must match the CAR of one of the
lists contained in cell type model parameter library. Returns the cell type structure, whether is
was already made or not. If the type was already made, and UPDATE-PARAMETERS is T, the type will be
updated according to the current description loaded in parameter library. Note that the function
CREATE-CELL-TYPE refers to the older version of this function which does not reference the cell-type
parameter library. If TYPE-SYMBOL does not correspond to an entry in the parameter library, then the
cell type parameters will be taken from various global variables, including *R-MEM*, *R-I*,
*CAP-MEM*, *CAP-MEM-DENDRITE*, *SOMA-SHUNT*, *E-LEAK*, *E-NA*, *E-K*, *E-CA*, *E-CL*, in addition to
default specifications for reversal potentials (:FIXED) and concentrations (:FOLLOWS-GLOBAL).
The TYPE-SYMBOL that is actually used for the type is an uppercase symbol."
  (typecase type-symbol
    (cell-type (setq type-symbol (intern (cell-type-name type-symbol))))
    (string (setq type-symbol (intern (string-upcase type-symbol)))))
  (let* ((type (unless actual-type-symbol
		 (if (cell-type-p type-symbol) type-symbol (gethash (string type-symbol) (CELL-TYPE-HASH-TABLE)))))
	 (model (type-symbol-model 'cell-type))
	 (original-parameters (retrieve-model-parameters-from-library type-symbol model))
	 (parent-type-symbol (get-a-value 'parent-type original-parameters)))
    (unless (and type (not update-parameters))
      (when (eq parent-type-symbol type-symbol)
	(sim-error (format nil "~A has a recursive parent type!" parent-type-symbol)))
      (unless original-parameters
	; (sim-error (format nil "Don't know anything about cell type ~A!" type-symbol))
	(format t "Don't know anything about cell type ~A - substituting generic values from global vars!~%" type-symbol))
      (when actual-type-symbol (setq type-symbol actual-type-symbol))
      (unless type
	(setq type (if parent-type-symbol
		       (create-CELLTYPE parent-type-symbol type-symbol update-parameters)
		       (make-CELL-TYPE :name (string type-symbol)))))
      (setq original-parameters (update-element-parameters-with-new-parameters original-parameters type))    
      (when parent-type-symbol (push-element-parameter type 'parent-types parent-type-symbol))
      (setf
       (cell-type-notes type) (or (get-a-value 'notes original-parameters) "")
       (cell-type-soma-resistivity type) (s-flt (or (get-a-value 'soma-resistivity original-parameters)
						    (get-a-value 'rmsoma original-parameters)
						    (get-a-value 'membrane-resistivity original-parameters)
						    (get-a-value 'rm original-parameters)
						    *r-mem*))

       (cell-type-membrane-resistivity type) (s-flt (or (get-a-value 'membrane-resistivity original-parameters)
							(get-a-value 'rm original-parameters)
							*r-mem*))
	    
       (cell-type-soma-shunt type) (s-flt (or (get-a-value 'soma-shunt original-parameters) *soma-shunt*))

       (cell-type-cytoplasmic-resistivity type) (s-flt (or (get-a-value 'cytoplasmic-resistivity original-parameters)
							   (get-a-value 'ri original-parameters)
							   (get-a-value 'ra original-parameters)
							   *r-i*))
	    
       (cell-type-soma-specific-capacitance type) (s-flt (or (get-a-value 'soma-specific-capacitance original-parameters)
							     (get-a-value 'spcapsoma original-parameters)
							     (get-a-value 'specific-capacitance original-parameters)
							     (get-a-value 'spcap original-parameters)
							     (get-a-value 'cm original-parameters)
							     *cap-mem*))
		       
       (cell-type-dendrite-specific-capacitance type) (s-flt (or (get-a-value 'dendrite-specific-capacitance
									      original-parameters)
								 (get-a-value 'spcapden original-parameters)
								 (get-a-value 'specific-capacitance original-parameters)
								 (get-a-value 'spcap original-parameters)
								 (get-a-value 'cm original-parameters)
								 *cap-mem-dendrite*))

       (cell-type-dendrite-v-leak type) (s-flt (or (get-a-value 'dendrite-v-leak original-parameters)
						   (get-a-value 'dvl original-parameters)
						   (get-a-value 'vl original-parameters)
						   (get-a-value 'v-leak original-parameters)
						   *e-leak*))
		       
       (cell-type-soma-v-leak type) (s-flt (or (get-a-value 'soma-v-leak original-parameters)
					       (get-a-value 'svl original-parameters)
					       (get-a-value 'vl original-parameters)
					       (get-a-value 'v-leak original-parameters)
					       *e-leak*))

       (cell-type-e-na type) (s-flt (or (get-a-value 'e-na original-parameters) *e-na*)) 
       (cell-type-e-k type) (s-flt (or (get-a-value 'e-k original-parameters) *e-k*))
       (cell-type-e-ca type) (s-flt (or (get-a-value 'e-ca original-parameters) *e-ca*))
       (cell-type-e-cl type) (s-flt (or (get-a-value 'e-cl original-parameters) *e-cl*))
       (cell-type-e-na-dependence type) (or (get-a-value 'e-na-dependence original-parameters) :fixed)
       (cell-type-na-conc-extra-dependence type) (or (get-a-value 'na-conc-extra-dependence
								  original-parameters) :follows-global)
       (cell-type-e-k-dependence type) (or (get-a-value 'e-k-dependence original-parameters) :fixed)
       (cell-type-k-conc-extra-dependence type) (or (get-a-value 'k-conc-extra-dependence
								 original-parameters) :follows-global)
       (cell-type-e-ca-dependence type) (or (get-a-value 'e-ca-dependence original-parameters) :fixed)
       (cell-type-ca-conc-extra-dependence type) (or (get-a-value 'ca-conc-extra-dependence original-parameters)
						     :follows-global)
       (cell-type-e-cl-dependence type) (or (get-a-value 'e-cl-dependence original-parameters) :fixed)
       (cell-type-cl-conc-extra-dependence type) (or (get-a-value 'cl-conc-extra-dependence original-parameters)
						     :follows-global))

      
      (setf (gethash (cell-type-name type) (CELL-TYPE-HASH-TABLE)) type)
      (update-cell-type-ionic-parameters type))
    (setq *cell-type* type)))

(defun document-cell-type (type)
  (let ((type-name (element-name type 'cell-type))
	(type (element type 'cell-type)))
    (when type
      (format t "(cell-type-def~%")
      (format t "  '(~a~%" type-name)
      (format t "     (soma-resistivity . ~S)~%" (cell-type-soma-resistivity type))
      (format t "     (membrane-resistivity . ~S)~%" (cell-type-membrane-resistivity type))
      (format t "     (soma-shunt . ~S)~%" (cell-type-soma-shunt type))
      (format t "     (cytoplasmic-resistivity . ~S)~%" (cell-type-cytoplasmic-resistivity type))

      (format t "     (soma-specific-capacitance . ~S)~%" (cell-type-soma-specific-capacitance type))
      (format t "     (specific-capacitance . ~S)~%" (cell-type-dendrite-specific-capacitance type))

      (format t "     (v-leak . ~S)~%" (cell-type-dendrite-v-leak type))
      (format t "     (soma-v-leak . ~S)~%" (cell-type-soma-v-leak type))

      (format t "     (e-na . ~S)~%" (cell-type-e-na type))
      (format t "     (e-k . ~S)~%" (cell-type-e-k type))
      (format t "     (e-ca . ~S)~%" (cell-type-e-ca type))
      (format t "     (e-cl . ~S)~%" (cell-type-e-cl type))

      (format t "     (e-na-dependence . ~S)~%" (cell-type-e-na-dependence type))
      (format t "     (e-k-dependence . ~S)~%" (cell-type-e-k-dependence type))
      (format t "     (e-ca-dependence . ~S)~%" (cell-type-e-ca-dependence type))
      (format t "     (e-cl-dependence . ~S)~%" (cell-type-e-cl-dependence type))

      (format t "     (na-conc-extra-dependence . ~S)~%" (cell-type-na-conc-extra-dependence type))
      (format t "     (k-conc-extra-dependence . ~S)~%" (cell-type-k-conc-extra-dependence type))
      (format t "     (ca-conc-extra-dependence . ~S)~%" (cell-type-ca-conc-extra-dependence type))
      (format t "     (cl-conc-extra-dependence . ~S)~%" (cell-type-cl-conc-extra-dependence type))

      (format t "     (notes . ~s)~%" (or (cell-type-notes type) ""))
      
      (element-document-extras type)
      
      (format t "                ))~%")
      (format t "~%~%~%"))))

;; Old version of create-cell-type, which does not use the cell-type parameter library.
(defun create-cell-type (&optional name &key
				   (membrane-resistivity *r-mem*) rm ; ohms-cm2
				   (cytoplasmic-resistivity *r-i*) ri ; ohms-cm
				   soma-resistivity rmsoma ; ohms-cm2
				   (soma-shunt *soma-shunt*) ; ohms
				   soma-specific-capacitance spcapsoma ; uF/cm2
				   dendrite-specific-capacitance spcapden ; uF/cm2
				   (specific-capacitance *cap-mem-dendrite*) spcap ; uF/cm2
				   (v-leak *dendrite-e-leak*) vl ; mV
				   dendrite-v-leak dvl ; mV
				   soma-v-leak svl ; mV
				   (e-na *e-na*) ; mV
				   (e-k *e-k*) ; mV
				   (e-ca *e-ca*) ; mV
				   (e-cl *e-cl*) ; mV

				   (e-na-dependence :fixed)
				   (na-conc-extra-dependence :follows-global)
				   (e-k-dependence :fixed)
				   (k-conc-extra-dependence :follows-global)
				   (e-ca-dependence :fixed)
				   (ca-conc-extra-dependence :follows-global)
				   (e-cl-dependence :fixed)
				   (cl-conc-extra-dependence :follows-global)
			      
				   (notes ""))
;    "Creates a new cell type with name NAME and parameters given by the key arguments, if not
; already defined. Returns the type. Most of these arguments may be referenced by a shorthand version
; of the keyword for convenience, e.g. a :RM keyword argument can be used instead of \(and will
; supersede\) a :MEMBRANE-RESISTIVITY keyword argument. For the dendritic/somatic parameter assignments
; if the dendritic or somatic argument is not supplied, then the general parameter is used. For
; example, :SOMA-RESISTIVITY or :RMSOMA will supersede :MEMBRANE-RESISTIVITY for the soma. Likewise,
; if either VL or V-LEAK is specifiec, then this value is assigned to both the :SOMA-V-LEAK and
; :DENDRITE-V-LEAK slots. If either SPCAP, SPECIFIC-CAPACITANCE or MEMBRANE-CAPACITANCE is specified,
; this value is assigned to both the somatic and dendritic slots."
  (setq *cell-type*
	(or (element name 'cell-type)
	    (let ((name (typecase name
			  (string name)
			  (schema nil)
			  (t (string (or name "Default Cell Type"))))))
	      (cond-every
	       (rm (setq membrane-resistivity rm))
	       (ri (setq cytoplasmic-resistivity ri))
	       ((not soma-resistivity) (setq soma-resistivity membrane-resistivity))
	       (rmsoma (setq soma-resistivity rmsoma))
	       (spcap (setq specific-capacitance spcap))
	       ((not soma-specific-capacitance) (setq soma-specific-capacitance specific-capacitance))
	       ((not dendrite-specific-capacitance) (setq dendrite-specific-capacitance specific-capacitance))
	       (spcapsoma (setq soma-specific-capacitance spcapsoma))
	       (spcapden (setq dendrite-specific-capacitance spcapden))
     
	       (vl (setq v-leak vl))
	       ((not dendrite-v-leak) (setq dendrite-v-leak v-leak))
	       (dvl (setq dendrite-v-leak dvl))
	       ((not soma-v-leak) (setq soma-v-leak (or v-leak dendrite-v-leak)))
	       (svl (setq soma-v-leak svl)))
	      (let ((cell-type (make-cell-type :name name
					       :notes notes
					       :soma-resistivity (s-flt soma-resistivity)
					       :soma-shunt (s-flt soma-shunt)
					       :membrane-resistivity (s-flt membrane-resistivity)
					       :cytoplasmic-resistivity (s-flt cytoplasmic-resistivity )
					       :soma-specific-capacitance (s-flt soma-specific-capacitance)
					       :dendrite-specific-capacitance (s-flt dendrite-specific-capacitance)
					       :dendrite-v-leak (s-flt dendrite-v-leak)
					       :soma-v-leak (s-flt soma-v-leak)
					       :e-na (s-flt e-na) 
					       :e-k (s-flt e-k)	
					       :e-ca (s-flt e-ca)	
					       :e-cl (s-flt e-cl)

					       :e-na-dependence e-na-dependence
					       :na-conc-extra-dependence na-conc-extra-dependence
					       :e-k-dependence e-k-dependence
					       :k-conc-extra-dependence k-conc-extra-dependence
					       :e-ca-dependence e-ca-dependence
					       :ca-conc-extra-dependence ca-conc-extra-dependence
					       :e-cl-dependence e-cl-dependence
					       :cl-conc-extra-dependence cl-conc-extra-dependence)))
		(setf (gethash (string name) (CELL-TYPE-HASH-TABLE)) cell-type)
		(update-cell-type-ionic-parameters cell-type)
		cell-type)))))


(defun print-cell-type-brief (&optional explicit-type)
  (print-cell-type explicit-type t))

(defun print-cell-type (&optional explicit-type brief)
  (loop for cell-type in (if explicit-type (list explicit-type) (CELL-TYPEs)) do
	(format t "Cell-type ~a:~%" (cell-type-name cell-type))
	(format t "   Rm ~d, Rm-sm ~d ohm-cm2, "
		(cell-type-membrane-resistivity cell-type)
		(cell-type-soma-resistivity cell-type))
	(if (loop for cell in (cell-type-cells cell-type)
		  when (soma-include-shunt (cell-soma cell)) do (return t))
		 
	    (format t "Soma shunt ~d ohms, Ri ~d ohm-cm, Cm-sm ~d, Cm-den ~d uF/cm2~%"
		    (cell-type-soma-shunt cell-type)
		    (cell-type-cytoplasmic-resistivity cell-type)
		    (cell-type-soma-specific-capacitance cell-type)
		    (cell-type-dendrite-specific-capacitance cell-type))
	    (format t "Ri ~d ohm-cm, Cm-sm ~d, Cm-den ~d uF/cm2~%"
		    (cell-type-cytoplasmic-resistivity cell-type)
		    (cell-type-soma-specific-capacitance cell-type)
		    (cell-type-dendrite-specific-capacitance cell-type)))
	(format t "   E-soma-leak ~d mV, E-dendrite-leak ~d mV~%"
		(cell-type-soma-v-leak cell-type)
		(cell-type-dendrite-v-leak cell-type))
	(format t "   E-Na ~,2f mV, E-K ~,2f mV, E-Ca ~,2f mV, E-Cl ~,2f mV~%"
		(cell-type-e-na cell-type)
		(cell-type-e-k cell-type)	
		(cell-type-e-ca cell-type)	
		(cell-type-e-cl cell-type))
	(when (> (length (cell-type-notes cell-type)) 0)
	  (format t "   ~A~%" (cell-type-notes cell-type)))
	(unless brief
	  (PRINT-CELL-TYPE-CONCENTRATIONS cell-type)
	  (print-num-elements-sourcefile cell-type))
	(format t "~%")))



(defun cell-r-in (&optional (cell *cell*))
  "Input resistance of CELL, in Mohms [references CELL-Z-DISCRETE-IN-CELL]."
  (unless *circuit-processed* (process-circuit-structure))
  (cell-z-discrete-in-cell cell))

(defun soma-shunt-string (cell)
  (if (soma-include-shunt (cell-soma cell))
      (format nil " (includes shunt of ~,2f Mohms)"
	      (/ 1.0 (soma-g-shunt (cell-soma cell))))
      ""))

;; handle this in print-cell-type
(defun print-cell (cell)
  (unless *circuit-processed* (process-circuit-structure))
  (format t "Cell ~a (soma @ [~d ~d ~d])"
	  (cell-name cell)
	  (first (cell-origin cell))
	  (second (cell-origin cell))
	  (third (cell-origin cell)))
  (let ((cell-definition (element-parameter cell 'cell-definition)))
    (when (if (stringp cell-definition)
	      (> (length cell-definition) 0)
	      cell-definition)
      (format t "  -  Created from ~A" cell-definition)))
  (format t "~%")
  (if (cell-tree-p cell)
      (let ((terms (or (element-parameter cell 'number-cell-distal-segments)
		       (element-parameter cell 'number-cell-distal-segments (count-cell-distal-segments cell))))
	    (segs (length (cell-segments cell)))
	    (trunks (length (trunk-segments cell)))
	    (bps (or (element-parameter cell 'number-cell-branch-points)
		     (element-parameter cell 'number-cell-branch-points (count-branch-points cell)))))
	(format t   "      ~A~A~a~A Membrane Area ~,2eum^2~%"
		(if (> segs 0) (format nil "~A Segment~:p," segs) "")
		(if (> bps 0) (format nil " ~a Branch point~:p," bps) "")
		(if (> trunks 0) (format nil " ~a Trunk~:p," trunks) "") 
		(if (> terms 0) (format nil " ~a Terminal~:p," terms) "")
		(or (element-parameter cell 'total-area) (update-cell-area cell))))
      (format t   "      Membrane Area ~,2eum^2~%"
	      (or (element-parameter cell 'total-area) (update-cell-area cell))))
  (unless (cell-max-g-in cell) (update-linear-z-in cell))
  (let ((g-in (cell-max-g-in cell)))
    (format
     t
     "      Passive somatic R-in (actual|cable model) = ~,2f|~,2f Mohms ~%"
     (cell-z-discrete-in-cell cell) (cell-z-cable-in-cell cell))
					;    (when g-in
					;      (format
					;       t
					;       "       Max G-in / Min R-in = ~,2f uS / ~,2f Mohms ~%"
					;       g-in (/ 1.0 g-in)))
    (if (cell-tree-p cell)
	(format t "        R-Soma (passive) = ~,2f Mohms~a"
		(/ 1.0 (+ (soma-g-leak (cell-soma cell))
			  (if (soma-include-shunt (cell-soma cell)) (soma-g-shunt (cell-soma cell)) 0.0)))
		(soma-shunt-string cell))
	(format t "        ~a" (soma-shunt-string cell)))
		
    (when (soma-segments cell)
      (format t ", ~a R-Soma segment~:p = ~,2f Mohms"
	      (length (soma-segments cell))
	      (/ 1.0 (soma-segments-g-leak cell))))
    (format t "~%")
    (when (cell-tree-p cell)
      (format
       t
       "        Tree R-in (passive) (actual|cable model) = ~,2f|~,2f Mohms~%"
       (cell-z-tree-discrete-in-cell cell)
       (cell-z-tree-cable-in-cell cell))
      (format
       t  
       (case (element-parameter (cell-type cell) 'g-axial-computation)
	 (:average-r "       Coupling R's from axial R average [average-r]~%")
	 (:average-g "       Coupling R's from axial G average [average-g]~%")
	 (t				; (:single-leg t)
	  "        Coupling R's from individual compartments [single-leg]~%")))))
  (format t "~%"))

 
;; COLLECT-CIRCUIT-OBJECTS This is to make sure that everyone knows about everyone else - this step
;; should eventually be unecessary when all references are filled at the appropriate create object
;; call.
(defun collect-circuit-objects ()
  (collect-cell-types)
;;  (collect-cells-nodes)
  (collect-cells-segments)
  ;;	   (collect-cells-somas)
  )

(defun collect-cell-types ()
  (maphash 'clear-cell-type-cells (CELL-TYPE-HASH-TABLE))
  (maphash 'collect-cell-type (CELL-HASH-TABLE)))

(defun clear-cell-type-cells (name cell-type)
  (declare (ignore name))
  (setf (cell-type-cells cell-type) nil))

(defun collect-cell-type (name cell)
  (declare (ignore name))
  (let ((cell-type (cell-type cell)))
    (if cell-type
	(if (member cell (cell-type-cells cell-type))
	    t
	    (push cell (cell-type-cells cell-type))))))

(defun collect-cells-segments ()
  (maphash 'clear-cell-segments (CELL-HASH-TABLE))
  (maphash 'collect-segments-cell (SEGMENT-HASH-TABLE)))

(defun clear-cell-segments (name cell)
  (declare (ignore name))
  (setf (cell-segments cell) nil))

(defun collect-segments-cell (name seg)
  (declare (ignore name))
  (unless (get-a-value 'electrode (segment-parameters seg))
    (push seg (cell-segments (segment-cell seg)))))

(defun collect-cells-somas ()
  (maphash 'collect-somas-cell (SOMA-HASH-TABLE)))

(defun collect-somas-cell (name soma)
  (declare (ignore name))
  (setf (cell-soma (soma-cell soma)) soma))



(defvar *clear-cell-name-maps* t)

(defun create-cell (cell-name &key cell-type
			      soma-diameter (segment-diameter 0.5) (cell-origin '(0.0 0.0 0.0)) ; microns
			      tree-list
			      (name-suffix *cell-name-suffix*)
			      (enable-automatic-cell-names *enable-automatic-cell-names*)
			      (automatic-name-fixing *PROMPT-FOR-ALTERNATE-ELEMENT-NAMES*))
  "Creates a new cell, if not already defined. Returns the cell. If cell is not already defined, a soma is created when SOMA-DIAMETER [microns] is
supplied. If SOMA-DIAMETER and TREE-LIST are supplied, TREE-LIST is used in a call to CREATE-TREE, with a :DEFAULT-DIAMETER argument given by
SEGMENT-DIAMETER [microns, default 0.5]. If the global variable *NEXT-CELL-NAME* is non-NIL, then this will be used instead of CELL-NAME. Always sets
*NEXT-CELL-NAME* to NIL on exit. If NAME-SUFFIX is non-NIL [default given by global variable *CELL-NAME-SUFFIX*], it is automatically added as a
suffix to the name of a cell, even if this is supplied by *NEXT-CELL-NAME*."
  (when *clear-cell-name-maps*
    (element-parameter *cell* 'name-map nil)) ; A convenient place to put this. Also in PROCESS-CIRCUIT-STRUCTURE.
  (let* ((name (concatenate 'string
			    (string (or *next-cell-name* cell-name))
			    (when NAME-SUFFIX (string NAME-SUFFIX))))
	 (cell-type (create-celltype (or cell-type name)))
	 (new-name (if enable-automatic-cell-names		       
		     (check-cell-name name :automatic-name-fixing automatic-name-fixing)
		     name)))
    (when *next-cell-name*
      (format t "CREATE-CELL: Clearing the non-nil value of *NEXT-CELL-NAME*, which was ~A~%" *next-cell-name*))
    (setq *cell* 
	  (or (gethash new-name (CELL-HASH-TABLE))
	      (let* ((cell-def (case *circuit-source*
				 (:forms "Lisp forms")
				 (t (if *input-is-function* *circuit-function* *circuit-file*))))
		     (cell (make-cell :name new-name :origin (float-list (or cell-origin '(0.0 0.0 0.0)))
				      :type cell-type
				      :parameters (when cell-def (list (cons 'cell-definition cell-def))))))
		(setq *circuit-processed* nil)
		(push cell (cell-type-cells (cell-type cell)))
		(setf (gethash new-name (CELL-HASH-TABLE)) cell)
		(unless (equal new-name name) (setq *add-cell-name-to-segs* t))
		(when soma-diameter
		  (create-soma :cell name :diameter soma-diameter)
		  (when tree-list (create-tree *soma* tree-list :default-diameter segment-diameter)))
		cell)))))

(defun find-or-create-cell (cell)
  (or (unless (consp (element cell 'cell)) (element cell 'cell))
      (atomize-list (ELEMENT-W-STRING-OR-SYMBOL cell 'cell)) (create-cell cell)))



(defun edit-cell-type (&optional (cell-type *cell-type*))
  (let (dummy1 dummy2 dummy3 dummy4 dummy5 dummy6 dummy7 dummy8 dummy9 dummy10 dummy11 dummy12
	       cell-type-changed)
    (loop while (progn
		  (setq dummy1 (cell-type-cytoplasmic-resistivity cell-type)
			dummy2 (cell-type-membrane-resistivity cell-type)
			dummy3 (cell-type-soma-specific-capacitance cell-type)
			dummy4 (cell-type-soma-resistivity cell-type)
			dummy5 (cell-type-soma-shunt cell-type)
			dummy6 (cell-type-soma-v-leak cell-type)
			dummy7 (cell-type-dendrite-v-leak cell-type)
			dummy8 (cell-type-dendrite-specific-capacitance cell-type)
			dummy9 (cell-type-global-membrane-conductance-factor cell-type)
			dummy11 (or (element-parameter cell-type 'g-axial-computation) :single-leg))
		  (choose-variable-values
		   `((dummy1 "Cytoplasmic resistivity [ohm-cm]" :float)
		     (dummy2 "Dendrite membrane resistivity [ohm-cm-sq]" :float) 
		     (dummy7 "Dendrite leak battery [mV]" :float) 
		     (dummy8 "Dendrite specific capacitance [uF/cm-sq]" :float)
		     (dummy4 "Soma membrane resistivity [ohm-cm-sq]" :float) 
		     (dummy5 "Soma membrane shunt [ohms]" :float) 
		     (dummy6 "Soma leak battery [mV]" :float) 
		     (dummy3 "Soma specific capacitance [uF/cm-sq]" :float)
		     (dummy9 "Coefficient for all channel and synapse conductances" :float)
					;		     (dummy11 "Method for computing compartment coupling (g-axial):"
					;			      :choose (:single-leg :average-r :average-g))
		     (dummy10 "Edit ionic concentration/e-rev parameters" :boolean)
		     ,(when nil		; (cell-type-cells cell-type)
			'(dummy12 "Remove some cells of this type" :boolean)))
		   :label (format niL "Linear Parameters For Cell Type ~a" (cell-type-name cell-type)))
		  (or (<= dummy1 0)
		      (<= dummy2 0)
		      (<= dummy3 0)
		      (<= dummy4 0)
		      (<= dummy5 0)
		      (<= dummy8 0)
		      (<= dummy9 0))))
		      


		      
    (when dummy10 (edit-cell-type-ionic-parameters cell-type))
    (setq cell-type-changed
	  (or (not (eq dummy11 (or (element-parameter cell-type 'g-axial-computation) :single-leg)))	      
	      (not (and (= dummy1 (cell-type-cytoplasmic-resistivity cell-type))
			(= dummy2 (cell-type-membrane-resistivity cell-type))
			(= dummy3 (cell-type-soma-specific-capacitance cell-type))
			(= dummy8 (cell-type-dendrite-specific-capacitance cell-type))
			(= dummy4 (cell-type-soma-resistivity cell-type))
			(= dummy5 (cell-type-soma-shunt cell-type))
			(= dummy6 (cell-type-soma-v-leak cell-type))
			(= dummy7 (cell-type-dendrite-v-leak cell-type))
			(= dummy7 (cell-type-dendrite-v-leak cell-type))
			(= dummy9 (cell-type-global-membrane-conductance-factor cell-type))))))
    (element-parameter cell-type 'g-axial-computation dummy11)
    (clear-all-z-cable-in cell-type)	
    (setf (cell-type-cytoplasmic-resistivity cell-type) dummy1
	  (cell-type-membrane-resistivity cell-type) dummy2 
	  (cell-type-soma-resistivity cell-type) dummy4 
	  (cell-type-soma-shunt cell-type) dummy5 
	  (cell-type-soma-specific-capacitance cell-type) dummy3
	  (cell-type-dendrite-specific-capacitance cell-type) dummy8
	  (cell-type-soma-v-leak cell-type) dummy6
	  (cell-type-dendrite-v-leak cell-type) dummy7
	  (cell-type-global-membrane-conductance-factor cell-type) dummy9)
    (when dummy12
      (let (dummy1 dummy2)
	(choose-variable-values
	 `((dummy1 ,(format nil "Remove all ~a cell~:p of this type"
		     (length (cell-type-cells cell-type))) :boolean)
	   (dummy2 "Remove specific cells of this type" :boolean))
	 :label (format nil "Removing Cells of Type ~A" (cell-type-name cell-type)))
	(if dummy1
	    (erase-elements (cell-type-cells cell-type))
	    (when dummy2
	      (loop for cell in
		    (choose-list-values
		     (loop for cell in (cell-type-cells cell-type) collect (element-name cell))
		     nil :label (format nil "Choose Cells of Type ~A to Remove" (cell-type-name cell-type)))
		    do (erase-element cell))))))
    (when cell-type-changed
      (set-and-update-cell-type-linear-membrane cell-type))))


(defun set-and-update-cell-type-linear-membrane (&optional cell-type)
  (loop for cell-type in (or (coerce-to-list cell-type) (cell-types)) do
	(set-segments-membrane-parameters t cell-type)
	(set-somas-membrane-parameters t cell-type)
	(update-linear-z-in-cell-type cell-type)))


(defun menu-for-cell-types (&optional type)
  (loop for cell-type in
	(if type
	    (list (element type 'cell-type))
	    (things-of-names
	     (select-hash-values-menu (CELL-TYPE-HASH-TABLE) "Select Cell Types To Modify" :punt-if-only-one-entry t
				      :inclusion-key 'cell-type-cells)
	     'cell-type))
	do (edit-cell-type cell-type)))



(defun edit-cell-type-ionic-parameters (&optional type)
  (setq type (or type (car (cell-types))))
  (let ((dummy1 (cell-type-na-conc-intra type))	(dummy2 (cell-type-na-conc-extra type))
	(dummy3 (cell-type-na-conc-extra-dependence type)) (dummy21 (cell-type-e-na-dependence type))
	(dummy5 (cell-type-e-na type))

	(dummy6 (cell-type-k-conc-intra type)) (dummy7 (cell-type-k-conc-extra type))
	(dummy8 (cell-type-k-conc-extra-dependence type)) (dummy22 (cell-type-e-k-dependence type))
	(dummy10 (cell-type-e-k type))

	(dummy11 (cell-type-ca-conc-intra type)) (dummy12 (cell-type-ca-conc-extra type))
	(dummy13 (cell-type-ca-conc-extra-dependence type)) (dummy23 (cell-type-e-ca-dependence type))
	(dummy15 (cell-type-e-ca type))

	(dummy16 (cell-type-cl-conc-intra type)) (dummy17 (cell-type-cl-conc-extra type))
	(dummy18 (cell-type-cl-conc-extra-dependence type)) (dummy24 (cell-type-e-cl-dependence type))
	(dummy20 (cell-type-e-cl type))
	dummy25
	dummy26)
    (choose-variable-values
     '((dummy25 "Modify parameters for the following ions:" :x-choose (:na :k :ca :cl))
       (dummy26 "Edit global concentration/reversal potential parameters" :boolean))
     :label (format nil "Modifying Ionic Parameters for Cell Type ~A" (cell-type-name type)))
    (cond-every
     ((member :na dummy25)
      (choose-variable-values
       `((dummy1 "[Na+]in [mM]" :float)
	 (dummy2 "[Na+]out [mM]" :float)
	 (dummy3 ,(format nil "[Na+]out dependence for cell type ~A:" (cell-type-name type))
	  :choose (:fixed :follows-global) :vertical)
	 (dummy21 ,(format nil "E-Na dependence for cell type ~A:" (cell-type-name type))
	  :choose (:fixed :follows-global :follows-concentration) :vertical)
	 (dummy5 "Fixed value for Na+ Reversal Potential [mV]" :float)
	 (dummy26 "Edit global concentration/reversal potential parameters" :boolean))
       :text "Global values for reversal potentials and concentrations are set from another menu"
       :label (format niL "Ionic Parameters For Cell Type ~a" (cell-type-name type))))
     ((member :k dummy25)
      (choose-variable-values
       `((dummy6 "[K+]in [mM]" :float)
	 (dummy7 "[K+]out [mM]" :float)
	 (dummy8 ,(format nil "[K+]out dependence for cell type ~A:" (cell-type-name type))
	  :choose (:fixed :follows-global) :vertical)
	 (dummy22 ,(format nil "E-K dependence for cell type ~A:" (cell-type-name type))
	  :choose (:fixed :follows-global :follows-concentration) :vertical)
	 (dummy10 "Fixed value for K+ Reversal Potential [mV]" :float)
	 (dummy26 "Edit global concentration/reversal potential parameters" :boolean))
       :text "Global values for reversal potentials and concentrations are set from another menu"
       :label (format niL "Ionic Parameters For Cell Type ~a" (cell-type-name type))))
     ((member :ca dummy25)
      (choose-variable-values
       `((dummy11 "[Ca++]in [mM]" :float)
	 (dummy12 "[Ca++]out [mM]" :float)
	 (dummy13 ,(format nil "[Ca++]out dependence for cell type ~A:" (cell-type-name type))
	  :choose (:fixed :follows-global) :vertical)
	 (dummy23 ,(format nil "E-Ca dependence for cell type ~A:" (cell-type-name type))
	  :choose (:fixed :follows-global :follows-concentration) :vertical)
	 (dummy15 "Fixed value for Ca++ Reversal Potential [mV]" :float)
	 (dummy26 "Edit global concentration/reversal potential parameters" :boolean))
       :text "Global values for reversal potentials and concentrations are set from another menu"
       :label (format niL "Ionic Parameters For Cell Type ~a" (cell-type-name type))))
     ((member :cl dummy25)
      (choose-variable-values
       `((dummy16 "[Cl-]in [mM]" :float)
	 (dummy17 "[Cl-]out [mM]" :float)
	 (dummy18 ,(format nil "[Cl-]out dependence for cell type ~A:" (cell-type-name type))
	  :choose (:fixed :follows-global) :vertical)
	 (dummy24 ,(format nil "E-Cl dependence for cell type ~A:" (cell-type-name type))
	  :choose (:fixed :follows-global :follows-concentration) :vertical)
	 (dummy20 "Fixed value for Cl- Reversal Potential [mV]" :float)
	 (dummy26 "Edit global concentration/reversal potential parameters" :boolean))
       :text "Global values for reversal potentials and concentrations are set from another menu"
       :label (format niL "Ionic Parameters For Cell Type ~a" (cell-type-name type)))))
    (when dummy26 (MENU-FOR-CONCENTRATIONS))
    (setf
     (cell-type-e-na-dependence type) dummy21
     (cell-type-e-k-dependence type) dummy22
     (cell-type-e-ca-dependence type) dummy23
     (cell-type-e-cl-dependence type) dummy24		
     
     (cell-type-na-conc-intra type) dummy1
     (cell-type-na-conc-extra type) dummy2
     (cell-type-na-conc-extra-dependence type) dummy3
     (cell-type-e-na type) dummy5

     (cell-type-k-conc-intra type) dummy6
     (cell-type-k-conc-extra type) dummy7
     (cell-type-k-conc-extra-dependence type) dummy8
     (cell-type-e-k type) dummy10

     (cell-type-ca-conc-intra type) dummy11
     (cell-type-ca-conc-extra type) dummy12
     (cell-type-ca-conc-extra-dependence type) dummy13
     (cell-type-e-ca type) dummy15

     (cell-type-cl-conc-intra type) dummy16
     (cell-type-cl-conc-extra type) dummy17
     (cell-type-cl-conc-extra-dependence type) dummy18
     (cell-type-e-cl type) dummy20)    
    (update-cell-type-ionic-parameters type)))



(defun UPDATE-CELL-AREA (cell)
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (element-parameter cell 'number-cell-distal-segments (count-cell-distal-segments cell))
  (element-parameter cell 'number-cell-branch-points (count-branch-points cell))
  (element-parameter cell 'total-area (+ (if (soma-segments cell) 0.0 (element-area (cell-soma cell)))
					 (element-area (cell-segments cell)))))


(defun menu-to-move-cells (&optional cell histology-win)
  (loop for cell in
	(if cell
	    (list (element cell 'cell))
	  (choose-list-values-from-keys
	   (loop for cell being the hash-value of (CELL-HASH-TABLE)
		 collect (list (format nil "~a @ ~a" (cell-name cell) (element-absolute-location cell)) cell))
	   nil :text "Cell names are followed by the XYZ soma coordinates." :label "Select cells to move"))
	do
	(let ((dummy1 (first (cell-origin cell)))
	      (dummy2 (second (cell-origin cell)))
	      (dummy3 (third (cell-origin cell)))
	      dummy4)
	  (choose-variable-values
	   `((dummy1 "Soma X coordinate" :Float)
	     (dummy2 "Soma Y coordinate" :Float)
	     (dummy3 "Soma Z coordinate" :Float)
	     ,(when histology-win
		(list 'dummy4 (format nil "Redraw histology in window ~a" (g-value histology-win :title)) :boolean)))
	   :Label (format nil "Move coordinates of cell ~A" (element-name cell)))
	  (move-cell cell (list dummy1 dummy2 dummy3))
	  (when dummy4
	    (let ((*automatic-run* t))
	      (drawing-menu histology-win t nil t))))))

(defun warp-cell (cell &key (x-factor 1.0) (y-factor 1.0) (z-factor 1.0))
  (loop for node being the hash-value of (NODE-HASH-TABLE)
	when (eq cell (node-cell node))
	do (setf (node-relative-location node)
		 (mapcar '*
			 (node-relative-location node)
			 (list x-factor y-factor z-factor))))
  (process-circuit-structure t))

(defun shift-element-position (element shift)
  (let ((node (element-physical-node element)))
    (when node
      (setf (node-relative-location node)
	    (mapcar '+ (node-relative-location node) shift))
      (setf (node-absolute-location node)
	    (mapcar '+ (node-absolute-location node) shift))

      (when (and (segment-p (element element))
		 (segment-dummy-proximal-node-location (element element)))
	(setf (segment-dummy-proximal-node-location (element element))
	      (mapcar '+ (segment-dummy-proximal-node-location (element element)) shift))))))


(defun zero-soma-position (&optional cell)
  (let ((cell (or cell *cell*)))
    (when cell
      (let ((shift (mapcar '- (node-relative-location (soma-node (cell-soma cell))))))
	(loop for seg in (cell-segments cell) do (shift-element-position seg shift))
	(shift-element-position (cell-soma cell) shift)
	(setf (cell-origin cell) (mapcar '+ (cell-origin cell) shift)))
      (maphash 'set-cells-coordinate-extrema (NODE-HASH-TABLE)))))


(defun move-cell (cell new-origin)
  "Moves the absolute location of CELL to the XYZ coordinates [microns] given in the numeric list NEW-ORIGIN."
  (let ((cell (element-cell cell)))
    (setf (cell-origin cell) (sequence-to-float-list new-origin))
    (locate-cell-nodes cell)
    (maphash 'set-cells-coordinate-extrema (NODE-HASH-TABLE))))

(defun shift-cell (cell &key (x-shift 0.0) (y-shift 0.0) (z-shift 0.0))
  "Moves the relative location of CELL according to X-SHIFT, Y-SHIFT and Z-SHIFT [microns, default 0]." 
  (let ((cell (element-cell cell)))
    (setf (cell-origin cell)
	  (mapcar '+ (cell-origin cell)
		  (sequence-to-float-list (list x-shift y-shift z-shift))))
    (locate-cell-nodes nil cell)
    (maphash 'set-cells-coordinate-extrema (NODE-HASH-TABLE))))

(defun synapses-of-node (node type)
  (let ((type (element type 'synapse-type)))
    (loop for elt in (node-elements node)
	  when (and (synapse-p elt) (eq (synapse-type elt) type))
	  collect elt)))

(defun cell-synapses (cell type)
  (let ((cell (element cell 'cell))
	(type (element type 'synapse-type)))
    (nconc
     (synapses-of-node (soma-node (cell-soma cell)) type)
     (loop for seg in (cell-segments cell)
	   nconc (synapses-of-node (segment-node-2 seg) type)))))

(defun channels-of-node (node type)
  (let ((type (element type 'channel-type)))
    (loop for elt in (node-elements node)
	  when (and (channel-p elt) (eq (channel-type elt) type))
	  collect elt)))


(defun cell-channels (cell type)
  (let ((cell (element cell 'cell))
	(type (element type 'channel-type)))
    (nconc
     (channels-of-node (soma-node (cell-soma cell)) type)
     (loop for seg in (cell-segments cell)
	   nconc (channels-of-node (segment-node-2 seg) type)))))


#|
(defun cell-element-p (element)
  "True if ELEMENT is a soma or segment."
  (let ((elt (element element)))
    (or (soma-p elt)
	(segment-p elt))))
|#

(defun cell-element-p (element)
  "True if ELEMENT is a soma or segment."
  (or (soma-p element)
      (segment-p element)
      (typecase (element element)
	((or soma segment) t))))







