;;; -*- mode: lisp; Syntax: Common-lisp; package: #-parallel surf #+parallel *surf; base: 10 ; -*-
;;; (c) Copyright 1988, Don Webber, Thinking Machines Corporation, Inc.
;;; alterations (c) Copyright 1990, Lyle Borg-Graham, MIT Center for Biological Information Processing
;;; File creation date: 1/25/88 17:58:12
;
; the soma model, part of the neuron model
;


#-parallel
(in-package "SURF-HIPPO"
	    :use '("COMMON-LISP-USER" "COMMON-LISP")
	    :nicknames '("SURF"))
#+parallel 
(in-package "*SURF")


(defstruct soma 
  "Model for a soma"
  (name ""		)   
  node
  core-soma
  cell
  model
  (params nil)
  (diameter zero :type single-float)		;microns
  (g-shunt zero :type single-float)	;microsiemans
  (g-leak zero :type single-float)	;microsiemans
  (v-leak zero :type single-float)	
  (capacitance zero :type single-float)	;nanofarad
  (conc-int nil)	; concentration integrator
)



#-parallel
(defstruct core-soma
  "Core model for a soma."
  (node-pointp nil	)	; zero if constant, one if node
  node-point
  (node-const zero	)
  (current 0.0 :type single-float)
  (g-shunt zero :type single-float)
  (g-leak zero :type single-float)
  (v-leak zero :type single-float)
  (capacitance zero :type single-float)
)

(defun create-soma-model ()
  "Creates a template for all somas."
  (let ((template (make-model-template)))
    (setf
      (model-template-name template) (string "soma")
      (model-template-default-params template) '((g-shunt . 0.0) (g-leak . 0.0)
						 (v-leak . -70.0) (capacitance . 0.0))
      (model-template-eval-routine template) #'eval-soma
      (model-template-print-routine template) #'print-soma
      (model-template-create-routine template) #'create-soma
      (model-template-create-core-routine template) #'create-core-soma
      (model-template-add-off-diag-routine template) #'add-off-diag-soma
      (model-template-find-coupling-routine template) #'find-coupling-soma
      (model-template-fix-dc-nodes-routine template) #'soma-fix-dc-nodes
      *models* (cons template *models*)
      (gethash (string "soma") *model-hash-table*) template
      soma-hash-table (make-hash-table :test #'equal))
    (create-model-instance (string "soma") (model-template-name template) '() )))
	; only need one soma model instance, so create it now.

(defun print-soma (soma)
  "Prints out this data associated with a soma."
  (format *output-stream "Soma ~a in cell ~a: diam= ~4fuM, g-s=~,2e uS g-l=~,2e uS v-l=~2d mV cap=~,2e nF~%"
	  (soma-name soma)
	  (cell-name (soma-cell soma))
	  (soma-diameter soma)
	  (if (> (soma-g-shunt soma) 1e-6)  (soma-g-shunt soma) 0)
	  (soma-g-leak soma)
	  (soma-v-leak soma)
	  (soma-capacitance soma)))

;; CREATE-SOMA Diameter is in microns.
(defun create-soma (soma-name cell-name diameter &key (model-ca-variation nil) (plot-pane 1)
			      (parameters '()))
  "Creates a element of type soma. Inputs 'name' and 'node' are strings,
   'model-name' is the name of the model-instance, and 'parameters' is an a-list."
  (if
   (gethash soma-name soma-hash-table)
   (sim-warning (format nil "create-soma: soma ~a  already defined, ignoring"
			soma-name))
   (let ((n1 (create-node soma-name :cell-name cell-name :plot-pane plot-pane :hines-circuit-node t))
	 (model (gethash "soma" *model-instance-hash-table*))
	 (cell (gethash cell-name cell-hash-table))
;        (area (* pi-single  diameter diameter 1e-8 ))	;cm squared
	 )
     (if (not cell) (setq cell (create-cell cell-name)))
     (if (not (or (node-is-dc-source n1)
		  (node-is-pwl-source n1)))
	 (let ((soma (make-soma
		      :name soma-name 
		      :cell cell :diameter diameter
		      :node n1
		      :model model
		      :v-leak (set-create-parameters 'v-leak model parameters)
		      )))
	   (setf
	    (node-elements n1) (cons soma (node-elements n1))
	    (gethash soma-name soma-hash-table) soma
	    (model-instance-elements model) (cons soma (model-instance-elements model)))
	   (update-cell-name-list cell-name)
	   (if model-ca-variation
	       (progn (setf (soma-conc-int soma)
			    (create-conc-int (format nil "~a-conc-int" soma-name) soma))
		      (setf (node-elements n1)
			    (cons (soma-conc-int soma) (node-elements n1)))))
	   soma)))))


(defun set-somas-membrane-parameters ()
  (maphash 'set-soma-membrane-parameters soma-hash-table))

;;; SET-SOMA-MEMBRANE-PARAMETERS
;; Set soma membrane properties, according to the membrane parameters for the appropriate cell type.
(defun set-soma-membrane-parameters (name soma)
  (declare (ignore name))
  ;;area is in cm-sq, resistivity is in ohm-cm-sq, conductance is in microS.
  (let* ((cell-type (cell-type (soma-cell soma)))
	 (diameter (soma-diameter soma))
	 (area (* pi-single  diameter diameter 1e-8)))	;cm squared
    (setf (soma-g-leak soma)
	  (* 1e6 (/ area (cell-type-soma-resistivity cell-type)))
	  (soma-g-shunt soma)
;	  (* 1e6 (/ area (cell-type-soma-shunt-resistivity cell-type)))
	  (/ 1e6 (cell-type-soma-shunt cell-type))
	  ;;specific capacitance is in microF/cm-sq, capacitance is in nF
	  (soma-capacitance soma)
	  (* 1e3 area (cell-type-specific-capacitance cell-type)))))

; This function creates a core soma data structure. In the parallel version, it 
; puts the device on the same processor as the fanout node, which is passed in as the 
; variable 'proc. The serial version ignores this and allocates a struct normally.
; This function only works if the 'nd argument is not a DC source node, because these
; do not have processors allocated to them.

(defun create-core-soma (soma nd)
  "Creates the core struct for a soma."
  (let (core-soma
	(proc #-parallel nil #+parallel (allocate-processor)))
   #-parallel (declare (ignore proc))
    #+parallel (*setf (pref fanout-valid proc) t)
    #+parallel (*setf (pref fanout-seg-forward proc) nil)
    (if (soma-core-soma soma)			; 
	(setf core-soma (soma-core-soma soma))	; core soma has already been allocated
	(progn						; else allocate one
	  #-parallel (setf core-soma (make-core-soma))
	  #+parallel (setf core-soma proc)
	  #+parallel (*setf (pref *lisp-struct-type core-soma) core-soma)
	  (#+parallel *setf #-parallel setf	 
	    (#+parallel pref #.core-soma-g-shunt core-soma) (soma-g-shunt soma)
	    (#+parallel pref #.core-soma-g-leak core-soma) (soma-g-leak soma)
	    (#+parallel pref #.core-soma-v-leak core-soma) (soma-v-leak soma)
	    (#+parallel pref #.core-soma-capacitance core-soma) (soma-capacitance soma))
	  (setf (soma-core-soma soma) core-soma)))
    (let ((node1 (soma-node soma)))
      (cond
	((eq nd node1)
	 #-parallel
	 (setf
	   (#.core-soma-node-pointp core-soma) t
	   (#.core-soma-node-point core-soma) (node-core-nd node1))
	 #+parallel
	 (*setf
	   (pref #.core-soma-node-pointp core-soma) t
	   (pref #.core-soma-node-point core-soma) proc))
	(t
	 (sim-error "Internal error: called create-core on a device with a invalid node"))
	))))

(defun add-off-diag-soma (soma diag off-diag off-diag-entry)
  (declare (ignore soma diag off-diag off-diag-entry)))

(defun find-coupling-soma (nd soma)
  "Returns a pair of (node . value) telling how coupled that node is to nd."
  (declare (ignore nd soma ))
  nil)

(defun soma-fix-dc-nodes (soma)
  "Fix up the nodes of this element which are connected to dc nodes."
  (if (soma-core-soma soma)
      (progn
	(if (node-is-dc-source (soma-node soma))
	    (#+parallel *setf #-parallel setf	 
	      (#+parallel pref #.core-soma-node-pointp (soma-core-soma soma)) nil
	      (#+parallel pref #.core-soma-node-const (soma-core-soma soma)) (node-voltage (soma-node soma))))
	)))

#-parallel
(defun get-soma-voltage (soma)
  (if (#.core-soma-node-pointp soma)
      (if (not *use-hines*)
	  (core-node-voltage-n+1 (#.core-soma-node-point soma))
	  (core-node-voltage-n (#.core-soma-node-point soma)))
      (#.core-soma-node-const soma)))

#-parallel
(defun eval-soma (soma)
  (let ((voltage 0.0)(current 0.0)(charge 0.0)(jacobian 0.0)
	(core-soma (soma-core-soma soma)))
    (declare (single-float voltage current charge jacobian))
    (if (null core-soma) (return-from eval-soma (values)))
    (setf voltage (get-soma-voltage core-soma))	; get the voltages

    (setf current (if (not *use-hines*)
		      (+ (* (#.core-soma-g-shunt core-soma) voltage)
			 (* (#.core-soma-g-leak core-soma) (- voltage (#.core-soma-v-leak core-soma))))
		      (*  (#.core-soma-g-leak core-soma) (- (#.core-soma-v-leak core-soma)))))
    (setf charge (* (#.core-soma-capacitance core-soma) voltage))
    (setf jacobian (+ (#.core-soma-g-shunt core-soma)
		      (#.core-soma-g-leak core-soma)
		      (* alpha (#.core-soma-capacitance core-soma))))
    ;; Send the values back where they go

    (setf
     (core-node-current (#.core-soma-node-point core-soma))
     (+ (core-node-current (#.core-soma-node-point core-soma))
	current)
     (core-node-charge (#.core-soma-node-point core-soma))
     (+ (core-node-charge (#.core-soma-node-point core-soma))
	charge)
     (core-node-alpha-charge (#.core-soma-node-point core-soma))
     (+ (core-node-alpha-charge (#.core-soma-node-point core-soma))
	(* alpha charge))
     (core-node-jacobian (#.core-soma-node-point core-soma))
     (+ (core-node-jacobian (#.core-soma-node-point core-soma))
	jacobian))))

#+parallel
(*defun get-soma-voltage (v1)
  (cond!!
    (#.core-soma-node-pointp v1)
    (t!! #.core-soma-node-const)))

#+parallel
(defun eval-soma ()
  (*select-type (core-soma)
    (*let
      ((voltage (!! 0)))
      (declare (type (pvar big-float) voltage))
      ; get the voltages

      (*set voltage (get-soma-voltage #.core-soma-node-voltage))
    
      ; calculate the current, etc.
      (*set #.core-soma-current
	    (+!! (*!! #.core-soma-g-shunt voltage)
		 (*!! #.core-soma-g-leak (-!! voltage #.core-soma-v-leak))))
      (*set #.core-soma-charge
	    (*!! #.core-soma-capacitance voltage))
      (*set #.core-soma-jacobian
	    (+!! #.core-soma-g-shunt
		 #.core-soma-g-leak
		 (*!! (!! alpha) #.core-soma-capacitance)))
      )))
