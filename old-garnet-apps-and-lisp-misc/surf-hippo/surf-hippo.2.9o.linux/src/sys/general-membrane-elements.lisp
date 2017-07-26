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


;;; SYS Source file: general-membrane-elements.lisp
					
;; Functions that are applied to both channels and synapses.


(in-package "SURF-HIPPO")




(defun pore-block-p (pore)
  "Predicate for the block of a channel or synapse PORE, either due to an individual block or block of
the associated type."
  (typecase pore
    (channel (or (channel-block pore) (channel-type-block (element-type pore))))
    (synapse (or (synapse-block pore) (synapse-type-block (element-type pore))))))

(defun element-has-intracellular-conc-ints (element)
  (loop for conc-int-type-param in (element-conc-int-type-params (element-type element))
	when (and (conc-int-type-intra-p (element (car conc-int-type-param)))
		  (not (= 0 (element-type-conc-int-type-param-permeability conc-int-type-param))))
	do (return t)))

(defun element-has-extracellular-conc-ints (element)
  (loop for conc-int-type-param in (element-conc-int-type-params (element-type element))
	when (and (not (conc-int-type-intra-p (element (car conc-int-type-param))))
		  (not (= 0 (element-type-conc-int-type-param-permeability conc-int-type-param))))
	do (return t)))

(proclaim '(inline get-element-conc-ints-conc-in))
(defun get-element-conc-ints-conc-in (elt conc-ints-params &optional fast-conc-in-calculation)
  (declare (optimize (safety 1) (speed 3) (space 0) (compilation-speed 0)))
  (if (and fast-conc-in-calculation conc-ints-params)
      (let ((conc-int-param (car conc-ints-params)))
	(* (element-conc-int-param-permeability conc-int-param) ; Relevant permeability for this element
	   (case (element-conc-int-param-shell conc-int-param)
	     (1 (the df (conc-int-shell-1-free-conc-n (element-conc-int-param-cint conc-int-param))))
	     (2 (the df (conc-int-shell-2-free-conc-n (element-conc-int-param-cint conc-int-param))))
	     (t 0.0d0))))
    (loop for conc-int-param in conc-ints-params
	  when (conc-int-type-intra-p (conc-int-type (element-conc-int-param-cint conc-int-param)))
	  summing (* (element-conc-int-param-permeability conc-int-param) ; Relevant permeability for this element
		     (case (element-conc-int-param-shell conc-int-param)
		       (1 (the df (conc-int-shell-1-free-conc-n (element-conc-int-param-cint conc-int-param))))
		       (2 (the df (conc-int-shell-2-free-conc-n (element-conc-int-param-cint conc-int-param))))
		       (t 0.0d0)))
	  into result double-float
	  finally (return (if (> result 0)
			      result
			    (element-parameter (element-type elt) 'effective-default-intracellular-concentration))))))


(proclaim '(inline get-element-conc-ints-conc-out))
(defun get-element-conc-ints-conc-out (elt conc-ints-params &optional fast-conc-out-calculation)
  (declare (optimize (safety 1) (speed 3) (space 0) (compilation-speed 0)))
  (if (and fast-conc-out-calculation conc-ints-params)
      (let ((conc-int-param (car conc-ints-params)))
	(* (element-conc-int-param-permeability conc-int-param) ; Relevant permeability for this element
	   (case (element-conc-int-param-shell conc-int-param)
	     (1 (the df (conc-int-shell-1-free-conc-n (element-conc-int-param-cint conc-int-param))))
	     (2 (the df (conc-int-shell-2-free-conc-n (element-conc-int-param-cint conc-int-param))))
	     (t 0.0d0))))
    (loop for conc-int-param in conc-ints-params
	  unless (conc-int-type-intra-p (conc-int-type (element-conc-int-param-cint conc-int-param)))
	  summing (* (element-conc-int-param-permeability conc-int-param) ; Relevant permeability for this element
		     (case (element-conc-int-param-shell conc-int-param)
		       (1 (the df (conc-int-shell-1-free-conc-n (element-conc-int-param-cint conc-int-param))))
		       (2 (the df (conc-int-shell-2-free-conc-n (element-conc-int-param-cint conc-int-param))))
		       (t 0.0d0)))
	  into result double-float
	  finally
	  (return (if (> result 0)
		      result
		    (element-parameter (element-type elt) 'effective-default-extracellular-concentration))))))


(defun element-iv-parameters-structure (elt)
  (let ((type (element-type elt)))
    (typecase type
      (channel-type (channel-type-iv-parameters type))
      (synapse-type (synapse-type-iv-parameters type)))))


;; MASSAGE-CONC-INT-TYPE-PARAMS For both channel and synapse types. Result goes into the
;; :CONC-INT-TYPE-PARAMS slot.
(defun massage-conc-int-type-params (CONC-INT-TYPE-PARAMS)
  (loop for conc-int-type-param in CONC-INT-TYPE-PARAMS
	collect (cons (car conc-int-type-param)	; Conc-int-type symbol
		      (loop for shell-param in (cdr conc-int-type-param)
			    collect (list (car shell-param) ; Shell 1 or 2
					  (coerce-to-double (cadr shell-param))	; Current proportion
					  )))))


(defun update-membrane-element-type-params (type)
  (update-membrane-element-type-effective-concentrations type)
  (update-membrane-element-type-static-v-dep type))


(defun update-membrane-element-type-effective-concentrations (type)
  (set-element-param type 'effective-default-valence (s-flt (effective-default-valence type)))
  (element-parameter type 'effective-default-valence-fn (round (element-parameter type 'effective-default-valence)))
  (element-parameter type 'intracellular-conc-ints (element-has-intracellular-conc-ints type))
  (element-parameter type 'extracellular-conc-ints (element-has-extracellular-conc-ints type))
  (set-element-param type 'effective-default-intracellular-concentration
		     (d-flt (effective-default-intracellular-concentration type)))
  (set-element-param type 'effective-default-extracellular-concentration
		     (d-flt (effective-default-extracellular-concentration type))))

(defun update-membrane-element-type-static-v-dep (type)
  (let ((static-v-dep-fun (element-parameter type 'static-voltage-dependence-function)))
    (when static-v-dep-fun
      (element-parameter
       type 'static-voltage-dependence
       (sequence-to-float-array
	(loop for voltage single-float from *particle-look-up-table-min-voltage*
	      by (the sf *particle-look-up-table-precision*)
	      for array-index fixnum from 0 to (1- (the fn *particle-look-up-table-length*))
	      collect (coerce-to-single (funcall static-v-dep-fun voltage type)))))))
  nil)

(defun clear-conductance-type-extra-parameters (type)
  (element-parameter type 'gbar-modulation nil)
  (element-parameter type 'conductance-function nil)
  (element-parameter type 'static-voltage-dependence-function nil)
  (element-parameter type 'static-voltage-dependence nil))

  
;; EXTRACT-CONDUCTANCE-TYPE-PARAMETERS Common parameter extraction from TYPE-PARAMETERS for both channel and synapse types.
(defun extract-conductance-type-parameters (type type-parameters)
  (clear-conductance-type-extra-parameters type)
  (let ((iv-parameters-structure (typecase type
				   (channel-type (channel-type-iv-parameters type))
				   (synapse-type (synapse-type-iv-parameters type)))))
    (cond-every
     ((assoc 'gbar-modulation type-parameters)
      (element-parameter type 'gbar-modulation (get-a-value 'gbar-modulation type-parameters)))
     ((or (assoc 'e-rev type-parameters) (assoc 'use-defined-e-rev type-parameters))
      (setf (membrane-element-iv-parameters-use-defined-e-rev iv-parameters-structure)
	    (or (not (assoc 'use-defined-e-rev type-parameters))
		(cdr-assoc 'use-defined-e-rev type-parameters))))
     ((assoc 'e-rev type-parameters)
      (setf (membrane-element-iv-parameters-e-rev iv-parameters-structure)
	    (coerce-to-single (get-a-value 'e-rev type-parameters))))
     ((or (assoc 'ion-permeabilities type-parameters)
	  (assoc 'ion-perms type-parameters))
      (setf (membrane-element-iv-parameters-ion-permeabilities iv-parameters-structure)
	    (let ((ion-perms (or (get-a-value 'ion-permeabilities type-parameters)
				 (get-a-value 'ion-perms type-parameters))))
	      (typecase ion-perms
		(cons (loop for ion-perm in ion-perms collect (list (car ion-perm) (float (cadr ion-perm)))))
		(t (list (list ion-perms 1.0)))))))

     ((or (assoc 'gbar-source type-parameters) (assoc 'permeability-source type-parameters))
      (setf (membrane-element-iv-parameters-gbar-source iv-parameters-structure)
	    (or (get-a-value 'gbar-source type-parameters) (get-a-value 'permeability-source type-parameters))))

     ((or (assoc 'gbar type-parameters) (assoc 'gbar-ref type-parameters) (assoc 'permeability type-parameters))
      (setf (membrane-element-iv-parameters-gbar-source iv-parameters-structure) :absolute)
      (setf (membrane-element-iv-parameters-gbar-ref iv-parameters-structure)
	    (coerce-to-single (or (get-a-value 'gbar type-parameters)
				  (get-a-value 'gbar-ref type-parameters)
				  (get-a-value 'permeability type-parameters)))))
   
     ((or (assoc 'gbar-density type-parameters) (assoc 'permeability-density type-parameters))
      (setf (membrane-element-iv-parameters-gbar-source iv-parameters-structure) :density)
      (setf (membrane-element-iv-parameters-gbar-density iv-parameters-structure)
	    (coerce-to-single
	     (or (get-a-value 'gbar-density type-parameters) (get-a-value 'permeability-density type-parameters)))))
				    
     ((assoc 'reference-temp type-parameters)
      (setf (membrane-element-iv-parameters-reference-temp iv-parameters-structure)
	    (coerce-to-single (get-a-value 'reference-temp type-parameters))))

     ((or (assoc 'qten type-parameters) (assoc 'q10 type-parameters))
      (setf (membrane-element-iv-parameters-q10 iv-parameters-structure)
	    (s-flt (or (get-a-value 'qten type-parameters) (get-a-value 'q10 type-parameters)))))

     ((assoc 'conductance-function type-parameters)
      (element-parameter type 'conductance-function (get-a-value 'conductance-function type-parameters)))

     ((assoc 'static-voltage-dependence-function type-parameters)
      (element-parameter type 'static-voltage-dependence-function
			 (get-a-value 'static-voltage-dependence-function type-parameters)))
     ((assoc 'static-voltage-dependence type-parameters)
      (element-parameter type 'static-voltage-dependence
			 (sequence-to-float-array
			  (let ((funspec (get-a-value 'static-voltage-dependence type-parameters)))
			    (apply (car funspec) (cdr funspec))))))
			     

     ((get-a-value 'iv-relation type-parameters)
      (setf (membrane-element-iv-parameters-iv-relation iv-parameters-structure)
	    (get-a-value 'iv-relation type-parameters))))
  
    (when (and (eq (membrane-element-iv-parameters-iv-relation iv-parameters-structure) :CONSTANT-FIELD)
	       (not (element-conc-int-type-params type))
	       (not (membrane-element-iv-parameters-ion-permeabilities iv-parameters-structure)))
      (sim-error (format nil "~A requires an 'ionic-perms term" (element-name type))))
  
    (setf (membrane-element-iv-parameters-variable-e-rev iv-parameters-structure)
	  (and (element-conc-int-type-params type)
	       (not (eq (membrane-element-iv-parameters-iv-relation iv-parameters-structure) :CONSTANT-FIELD))
	       (or (get-a-value 'use-variable-e-rev type-parameters)
		   (not (membrane-element-iv-parameters-use-defined-e-rev iv-parameters-structure)))))

    (unless (or (and (> (membrane-element-iv-parameters-gbar-density iv-parameters-structure) 0)
		     (eq :density (membrane-element-iv-parameters-gbar-source iv-parameters-structure)))
		(and (membrane-element-iv-parameters-gbar-ref iv-parameters-structure)
		     (eq :absolute (membrane-element-iv-parameters-gbar-source iv-parameters-structure))))
      (setf (membrane-element-iv-parameters-gbar-source iv-parameters-structure)
	    (if (> (membrane-element-iv-parameters-gbar-density iv-parameters-structure) 0)
		:density :absolute)))
    (unless (or (membrane-element-iv-parameters-use-defined-e-rev iv-parameters-structure)
		(membrane-element-iv-parameters-ion-permeabilities iv-parameters-structure))
      (sim-error (format nil "~a must have 'ion-perms specified!" type)))))  



(defun print-general-element-stuff (type)
  (when (element-parameter type 'parent-type)
    (format t " Parameters inherited from type ~a~%" (element-parameter type 'parent-type))))

(defun print-membrane-element-iv-parameters (type)
  (let* ((iv-parameters (typecase type
			  (channel-type (channel-type-iv-parameters type))
			  (synapse-type (synapse-type-iv-parameters type))))
	 (constant-field-p (eq :CONSTANT-FIELD (membrane-element-iv-parameters-iv-relation iv-parameters))))
    (if constant-field-p
	(case (membrane-element-iv-parameters-gbar-source iv-parameters)
	  (:density
	   (format t
		   "Permeability density ~,2e 1.0e-6cm3/s/um^2, "
		   (membrane-element-iv-parameters-gbar-density iv-parameters)))
	  (:absolute
	   (format t
		   "Absolute Permeability ~,2e cm3/s, "
		   (membrane-element-iv-parameters-gbar-ref iv-parameters))))
	(case (membrane-element-iv-parameters-gbar-source iv-parameters)
	  (:density
	   (format t "Gbar density ~,2e pS/um^2, "
		   (membrane-element-iv-parameters-gbar-density iv-parameters)))
	  (:absolute
	   (format t  "Absolute gbar ~,2e uS, "
		   (membrane-element-iv-parameters-gbar-ref iv-parameters)))))
    (format t  "(~A Q10 ~a, T_ref ~a)~%"
	    (if constant-field-p "gbar" "perm")
	    (my-float-format (membrane-element-iv-parameters-q10 iv-parameters))
	    (my-float-format (membrane-element-iv-parameters-reference-temp iv-parameters)))
    (unless (= (or (element-parameter type 'gbar-modulation) 1.0) 1.0)
      (format t "    GBAR modulation by ~,2f~%" (element-parameter type 'gbar-modulation)))
    (if constant-field-p
	(format t "    Constant Field Permeability Model~%")
        (if (membrane-element-iv-parameters-use-defined-e-rev iv-parameters)
	    (format t "    Fixed E-rev ~a mV~%" (current-element-type-e-rev type) )
	    (format t "    E-rev: ~,1f mV [set w/concentrations according to ion-permeabilites]~%"
	     (current-element-type-e-rev type))))
    (when (membrane-element-iv-parameters-variable-e-rev iv-parameters)
      (format t "    Variable E-rev enabled~%"))
    (when (membrane-element-iv-parameters-ion-permeabilities iv-parameters)
      (format t  "    Ion Permeabilities:")
      (loop for ion-perm in (membrane-element-iv-parameters-ion-permeabilities iv-parameters) do
	    (format t  " ~,1f% ~a" (* 100 (cadr ion-perm)) (car ion-perm)))
      (format t  "~%"))))



(defun document-iv-parameters (type)
  (let* ((iv-parameters (typecase type
			  (channel-type (channel-type-iv-parameters type))
			  (synapse-type (synapse-type-iv-parameters type))))
	 (iv-relation (membrane-element-iv-parameters-iv-relation iv-parameters)))
    (format t "        (iv-relation . ~s)~%" iv-relation)
    (case iv-relation
      (:constant-field
       (format t "        (permeability-source . ~s)~%" (membrane-element-iv-parameters-gbar-source iv-parameters))
       (case (membrane-element-iv-parameters-gbar-source iv-parameters)
	 (:ABSOLUTE (format t "        (permeability . ~a)~%"
			    (membrane-element-iv-parameters-gbar-ref iv-parameters)))
	 (t         (format t "        (permeability-density . ~a)~%"
			    (membrane-element-iv-parameters-gbar-density iv-parameters)))))
      (t
       (format t "        (gbar-source . ~s)~%" (membrane-element-iv-parameters-gbar-source iv-parameters))
       (case (membrane-element-iv-parameters-gbar-source iv-parameters)
	 (:ABSOLUTE (format t "        (gbar-ref . ~a)~%"
			    (membrane-element-iv-parameters-gbar-ref iv-parameters)))
	 (t         (format t "        (gbar-density . ~a)~%"
			    (membrane-element-iv-parameters-gbar-density iv-parameters))))))
    (when (and (element-parameter type 'gbar-modulation)
	       (not (= 1.0 (element-parameter type 'gbar-modulation))))
      (format t "        (gbar-modulation . ~a)~%" (element-parameter type 'gbar-modulation)))
    (when (membrane-element-iv-parameters-use-defined-e-rev iv-parameters)
      (format t "        (e-rev . ~a)~%" (membrane-element-iv-parameters-e-rev iv-parameters)))

    (format t "        (use-defined-e-rev . ~a)~%" (membrane-element-iv-parameters-use-defined-e-rev iv-parameters))
    (format t "        (ion-permeabilities . ~a)~%" (membrane-element-iv-parameters-ion-permeabilities iv-parameters))
    (format t "        (q10 . ~a)~%" (membrane-element-iv-parameters-q10 iv-parameters))
    (format t "        (reference-temp . ~a)~%" (membrane-element-iv-parameters-reference-temp iv-parameters))
    
    (cond-every
     ((membrane-element-iv-parameters-conc-int-type-params iv-parameters)
      (format t "        (conc-int-type-params . ~A)~%"
	      (membrane-element-iv-parameters-conc-int-type-params iv-parameters)))
     ((element-parameter type 'conductance-function)
      (format t "        (conductance-function . ~s)~%" (element-parameter type 'conductance-function))))
    (cond ((element-parameter type 'static-voltage-dependence-function)
	   (format t "        (static-voltage-dependence-function . ~s)~%"
		   (element-parameter type 'static-voltage-dependence-function)))
	  ((element-parameter type 'static-voltage-dependence)
	   (format t "        (static-voltage-dependence . ~s)~%" (element-parameter type 'static-voltage-dependence))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; REVERSAL POTENTIAL FUNCTIONS
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun current-element-type-e-rev (elt &optional cell-type element-type)
  (declare (optimize (safety 0) (speed 3) (space 0) (compilation-speed 0)))
  (let* ((element-type (or element-type (element-type elt)))
	 (iv-parameters-structure (element-iv-parameters-structure element-type)) ; Gets the params from the elt type.
	 ;; (cell-type (or (element cell-type 'cell-type) (and (element-cell elt) (cell-type (element-cell elt)))))
	 (ion-perms (membrane-element-iv-parameters-ion-permeabilities iv-parameters-structure)))
    (if (or (membrane-element-iv-parameters-use-defined-e-rev iv-parameters-structure)
	    (not (membrane-element-iv-parameters-ion-permeabilities iv-parameters-structure)))
	(membrane-element-iv-parameters-e-rev iv-parameters-structure)
	(the sf (effective-reversal-potential ion-perms (or (element cell-type 'cell-type) (element-cell-type elt)))))))


(defun effective-reversal-potential (ion-perms &optional element)
  "Calculate reversal potential based on the list ION-PERMS, which has the format:

             '((ion permeability) (ion permeability) ...)

where ION is one of the symbols used by the function DEFAULT-ION-REVERSAL-POTENTIAL, and
PERMEABILITY is the [single float] relative permeability. The reversal potentials for each ION
references the cell-type associated with ELEMENT, or the DEFAULT-ION-REVERAL-POTENTIAL."
  (declare (optimize (safety 0) (speed 3) (space 0) (compilation-speed 0)))
  (when (> (loop for ion-perm in ion-perms sum (cadr ion-perm)) 1)
    (sim-error (format nil "The perms in ~A are greater than 1." ion-perms)))
  (let* ((cell (element-cell element))
	 (cell-type (when cell (cell-type cell)))
	 (e-na (if cell-type (cell-type-e-na cell-type) (DEFAULT-ION-REVERSAL-POTENTIAL 'na)))
	 (e-k (if cell-type (cell-type-e-k cell-type) (DEFAULT-ION-REVERSAL-POTENTIAL 'k)))
	 (e-ca (if cell-type (cell-type-e-ca cell-type) (DEFAULT-ION-REVERSAL-POTENTIAL 'ca)))
	 (e-cl (if cell-type (cell-type-e-cl cell-type) (DEFAULT-ION-REVERSAL-POTENTIAL 'cl))))
    (declare (single-float e-na e-k e-ca e-cl))
    (loop for ion-perm in ion-perms sum
	  (let ((perm (cadr ion-perm)))
	    (declare (single-float perm))
	    (the sf (case (car ion-perm)
		      (NA (the sf (* perm e-na)))
		      (K (the sf (* perm e-k)))
		      (CL (the sf (* perm e-cl)))
		      (CA (the sf (* perm e-ca)))
		      (t 0.0))))
	  into e-rev single-float finally (return e-rev))))

;; For both synapses and channels - this does not apply to elements whose reversal potentials depend
;; on ion integrators, since in this case the e-revs are initialized by the integrators' states.
;; Called from INIT-CHANNELS-E-REV, INIT-SYNAPSES-E-REV, SET-CHANNEL-PARAMETERS and
;; SET-SYNAPSE-PARAMETERS.
(defun update-element-fixed-e-rev (elt &optional fixed-e-rev elt-type)
  (declare (optimize (safety 0) (speed 3) (space 0) (compilation-speed 0))
	   (ignore elt-type))
  (let ((fixed-e-rev (the sf (or fixed-e-rev (current-element-type-e-rev elt nil elt-type)))))
    (typecase elt
      (channel (setf (channel-e-rev elt) (coerce-to-double fixed-e-rev)))
      (synapse (setf (synapse-e-rev elt) (coerce-to-double fixed-e-rev))))
    nil))



(proclaim '(inline element-e-rev-from-shells))
;; ELEMENT-E-REV-FROM-SHELLS The CONC-INTS-PARAMS argument comes from the channel or synapse :CONC-INTS-PARAMS slot.
;; This function is called from within EVAL-ALL-CHANNELS and EVAL-ALL-x-SYNAPSES. The reversal potentials as
;; evaluated from the concentration integrators are calculated on the concentrations of the *last* time step.
;;
;; The format of the CONC-INTS-PARAMS list is
;;
;;     '( (conc-int conc-int-shell e-reversal-coefficient)
;;        (conc-int conc-int-shell e-reversal-coefficient)
;;                           .
;;                           .
;;                           .
;;        (conc-int conc-int-shell e-reversal-coefficient))
;;
;;
;;  Typically, the value of E-REVERSAL-COEFFICIENT will be the same as the permeablility coefficient associated
;;  with this channel or synapse, and assigned to the concentration integrator CONC-INT
;;  :SHELL-x-PORES slot by the function SET-CONC-INTEGRATOR-PARAMETERS. 
;;
;;  For each given channel or synapse, the :CONC-INTS-PARAMS list is set with the result of
;;  PARSE-CONC-INT-INFO-FOR-ELEMENT. For the information regarding what concentration integrators to reference
;;  the reversal potential, this function first looks at the associated channel or synapse type :PARAMETERS
;;  slot for an entry with the key 'CONC-INT-TYPE-E-REV-PARAMS. If this entry does not exist then, like the
;;  function SET-CONC-INTEGRATOR-PARAMETERS, this function references the :CONC-INT-TYPE-PARAMS slot of the
;;  apprpriate channel or synapse type. Both the 'CONC-INT-TYPE-E-REV-PARAMS list (if it exists) and the
;;  :CONC-INT-TYPE-PARAMS are lists including each ion that passes through the channel or synapse type, an
;;  entry for a concentration integrator type, for the shell (1 and/or 2) and the proportion of the current
;;  associated with the shell, e.g.:
;;
;;         '((CA-IN (1 0.7) (2 0.24)) (K-EXTRA (1 1)))
;;
;;  The distinction between the interpretation of the current proportion parameter when processed by
;;  SET-CONC-INTEGRATOR-PARAMETERS versus PARSE-CONC-INT-INFO-FOR-ELEMENT is that in the first case this
;;  parameter is used to fractionate the total current of the channel or synapse into various concentration
;;  integrator compartments, whereas in the latter case the parameter specifies the relative contribution of a
;;  concentration integrator compartment to the reversal potential of a given channel or synapse. Note that, as
;;  mentioned above, in the more typical case the relative permeability to flow and for reversal potential will
;;  be the same.

(defun element-e-rev-from-shells (conc-ints-params)
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (let ((result 0.0d0))
    (declare (double-float result))
    (do ((conc-ints-params conc-ints-params (cdr conc-ints-params)))
	((null conc-ints-params) result)
      (let* ((integrator-list (car conc-ints-params))
	     (coefficient (the df (caddr integrator-list))))		    
	(setq result (the df (+ result (the df (* coefficient
						  (the df (case (the fn (cadr integrator-list))
							    (1 (conc-int-e-rev-shell-1 (car integrator-list)))
							    (2 (conc-int-e-rev-shell-2 (car integrator-list)))
							    (t 0.0d0))))))))))))



