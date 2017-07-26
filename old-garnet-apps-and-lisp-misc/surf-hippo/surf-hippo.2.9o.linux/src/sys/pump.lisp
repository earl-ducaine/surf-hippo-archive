;;;-*- mode: lisp; Syntax: Common-lisp; package: surf ; base: 10 ; -*-
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


;;; SYS Source file: pump.lisp
;
; The model to integrate ion pumps. See also pump-preliminaries.lisp
;


(in-package "SURF-HIPPO")

(defun document-pump-type (type-name-or-type)
  (let ((type-name (element-name type-name-or-type 'pump-type))
	(type (element type-name-or-type 'pump-type)))
    (when type
      (format t "(pump-type-def~%")
      (format t "  `(~a~%" type-name)
      (format t "        (class . ~s)~%" (pump-type-class type))
      (case (pump-type-class type)
	(:mm-zador
	 (format t "       (k-max . ~A)~%" (element-parameter type 'k-max))
	 (format t "       (kd . ~A)~%" (pump-type-kd type))
	 (format t "       (density . ~A)~%" (element-parameter type 'density)))
	(:mm
	 (format t "       (v-max . ~A)~%" (pump-type-v-max type))
	 (format t "       (kd . ~A)~%" (pump-type-kd type)))
	(:first-order-tau-v
	 (format t "       (equilibrium-conc . ~A)~%" (pump-type-equilibrium-conc type))
	 (format t "       (tau-function . ~A)~%" (element-parameter type 'tau-function)))
	(:first-order
	 (format t "       (equilibrium-conc . ~A)~%" (pump-type-equilibrium-conc type))
	 (format t "       (tau . ~A)~%" (pump-type-tau type))))
      (format t "       (species . ~A)~%" (pump-type-species type))
      (format t "        (q10 . ~a)~%" (pump-type-q10 type))
      (format t "        (reference-temp . ~a)~%" (pump-type-reference-temp type) )
      (element-document-extras type)
      (format t "   ))~%"))))


(defun menu-for-pumps ()
  (loop for name in (choose-list-values (namelist-of-all-things 'pump-type t)
					nil :label "Select Pump Types To Modify")
	do (edit-pump-type (element name))))


(defun edit-pump-types (&optional types)
  (let ((types (if types
		   (loop for type in (if (consp types) types (list types)) collect (element type 'pump-type))
		   (menu-for-type 'pump-type))))
    (loop for type in types do (edit-pump-type type))))

(defun edit-pump-type (type)
  (let* ((type (element type 'pump-type))
	 (dummy1 (pump-type-equilibrium-conc type))
	 (dummy2 (pump-type-tau type))
	 (dummy3 (pump-type-enabled type))
	 (dummy4 (pump-type-v-max type))
	 (dummy5 (pump-type-kd type))
	 (dummy13 (pump-type-name type))
	 (dummy14 (pump-type-q10 type))
	 (dummy15 (pump-type-reference-temp type))
	 (menu-label (format nil "Editing Pump Type ~A" (pump-type-name type)))
	 (menu-text (ADD-LINEFEEDS-TO-STRING-LIST
		     (list (format nil "Membrane Ion Pump Class ~A" (pump-type-class type))
			   (ELEMENT-SOURCEFILE-STRING type nil))))
	 (menu-list (case (pump-type-class type)
		      (:first-order
		       `((dummy1 "Equilibrium concentration [mM]" :float)
			 (dummy2 "Tau [ms]" :float)
			 (dummy3 "Enable this pump type" :boolean)
			 (dummy14 "Q10" :float)
			 (dummy15 "Kinetics Reference Temperature [degs C]" :float)
			 (dummy13 ,(format nil "Edit name of type (used if saved to file):") :string)))
		      (:FIRST-ORDER-TAU-V
		       `((dummy1 "Equilibrium concentration [mM]" :float)
			 (dummy3 "Enable this pump type" :boolean)
			 (dummy14 "Q10" :float)
			 (dummy15 "Kinetics Reference Temperature [degs C]" :float)
			 (dummy13 ,(format nil "Edit name of type: ~% (used if saved to file)") :string)))    
		      ((:mm-zador :mm)
		       `((dummy3 "Enable this pump type" :boolean)
			 (dummy4 "Vmax [mM / (ms*cm2)]" :float)
			 (dummy5 "Kd [mM]" :float)
			 (dummy14 "Q10" :float)
			 (dummy15 "Kinetics Reference Temperature [degs C]" :float)
			 (dummy13 ,(format nil "Edit name of type: ~% (used if saved to file)") :string))))))
    (choose-variable-values menu-list :text menu-text :label menu-label)
    (setf (pump-type-equilibrium-conc type) dummy1
	  (pump-type-tau type) dummy2
	  (pump-type-enabled type) dummy3
	  (pump-type-v-max type) dummy4
	  (pump-type-kd type) dummy5
	  (pump-type-name type) dummy13
	  (pump-type-q10 type) dummy14
	  (pump-type-reference-temp type) dummy15)
    (set-pumps-parameters)))

(defun set-pumps-parameters ()
  (maphash 'set-pump-parameters (PUMP-HASH-TABLE)))

(defun set-pump-parameters (name pump)
  (declare (optimize (safety 0) (speed 3) (space 0) (compilation-speed 0))
	   (ignore name))
  (set-pump-area pump))

(defun create-pump-type (type-symbol &optional actual-type-symbol update-parameters)
  (when (stringp type-symbol) (setq type-symbol (intern type-symbol)))
  (let* ((type (unless actual-type-symbol
		 (if (pump-type-p type-symbol)
		     type-symbol
		     (gethash (string type-symbol) (PUMP-TYPE-HASH-TABLE)))))
	 (model (type-symbol-model 'pump-type))
	 (original-parameters (retrieve-model-parameters-from-library type-symbol model))
	 (parent-type-symbol (get-a-value 'parent-type original-parameters)))
    (unless (and type (not update-parameters))
      (when (eq parent-type-symbol type-symbol)
	(sim-error (format nil "~A has a recursive parent type!" parent-type-symbol)))
      (unless original-parameters (sim-error (format nil "Don't know anything about pump type ~A!" type-symbol)))
      (when actual-type-symbol (setq type-symbol actual-type-symbol))
      (unless type
	(setq type (if parent-type-symbol
		       (create-PUMP-TYPE parent-type-symbol type-symbol update-parameters)
		       (make-PUMP-TYPE :name (string type-symbol)))))
      (setq original-parameters (update-element-parameters-with-new-parameters original-parameters type))    
      (when parent-type-symbol (push-element-parameter type 'parent-types parent-type-symbol))
      (setf (pump-type-class type) (get-a-value 'class original-parameters))
      (case (pump-type-class type)
	(:mm-zador
	 (element-parameter 'k-max (get-a-sf-value 'k-max original-parameters))
	 (element-parameter 'density (get-a-sf-value 'density original-parameters))
	 (setf (pump-type-kd type) (get-a-sf-value 'kd original-parameters)))
	(:mm
	 (setf
	  (pump-type-v-max type) (get-a-sf-value 'v-max original-parameters)
	  (pump-type-kd type) (get-a-sf-value 'kd original-parameters)))
	(:FIRST-ORDER-TAU-V
	 (setf (pump-type-equilibrium-conc type) (get-a-sf-value 'equilibrium-conc original-parameters))
	 (element-parameter type 'tau-function (get-a-value 'tau-function original-parameters)))
	(:first-order
	 (setf (pump-type-equilibrium-conc type) (get-a-sf-value 'equilibrium-conc original-parameters)
	       (pump-type-tau type) (get-a-sf-value 'tau original-parameters))))
      (cond-every
       ((assoc 'species original-parameters)
	(setf (pump-type-species type) (get-a-value 'species original-parameters)))
       ((or (assoc 'qten original-parameters) (assoc 'q10 original-parameters))
	(setf (pump-type-q10 type) (s-flt (cdr (or (assoc 'qten original-parameters) (assoc 'q10 original-parameters))))))

       ((assoc 'reference-temp original-parameters)
	(setf (pump-type-reference-temp type) (get-a-sf-value 'reference-temp original-parameters))))
      (unless (member type *make-needed-v-pump-arrays*) (push type *make-needed-v-pump-arrays*))
      (setf (gethash (string type-symbol) (PUMP-TYPE-HASH-TABLE)) type))
    (setq *pump-type* type)
    type))

(defun get-pump-simple-name ()
  (loop for candidate from (max 1 *pump-simple-name-counter*)
	until (not (gethash candidate (pump-hash-table)))
	finally (return (setf *pump-simple-name-counter* candidate))))

(defun rename-pumps-simple (&optional (pumps (pumps)))
  "Rename PUMPS [default all pumps in circuit] with simple integer names."
  (loop for seg in pumps do
	(let ((name (get-pump-simple-name)))
	  (set-element-name seg name 'pump))))

(defun create-pump (cint type conc-int-compartment)
  (let* (*print-pretty*
	 (type (typecase type
		 (pump-type type)
		 (t (create-pump-type type))))
	 (pump-name (if *use-simple-names*
			(get-pump-simple-name)
					; (1+ (hash-table-count (PUMP-HASH-TABLE)))
			(concatenate-strings (conc-int-name cint) "-" (pump-type-name type))))
	 (nd (element-physical-node cint))
	 (pump (make-pump :name pump-name :type type :conc-int cint :conc-int-compartment conc-int-compartment)))
    (setf (gethash pump-name (PUMP-HASH-TABLE)) pump)
    (case (pump-conc-int-compartment pump)
      (1 (push pump (conc-int-shell-1-pumps cint)))
      (2 (push pump (conc-int-shell-2-pumps cint)))
      (3 (push pump (conc-int-shell-3-pumps cint))))
    (push pump (node-elements nd))
    (push pump (pump-type-pumps type))
    (setq *pump* pump)))


(defun remove-pump-from-conc-int (pump)
  (let ((cint (pump-conc-int pump)))
    (when cint
      (setf (conc-int-shell-1-pumps cint) (remove pump (conc-int-shell-1-pumps cint))
	    (conc-int-shell-2-pumps cint) (remove pump (conc-int-shell-2-pumps cint))
	    (conc-int-shell-3-pumps cint) (remove pump (conc-int-shell-3-pumps cint))))))

;; SET-PUMP-AREA Called by SET-CONC-INT-PUMPS.
(defun set-pump-area (pump)
  (let ((cint (pump-conc-int pump)))
    (setf (pump-area pump) 
	  (d-flt (conc-int-shell-membrane-area cint (pump-conc-int-compartment pump))))
    (case (pump-type-class (pump-type pump))
      (:mm (setf (pump-mm-coefficent pump) ; millimoles/ms
		 (* (pump-area pump)	; um2
		    1.0e-8		; cm2/um2
		    (pump-type-v-max (pump-type pump))))))) ; millimole ms^-1 cm^-2
  nil)



	 
(defun setup-pumps ()
  (loop for type in (pump-types) do
	(remove-pump-type-lists type)
	(loop for pump in (pump-type-pumps type) do (setf (pump-enabled-for-this-simulation pump) nil)))
  (loop for conc-int-type in *conc-int-type-list* do
	(loop for cint in (element-parameter conc-int-type 'active-conc-ints) do
	      (loop for pump in (conc-int-all-pumps cint) 
		    do (setf (pump-enabled-for-this-simulation pump)
			     (and (pump-type-enabled (pump-type pump))
				  (pump-enabled pump))))))
  (setq *pump-type-list*
	(no-nils
	 (loop for type in (pump-types)
	       when (pump-type-enabled type)
	       collect
	       (let ((active-pumps (loop for pump in (pump-type-pumps type)
					 do (setf (pump-current pump) 0.0d0)
					 when (pump-enabled-for-this-simulation pump)
					 collect pump)))
		 (when active-pumps
		   (revamp-pump-type-lists type active-pumps)
		   type))))))

(defun remove-pump-type-lists (type)
  (remove-element-parameters type '(active-pumps)))

(defun revamp-pump-type-lists (type active-pumps &optional inclusion)
  (when inclusion (setq active-pumps (loop for pump in active-pumps when (funcall inclusion pump) collect pump)))
  (when active-pumps (element-parameter type 'active-pumps active-pumps)))


(defun init-pump (pump)
  (declare (optimize (safety 0) (speed 3) (space 0)))
  (let ((type (pump-type pump))
	(conc ;; (conc-int-type-core-conc-double (conc-int-type (pump-conc-int pump)))
	 (pump-conc-int-compartment-free-concentration-n pump t)))

    (case (pump-type-class type)
      (:mm (setf (pump-basal-rate pump) (eval-mm-pump pump conc t)))
      (:mm-zador (setf (pump-basal-rate pump) (eval-mm-pump pump conc t))))
    (setf (pump-current pump) 0.0d0)
;    (setf (pump-current pump) (pump-basal-rate pump))
    )
  nil)


;; check this....
(defun set-zador-mm-coefficent (pump)
  (let ((type (pump-type pump)))
    (setf (pump-mm-coefficent pump)
	  (* (element-parameter type 'k-max) ; ms^-1
	     (/ (pump-area pump)	; in um2
		(* (pump-conc-int-compartment-volume pump);  cm3
		   1.0e12))		; convert to um3
	     (pump-type-total-density type) ; millimole/cm2
		
	     ;; Faraday			; Coulombs/mole
	     1000			; ms/second
	     ;; 1.0e-3			; moles/millimole
	     ;; 1.0e9			; nA/A
	     ))))


;; Returns pump current in millimole/ms. CONCENTRATION in mM.
(proclaim '(inline eval-mm-pump))
(defun eval-mm-pump (pump concentration &optional initialize)
  (declare (optimize (safety 0) (speed 3) (space 0))
	   (double-float concentration))
  (let ((type (pump-type pump)))
    (- (* (pump-mm-coefficent pump)	; millimoles/ms
	  (/ concentration		; mM
	     (+ concentration		; mM
		(pump-type-kd type))))	; mM
       (if initialize 0.0d0 (pump-basal-rate pump)))) ; millimoles/ms
  )
  




(proclaim '(inline pump-conc-int-compartment-free-concentration-n))
(defun pump-conc-int-compartment-free-concentration-n (pump &optional initial-value)
  (declare (optimize (safety 0) (speed 3) (space 0)))
  (let ((cint (pump-conc-int pump)))
    (if initial-value
	(d-flt (element-parameter-fast 'resting-free-conc (conc-int-type-parameters (conc-int-type cint))))
	(the df (case (pump-conc-int-compartment pump)
		  (1 (conc-int-shell-1-free-conc-n cint))
		  (2 (conc-int-shell-2-free-conc-n cint))
		  (3 (conc-int-shell-3-free-conc-n cint))
		  (t 0.0d0))))))

(proclaim '(notinline eval-pump))
(defun eval-pump (pump)
  (declare (optimize (safety 0) (speed 3) (space 0)))
  (setf (pump-current pump)
	(let ((concentration (the df (pump-conc-int-compartment-free-concentration-n pump)))
	      (type (pump-type pump)))
	  (case (pump-type-class type)
	    (:generic (funcall (element-parameter type 'pump-function) pump concentration))
	    (:mm (eval-mm-pump pump concentration))
					; (:mm-zador (eval-mm-zador-pump pump concentration))
	    (t 0.0d0))))
  nil)

(defun eval-all-pumps (&optional initial-state)
  (declare (optimize (safety 0) (speed 3) (space 0) (compilation-speed 0)))
  (if initial-state
      (loop for pump being the hash-value of (pump-hash-table) do (init-pump pump))
		
    (loop for type in *pump-type-list* do
	  (loop for pump in (get-a-value 'active-pumps (pump-type-parameters type)) do (eval-pump pump)))))


(defun init-pumps ()
  (eval-all-pumps t))


(defun make-needed-v-pump-arrays (&optional all)
  (loop for type in (if all (hash-table-list (PUMP-TYPE-HASH-TABLE)) (delete-duplicates *make-needed-v-pump-arrays*))
	do (make-v-pump-arrays type))
  (setq *make-needed-v-pump-arrays* nil))


(defun make-v-pump-arrays (type)
  (declare (optimize (safety 1) (speed 3) (space 0)) (type pump-type type))
  (case (pump-type-class type)
    (:FIRST-ORDER-TAU-V
     (let ((q10-tau-factor (element-q10-tau-factor type))
	   (pump-type-tau-function (element-parameter type 'tau-function)))
       (declare (type single-float q10-tau-factor))
       (setf (pump-type-tau-array type)
	     (list-to-array-double
	      (loop for voltage from *particle-look-up-table-min-voltage* to (particle-look-up-table-max-voltage)
		    by *particle-look-up-table-precision*
		    collect (the sf (* q10-tau-factor (funcall pump-type-tau-function voltage))))))))))

	       
(defun print-pump-type (pump-type)
  (format t "Membrane Ion Pump Type ~a (class ~a): species ~a, Q10 ~a T_ref ~a~%"
	  (pump-type-name pump-type)
	  (pump-type-class pump-type)
	  (pump-type-species pump-type)
	  (my-float-format (pump-type-Q10 pump-type))
	  (my-float-format (pump-type-reference-temp pump-type)))
  (case (pump-type-class pump-type)
    (:FIRST-ORDER-TAU-V
     (format t "  Equilibrium concentration ~,2e mM, voltage-dependent tau~%"
	     (pump-type-equilibrium-conc pump-type)))
    (:FIRST-ORDER
     (format t "  Equilibrium concentration ~,2e mM, tau ~,2e ms~%"
	     (pump-type-equilibrium-conc pump-type)
	     (pump-type-tau pump-type)))
    (:mm
     (format t "  V-max ~,2e [mM ms^-1 cm^-2], Kd ~,2e mM~%"
	     (pump-type-v-max pump-type)
	     (pump-type-kd pump-type)))
    (:mm-zador
     (format t "  V-max ~,2e [mM ms^-1 cm^-2], Kd ~,2e mM~%"
	     (pump-type-v-max pump-type)
	     (pump-type-kd pump-type))))
  (print-num-elements-sourcefile pump-type)
  (format t "~%"))


(defun print-pump (pump)
  (format t "Membrane Ion Pump ~A, cell ~A: type ~a~%"
	  (pump-name pump)
	  (element-name (element-cell pump))
	  (pump-type-name (pump-type pump)))
  (format t "    Concentration integrator ~A, compartment ~A~%"
	  (conc-int-name (pump-conc-int pump))
	  (pump-conc-int-compartment pump))
  (when *simulation-initialized*
    (format t "    Current ~,2e ~a @ ~,2e ms~%"
	    (pump-concentration-current pump) (pump-current-units) *real-time*))
  (case (pump-type-class (pump-type pump))
    (:mm (format t "    Basal rate ~,2e millimoles/ms~%" (pump-basal-rate pump)))
    (:mm-zador (format t "    Basal rate ~,2e millimoles/ms~%" (pump-basal-rate pump)))))


(defun pump-current-units ()
  (plot-list-info-units-label (find '*plot-pump-currents* *plot-lists-info* :key 'car)))

