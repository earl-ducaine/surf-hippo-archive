;; -*- Mode: lisp; Syntax: Common-lisp; package: surf ; base: 10 ; -*-
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


;;; SYS Source file: particle.lisp

(in-package "SURF-HIPPO")

;; These two are for markov particles.
(defmacro nb-states-1 (prt)
  `(1- (the fn (particle-type-number-of-states (particle-type ,prt)))))

(defmacro prt-state (prt index)
  `(aref (the simple-vector (particle-state-arrays ,prt)) ,index))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun number-PARTICLE-TYPE-PARTICLES (type)
  (let ((type (element type 'particle-type)))
    (when type
      (loop with prt = (particle-type-first-particle type)
	    while prt 
	    sum 1 into total
	    do (setq prt (particle-next-particle prt))
	    finally (return total)))))
  
(defun PARTICLE-TYPE-PARTICLES (type)
  (let* ((type (element type 'particle-type)))
    (when type
      (loop with prt = (particle-type-first-particle type)
	    while prt
	    collect prt into prts
	    do (setq prt (particle-next-particle prt))
	    finally (return prts)))))

(defun reorder-particles-of-type (type &optional prt last-prt)
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (let ((type (element-type type 'particle-type)))
    (when (and type *enable-reorder-particles-of-type*)
      (if prt
	  (let ((last-particle-of-type (or last-prt (last-particle-of-type type))))
	    (if last-particle-of-type
		(setf (particle-next-particle last-particle-of-type) prt)
		(setf (particle-type-first-particle type) prt)))
	  (progn
	    (clear-particles-of-type type)
	    (let ((last-prt nil))
	      (loop for prt being the hash-value of (particle-hash-table)
		    when (eq (particle-type prt) type)
		    do
		    (reorder-particles-of-type type prt last-prt)
		    (setq last-prt prt))))))))

(defun last-particle (prt)
  (if (and prt (particle-next-particle prt))
      (last-particle (particle-next-particle prt))
      prt))

(defun last-particle-of-type (type)
  (last-particle (particle-type-first-particle type)))

(defun clear-particles-of-type (type)
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (setf (particle-type-first-particle type) nil)
  (loop for prt being the hash-value of (PARTICLE-HASH-TABLE)
	when (eq type (particle-type prt))
	do (setf (particle-next-particle prt) nil)))

(defun get-particles-of-type (type)
  (loop for prt being the hash-value of (PARTICLE-HASH-TABLE)
	when (eq type (particle-type prt))
	collect prt))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun edit-particle-type (type &optional channel-type number-particles-per-channel)
  (when (element type 'particle-type)
    (let* ((type (element type 'particle-type))
	   (channel-type (element channel-type 'channel-type))
	   menu-list
	   (dummy1 (particle-type-z type))
	   (dummy2 (particle-type-gamma type))
	   (dummy3 (particle-type-base-rate type))
	   (dummy4 (particle-type-tau-0 type))
	   (dummy5 (particle-type-v-half type))
	   (dummy6 (particle-type-q10 type))
	   (dummy7 (particle-type-reference-temp type))
	   (dummy8 number-particles-per-channel)
	   (dummy9 (particle-type-name type))
	   (dummy10 (convert-p-type-class-sym (particle-type-class type)))
	   (dummy11 *Temp-celcius*)
	   (dummy12 (element-parameter type 'use-Fixed-boltzmann-reference-temperature))
	   (dummy13 (or (element-parameter type 'Fixed-boltzmann-reference-temperature) *temp-celcius*))
	   (dummy14 (cond ((particle-type-IGNORE-Tau-VOLTAGE-DEPENDENCE type) :Sets_tau)
			  (t (case (convert-p-type-class-sym (particle-type-class type))
			       (:hh-ext :Additive)
			       (t :Minimum_tau)))))
	   dummy15
	   dummy16
	   dummy17
	   dummy18
	   (dummy19 (or (element-parameter type 'alpha_0) 0.0))
	   (dummy20 (or (element-parameter type 'beta_0) 0.0))
	   (dummy21 t)
	   (dummy22 (or (element-parameter type 'tau_0_applied_to) :all_kinetics))
	   (dummy23 (element-parameter type 'linear-markov-n))
	   (dummy24 (element-parameter type 'linear-markov-m)))
      (loop while (or dummy15 dummy21) do
	    (setq dummy15 nil)
	    (case dummy10
	      (:markov (setq menu-list '((dummy17 "Edit Markov transition functions" :boolean))))
	      ((:hh-ext :hh-ext-old)
	       (setq menu-list `((:comment ,(format nil "Parameters of ~A Particle type class:"
					     dummy10))
				 (dummy5 "V-1/2 [mV]" :float)
				 (dummy1 "Valence [+/- => act/inact]" :number)
				 (dummy2 "Gamma [0-1]" :float)
				 (dummy3 "Base-Rate for transitions [1/ms]" :float)
				 (dummy4 "Tau-0 [ms]" :float)
				 (dummy14 "Role of tau-0:" :choose (:Sets_tau :Additive :Minimum_tau))
				 (dummy22 "Apply tau-0 to:" :choose (:all_kinetics :v-dep_kinetics))
				 (:comment "Voltage independent rate constants:")
				 (dummy19 "alpha_0 - Forward rate constant [1/ms]" :float)
				 (dummy20 "beta_0 - Backward rate constant [1/ms]" :float)
				 (dummy12 "Use fixed boltzmann reference temperature" :boolean)
				 (dummy13 "Fixed boltzmann reference temperature [deg C]" :float))))
	      (t (setq menu-list `((:comment ,(format nil "~A Particle type class." dummy10))))))
					       
	    (setq menu-list (reverse menu-list))
	    (when (and dummy23 dummy24)
	      (push '(dummy24 "Linear Markov model M:" :integer) menu-list)
	      (push '(dummy23 "Linear Markov model N:" :integer) menu-list))
	    (unless channel-type
	      (push `(dummy15 "Plot voltage-dependent particles:" :x-choose
			      ,(case (particle-type-class type)
				 (:markov '(:alpha_&_beta :steady_state))
				 (t '(:steady_state :tau :alpha_&_beta)))) menu-list))
	    (push '(dummy6 "Q10" :float) menu-list)
	    (push '(dummy7 "Kinetics Reference Temperature [degs C]" :float) menu-list)
	    (push '(dummy11 "Current temperature [degs C]" :float) menu-list) 
	    (when (element-parameter type 'concentration-particle-type)
	      (push '(dummy18 "Edit associated concentration particle type" :boolean) menu-list))
	    (when (and number-particles-per-channel channel-type)
	      (push '(dummy8 "Number of particles per channel" :integer) menu-list))
	    (push `(dummy9 ,(format nil "Edit name of type (used if saved to file):") :string) menu-list)
	    (when (element-parameter type 'v-half-shift-particle-type)
	      (push `(dummy16 ,(format nil "Edit V-shift ~A particle type"
				       (element-name (element-parameter type 'v-half-shift-particle-type)))
			      :boolean) menu-list))
	    (setq menu-list (reverse menu-list))
	    (choose-variable-values menu-list
				    :text (ADD-LINEFEEDS-TO-STRING-LIST (list
									 (ELEMENT-SOURCEFILE-STRING type nil))) 
				    :label (format nil "Parameters Of Particle Type ~A" (particle-type-name type)))
	    (when (and dummy24 dummy23)
	      (element-parameter type 'linear-markov-n dummy23)
	      (element-parameter type 'linear-markov-m dummy24))
	    (when dummy17 (edit-markov-transition-functions type))
	    (when dummy16 (edit-element (element-parameter type 'v-half-shift-particle-type)))
	    (when dummy18 (edit-element (element-parameter type 'concentration-particle-type)))
	    
	    (let* ((temp-changed-for-this-prt-type
		    (or (xor dummy12 (element-parameter type 'use-Fixed-boltzmann-reference-temperature))
			(and (element-parameter type 'Fixed-boltzmann-reference-temperature)
			     (not (and (element-parameter type 'Fixed-boltzmann-reference-temperature)
				       (= dummy13 (element-parameter type 'Fixed-boltzmann-reference-temperature)))))
			(not (eq dummy14 (cond ((particle-type-IGNORE-Tau-VOLTAGE-DEPENDENCE type) :Sets_tau)
					       (t (case (convert-p-type-class-sym (particle-type-class type))
						    (:hh-ext :Additive)
						    (t :Minimum_tau))))))
			(not (eq dummy22 (element-parameter type 'tau_0_applied_to)))
			(not (= dummy6 (particle-type-q10 type)))
			(not (= dummy7 (particle-type-reference-temp type)))))
		   (nothing-changed
		    (and (not temp-changed-for-this-prt-type)
			 (= dummy19 (or (element-parameter type 'alpha_0) 0.0))
			 (= dummy20 (or (element-parameter type 'beta_0) 0.0))
			 (= dummy1 (particle-type-z type))
			 (= dummy2 (particle-type-gamma type))
			 (= dummy3 (particle-type-base-rate type))
			 (= dummy4 (particle-type-tau-0 type))
			 (= dummy5 (particle-type-v-half type))
			 (= dummy6 (particle-type-q10 type)))))
	      (setq *update-temperature-dependent-parameters*
		    (or *update-temperature-dependent-parameters* (not (= dummy11 *Temp-celcius*))))
	      (setq *temperature* (temperature-centigrade-to-kelvin dummy11) *Temp-celcius* dummy11)
	      (SET-ELEMENT-NAME type dummy9)
	      (case (particle-type-class type)
		((:hh-ext :hh-ext-old)
		 (element-parameter type 'tau_0_applied_to dummy22)
		 (element-parameter type 'alpha_0 (unless (= 0 dummy19) dummy19))
		 (element-parameter type 'beta_0 (unless (= 0 dummy20) dummy20))
		 (setf (particle-type-z type) dummy1
		       (particle-type-gamma type) dummy2
		       (particle-type-base-rate type) dummy3
		       (particle-type-IGNORE-Tau-VOLTAGE-DEPENDENCE type) dummy14
		       (particle-type-tau-0 type) dummy4
		       (particle-type-v-half type) dummy5)
		 (element-parameter type 'use-Fixed-boltzmann-reference-temperature dummy12)
		 (element-parameter type 'Fixed-boltzmann-reference-temperature (coerce-to-single dummy13))))

	      (case dummy14
		(:Sets_tau (setf (particle-type-IGNORE-Tau-VOLTAGE-DEPENDENCE type) t))
		(:Additive (setf (particle-type-class type) :hh-ext
				 (particle-type-IGNORE-Tau-VOLTAGE-DEPENDENCE type) nil))
		(:Minimum_tau (setf (particle-type-class type) :hh-ext-old
				    (particle-type-IGNORE-Tau-VOLTAGE-DEPENDENCE type) nil)))
	      (setf (particle-type-q10 type) dummy6
		    (particle-type-reference-temp type) dummy7)
	      (if *update-temperature-dependent-parameters*
		  (update-temperature-dependent-parameters)
		  (unless nothing-changed
		    (update-particle-type-q10 type)
		    (make-v-particle-arrays type)))
	      (when dummy15 (plot-v-particles type :what dummy15))
	      (setq dummy21 nil)))
      (values type dummy8))))

(defun particles-of-type (type)
  (let ((type (element type 'particle-type)))
    (loop for prt in (particles) when (equal (particle-type prt) type) collect prt)))

(defun print-particle-type (type &optional (always t))
  (let ((type (element type 'particle-type)))
    (when (and type (or always (particles-of-type type)))
      (format t "Particle-type ~a (class ~s):~%" (particle-type-name type) (particle-type-class type))
      (case (particle-type-class type)
	(:markov
	 (format t "  States: ") (simple-format-list (element-parameter type 'STATEs) t)
	 (format t "  Open States: ") (simple-format-list (element-parameter type 'open-STATEs) t)
	 (when (element-parameter type 'initial-state)
	   (format t "  Explicit Initial Conditions: ~A~%" (element-parameter type 'initial-state))) 
	 (format t "~%")
	 (print-markov-particle-type-STATE-VOLTAGE-TRANSITION-FUNCTIONS type))
	((or :hh-ext-old :hh-ext)
	 (format t "  z ~a gamma ~a base-rate ~a V-1/2 ~,f tau-0 ~a ~A~,f Q10 ~a T_ref ~a~%"
		 (particle-type-z type)
		 (particle-type-gamma type)
		 (if (particle-type-IGNORE-Tau-VOLTAGE-DEPENDENCE type) "UNDEFINED" (particle-type-base-rate type))
		 (particle-type-v-half type)
		 (case (particle-type-class type)
		   (:hh-ext-old "(hard limit)")
		   (:hh-ext "(additive)"))
		 (case (particle-type-class type)
		   (:hh-ext-old "")
		   (:hh-ext (case (or (element-parameter type 'tau_0_applied_to) :all_kinetics)
			      (:all_kinetics "(applied to entire kinetics) ") 
			      (t "(applied to v-dep kinetics) ")))) 
		 (particle-type-tau-0 type)
		 (particle-type-Q10 type)
		 (particle-type-reference-temp type))

	 )
	(:hh			
	 (format t   "  Canonical Hodgkin-Huxley Parameters, Q10 ~a T_ref ~a~%"
		 (particle-type-Q10 type) (particle-type-reference-temp type))))
      (case (particle-type-class type)
      	((:hh-ext :hh-ext-old :hh)
	 (when (element-parameter type 'linear-markov) 
	   (format t "  Linear Markov Model, N= ~D, M = ~D~%"
		   (element-parameter type 'linear-markov-n)
		   (element-parameter type 'linear-markov-m)))))
      (case (particle-type-class type)
	((:hh-ext :hh-ext-old)
	 (when (and (element-parameter type 'use-Fixed-boltzmann-reference-temperature)
		    (element-parameter type 'Fixed-boltzmann-reference-temperature))
	   (format t "  Fixed boltzmann reference temperature ~A~%"
		   (element-parameter type 'Fixed-boltzmann-reference-temperature)))))
      (cond-every
       ((element-parameter type 'alpha_0)
	(format t "  Alpha_0 (v-indep forward rate constant) [1/ms]: ~A~%" (element-parameter type 'alpha_0)))
       ((element-parameter type 'beta_0)
	(format t "  Beta_0 (v-indep backward rate constant) [1/ms]: ~A~%" (element-parameter type 'beta_0)))
       ((particle-type-alpha-function type) (format t "  Explicit alpha rate function~%"))
       ((particle-type-beta-function type) (format t "  Explicit beta rate function~%"))

       ((and (particle-type-alpha-function type)
	     (particle-type-beta-function type)
	     (/= 1 (particle-type-tau-coefficient type)))
	(format t "  Tau coefficient: ~A~%" (particle-type-tau-coefficient type)))
       
       ((particle-type-tau-function type) (format t "  Explicit tau function~%"))
       ((particle-type-ss-function type) (format t "  Explicit steady-state function~%")))
      (print-num-elements-sourcefile type)
      (format t "~%~%"))))




		 
(defun document-particle-type (type-name-or-type)
  (let* ((type-name (element-name type-name-or-type 'particle-type))
	 (type (element type-name-or-type 'particle-type))
	 (cprt-type (element-parameter type 'concentration-particle-type)))
    (when type
      (format t "(particle-type-def~%")
      (format t "  `(~a~%" type-name)
      (format t "        (class . ~s)~%" (particle-type-class type))

      (case (particle-type-class type)
	(:markov
	 (format t "       (STATES . ~A)~%" (element-parameter type 'STATES))
	 (format t "       (OPEN-STATES . ~A)~%" (element-parameter type 'open-STATEs))
	 (format t "       (STATE-TRANSITIONS . ~%")
	 (format t "          ~a)~%" (markov-particle-type-STATE-VOLTAGE-TRANSITION-FUNCTIONS-string type)))
	 
	((:hh-ext :hh-ext-old)
	 (format t "        (valence . ~a)~%" (particle-type-z type))
	 (format t "        (gamma . ~a)~%" (particle-type-gamma type))
	 (format t "        (base-rate . ~a)~%" (particle-type-base-rate type))
	 (format t "        (v-half . ~a)~%" (particle-type-v-half type))
	 (format t "        (tau-0 . ~a)~%" (particle-type-tau-0 type))
	 (format t "        (IGNORE-TAU-VOLTAGE-DEPENDENCE . ~a)~%" (particle-type-IGNORE-TAU-VOLTAGE-DEPENDENCE type))))

      (when (and (element-parameter type 'linear-markov-n)
		 (element-parameter type 'linear-markov-m))
	(format t "        (linear-markov . ~A)~%" (list (element-parameter type 'linear-markov-n)
							 (element-parameter type 'linear-markov-m))))
      (when (element-parameter type 'alpha_0)
	(format t "        (alpha_0 . ~a)~%"	(element-parameter type 'alpha_0)))
      (when (element-parameter type 'beta_0)
	(format t "        (beta_0 . ~a)~%"	(element-parameter type 'beta_0)))
      
      (format t "        (reference-temp . ~a)~%" (particle-type-reference-temp type))
      (when (element-parameter type 'FIXED-BOLTZMANN-REFERENCE-TEMPERATURE)
	(format t "        (FIXED-BOLTZMANN-REFERENCE-TEMPERATURE . ~a)~%"
		(element-parameter type 'FIXED-BOLTZMANN-REFERENCE-TEMPERATURE)))
      (format t "        (q10 . ~a)~%" (particle-type-q10 type))
      (when cprt-type
	(format t "        (concentration-particle-type . ~a)~%" (element-name cprt-type)))
      (element-document-extras type)
      (format t "   ))~%~%")
      (when cprt-type (document-conc-particle-type cprt-type)))))

(defun print-particle-types ()
  (PRINT-MODEL-PARAMETERS "particle-type"))

(defun print-particles ()
  (PRINT-MODEL-PARAMETERS "particle"))

(defun print-particle (prt)
  (let ((prt (element prt 'particle)))
    (when prt
      (format t "Particle ~a: ~A, cell ~a~A~%"
	      (particle-name prt)
	      (particle-type-name (particle-type prt))
	      (cell-name (node-cell (particle-vnode-point prt)))
	      (if *simulation-initialized*
		  (format nil ", State ~,2e @ ~,2e ms"
			  (particle-state-n+1-double prt)
			  *real-time*)
		  ""))
      (when (element-parameter prt 'initial-state)
	(format t "   Explicit Initial Conditions: ~A~%" (element-parameter prt 'initial-state)))))
    (format t "~%"))

      

(defun convert-p-type-class-sym (sym)
  (case sym
    ((markov :markov) :markov)
    ((hh :hh) :hh)
    ((hh-ext :hh-ext) :hh-ext)
    ((hh-ext-old :hh-ext-old) :hh-ext-old)))

;; CREATE-PARTICLE-TYPE If the type was already made, this will update the type according to the
;; current description loaded in particle type parameter library.
(defun create-particle-type (type-symbol &optional actual-type-symbol update-parameters)
  (typecase type-symbol
    (string (setq type-symbol (intern type-symbol)))
    (particle-type (setq type-symbol (element-name type-symbol))))
  (let* ((type (unless actual-type-symbol
		 (if (particle-type-p type-symbol)
		     type-symbol
		     (gethash (string type-symbol) (PARTICLE-TYPE-HASH-TABLE)))))
	 (model (type-symbol-model 'particle-type))
	 (original-parameters (retrieve-model-parameters-from-library type-symbol model))
	 (parent-type-symbol (get-a-value 'parent-type original-parameters)))
    (unless (and type (not update-parameters))
      (when (eq parent-type-symbol type-symbol)
	(sim-error (format nil "~A has a recursive parent type!" parent-type-symbol)))
      (unless original-parameters (sim-error (format nil "Don't know anything about particle type ~A!" type-symbol)))
      (when actual-type-symbol (setq type-symbol actual-type-symbol))
      (unless type (setq type (if parent-type-symbol
				  (create-PARTICLE-TYPE parent-type-symbol type-symbol update-parameters)
				  (make-PARTICLE-TYPE :name (string type-symbol)))))
      (setq original-parameters (update-element-parameters-with-new-parameters original-parameters type))    
      (when parent-type-symbol (push-element-parameter type 'parent-types parent-type-symbol))
      
      (setf (particle-type-class type) (convert-p-type-class-sym (get-a-value 'class original-parameters)))
      (let* ((IGNORE-Tau-VOLTAGE-DEPENDENCE (get-a-value 'IGNORE-Tau-VOLTAGE-DEPENDENCE original-parameters))
	     (base-rate-undefined (or (get-a-value 'base-rate-undefined original-parameters)
				      IGNORE-Tau-VOLTAGE-DEPENDENCE
				      (if parent-type-symbol
					  (particle-type-IGNORE-Tau-VOLTAGE-DEPENDENCE type)
					  (not (assoc 'base-rate original-parameters))))))
	(case (particle-type-class type)
	  (:markov (set-markov-particle-type-parameters type original-parameters))
	  ((:hh-ext ::hh-ext-old)
	   (cond-every
	    ((assoc 'valence original-parameters)
	     (setf (particle-type-z type) (get-a-sf-value 'valence original-parameters)))
	    ((or base-rate-undefined iGNORE-Tau-VOLTAGE-DEPENDENCE)
	     (setf (particle-type-gamma type) 0.0))
	    ((assoc 'gamma original-parameters)
	     (setf (particle-type-gamma type) (get-a-sf-value 'gamma original-parameters)))
	    ((assoc 'v-half original-parameters)
	     (setf (particle-type-v-half type) (get-a-sf-value 'v-half original-parameters)))
	    ((assoc 'tau-0 original-parameters)
	     (setf (particle-type-tau-0 type) (get-a-sf-value 'tau-0 original-parameters)))
	    (base-rate-undefined (setf (particle-type-base-rate type)  1.0))
	    ((assoc 'base-rate original-parameters)
	     (setf (particle-type-base-rate type) (get-a-sf-value 'base-rate original-parameters)))
	    (t (element-parameter type (or (get-a-value 'tau_0_applied_to original-parameters) :all_kinetics))))))
	(cond-every
	 ((assoc 'linear-markov original-parameters)
	  (element-parameter type 'linear-markov-n (round (car (get-a-value 'linear-markov original-parameters))))
	  (element-parameter type 'linear-markov-m (round (cadr (get-a-value 'linear-markov original-parameters))))
	  (element-parameter type 'linear-markov t))
	 ((assoc 'alpha_0 original-parameters)
	  (element-parameter type 'alpha_0 (s-flt (get-a-value 'alpha_0 original-parameters))))
	 ((assoc 'beta_0 original-parameters)
	  (element-parameter type 'beta_0 (s-flt (get-a-value 'beta_0 original-parameters))))
	 
	 ((assoc 'concentration-particle-type original-parameters)
	  (let ((cprt-type (create-conc-particle-type (get-a-value 'concentration-particle-type original-parameters))))
	    (element-parameter cprt-type 'reference-particle-type type)
	    (element-parameter type 'concentration-particle-type cprt-type)))

	 ((assoc 'v-half-shift-particle-type original-parameters)
	  (element-parameter type 'v-half-shift-particle-type
			     (create-particle-type (get-a-value 'v-half-shift-particle-type original-parameters)))
	  (element-parameter type 'v-half-shift-magnitude
			     (s-flt (or (get-a-value 'v-half-shift-magnitude original-parameters) 0.0))))
	 ((assoc 'Fixed-boltzmann-reference-temperature original-parameters)
	  (element-parameter
	   type 'Fixed-boltzmann-reference-temperature
	   (get-a-sf-value 'Fixed-boltzmann-reference-temperature original-parameters))
	  (element-parameter type 'use-Fixed-boltzmann-reference-temperature t))
	 ((or base-rate-undefined IGNORE-Tau-VOLTAGE-DEPENDENCE)
	  (setf (particle-type-IGNORE-Tau-VOLTAGE-DEPENDENCE type) t))
	 ((assoc 'alpha-function original-parameters)
	  (setf (particle-type-alpha-function type)
		(let ((alpha-function (get-a-value 'alpha-function original-parameters)))
		  (if (consp alpha-function) alpha-function (extract-function-from-atom alpha-function)))))
	 ((assoc 'beta-function original-parameters)
	  (setf (particle-type-beta-function type)
		(let ((beta-function (get-a-value 'beta-function original-parameters)))
		  (if (consp beta-function) beta-function (extract-function-from-atom beta-function)))))
	 ((assoc 'ss-function original-parameters)
	  (setf (particle-type-ss-function type)
		(let ((ss-function (get-a-value 'ss-function original-parameters)))
		  (if (consp ss-function) ss-function (extract-function-from-atom ss-function)))))
	 ((assoc 'tau-function original-parameters)
	  (setf (particle-type-tau-function type)
		(let ((tau-function (get-a-value 'tau-function original-parameters)))
		  (if (consp tau-function) tau-function (extract-function-from-atom tau-function)))))

	 ((assoc 'tau-coefficient original-parameters)
	  (setf (particle-type-tau-coefficient type) (get-a-value 'tau-coefficient original-parameters)))

	 ((or (assoc 'qten original-parameters) (assoc 'q10 original-parameters))
	  (setf (particle-type-q10 type) (s-flt (cdr (or (assoc 'qten original-parameters)
							 (assoc 'q10 original-parameters))))))
	 ((assoc 'reference-temp original-parameters)
	  (setf (particle-type-reference-temp type) (get-a-sf-value 'reference-temp original-parameters))))

	(setf (gethash (string type-symbol) (PARTICLE-TYPE-HASH-TABLE)) type)

	;; Do this after the setf of the hash-table since we need ELEMENT to work.
	(make-v-particle-arrays type)
	
	(unless (or (member (particle-type-class type) '(:hh-ext ::hh-ext-old :markov))
		    (and (particle-type-alpha-function type) (particle-type-beta-function type))
		    (and (particle-type-tau-function type) (particle-type-ss-function type)))
	  (sim-error (format nil "Part type ~A needs a/b *or* tau/ss functions!" type)))))
    (setq *particle-type* type)
    type))


(defun get-particle-simple-name ()
  (loop for candidate from (max 1 *particle-simple-name-counter*)
	until (not (gethash candidate (particle-hash-table)))
	finally (return (setf *particle-simple-name-counter* candidate))))

(defun rename-particles-simple (&optional (particles (particles)))
  "Rename PARTICLES [default all particles in circuit] with simple integer names."
  (loop for seg in particles do
	(let ((name (get-particle-simple-name)))
	  (set-element-name seg name 'particle))))



(defun create-particle (channel type &optional particle dummy-particle)
  (let* (*print-pretty*
	 (type (create-particle-type type))
	 (particle-name (if particle (particle-name particle)
			    (if dummy-particle (format nil "~A" (gensym))
				(if *use-simple-names*
				    (get-particle-simple-name)
					; (1+ (hash-table-count (PARTICLE-HASH-TABLE)))
				    (concatenate-strings (channel-name channel) "-" (particle-type-name type))))))
	 (nd (if dummy-particle
		 (create-node nil :dummy-node t)
		 (or (channel-pre-synaptic-node channel) (channel-node channel))))
	 (prt (or particle (make-particle :name particle-name :type type :channel channel :vnode-point nd))))
    (cond-every
     ((element-parameter type 'concentration-particle-type)
      (setf (particle-conc-particle prt)
	    (create-conc-particle channel (element (element-parameter type 'concentration-particle-type))
				  dummy-particle)))
     ((element-parameter type 'v-half-shift-particle-type) (create-v-half-shift-particle prt type)))
    (case (particle-type-class type)
      (:markov
       (setf (particle-state-arrays prt)
	     (sequence-to-gen-array
	      (loop for i from 1 to (particle-type-number-of-states type) collect (make-particle-double-floats))))))
    (setf (gethash particle-name (PARTICLE-HASH-TABLE)) prt)
    (unless dummy-particle
      (push prt (node-elements nd))
      (reorder-particles-of-type (particle-type prt) prt)
      (setf (node-has-v-dep-element nd) t))
    (setq *particle* prt)
    prt))


(defun markov-particle-p (particle)
  (let ((particle (element particle 'particle)))
    (when particle
      (eq (particle-type-class (particle-type particle)) :markov))))
	  

(defun create-v-half-shift-particle (prt type)
  (element-parameter prt 'v-half-shift-particle
		     (create-particle channel (element-parameter type 'v-half-shift-particle-type))))


(proclaim '(inline particle-concentration-particle))
(defun particle-concentration-particle (prt)
  (particle-conc-particle prt))

(proclaim '(inline particle-v-index particle-v-rem))
(defun particle-v-index (prt)
  (node-prt-v-index (particle-vnode-point prt)))

(defun particle-v-rem (prt)
  (node-prt-v-index-rem (particle-vnode-point prt)))

(defun particle-look-up-voltages ()
  (loop for voltage single-float from (the sf *particle-look-up-table-min-voltage*)
	by (the sf *particle-look-up-table-precision*)
	for array-index fixnum from 0 to (1- (the fn *particle-look-up-table-length*)) 
	collect voltage))

(defun particle-look-up-voltages-by-1 ()
  (list-of-nums *particle-look-up-table-length* *particle-look-up-table-min-voltage*))

(defun particle-look-up-table-max-voltage ()
  (+ *particle-look-up-table-min-voltage*
     (* *particle-look-up-table-precision* *particle-look-up-table-length*)))


(defun v-inf-particle-plot-list (particle-type &optional (power 1))
  (let ((particle-type (element particle-type 'particle-type)))
    (when particle-type
      (case (particle-type-class particle-type)
	(:markov (v-inf-markov-particle-plot-lists particle-type))
	(t
	 (let ((linear-markov-n (element-parameter particle-type 'linear-markov-n))
	       (linear-markov-m (element-parameter particle-type 'linear-markov-m)))
	   (unless (arrayp (particle-type-inf-array particle-type)) (make-v-particle-arrays particle-type))
	   (loop for voltage  from *particle-look-up-table-min-voltage* by *particle-look-up-table-precision*
		 for i from 0 to (1- (length  (particle-type-inf-array particle-type)))
		 collecting voltage into volts
		 collecting
		 (let* ((p (aref (particle-type-inf-array particle-type) i))
			(p-cumulative
			 (if linear-markov-n (linear-markov-binomial p linear-markov-n linear-markov-m) p)))
		   (if (= power 1) p-cumulative (expt p-cumulative power)))
		 into inf
		 finally (return (list volts inf)))))))))

(defun v-tau-particle-plot-list (particle-type)
  (let ((particle-type (element particle-type 'particle-type)))
    (when particle-type
      (case (particle-type-class particle-type)
	(:markov)
	(t
	 (unless (arrayp (particle-type-tau-array particle-type)) (make-v-particle-arrays particle-type))
	 (loop for voltage  from *particle-look-up-table-min-voltage* by *particle-look-up-table-precision*
	       for i from 0 to (1- (length  (particle-type-inf-array particle-type)))
	       collecting voltage into volts
	       collecting (aref (particle-type-tau-array particle-type) i) into tau
	       finally (return (list volts tau))))))))

(proclaim '(inline gating-particle-beta-prime-function gating-particle-alpha-prime-function))
(defun gating-particle-alpha-prime-function (base-rate gamma v-half voltage &key arg-constant (z 0.0) (temperature 1.0))
  (declare (optimize (speed 3) (space 0))
	   (single-float base-rate gamma v-half voltage z Temperature))
  (* base-rate 
     (the sf (exp-w-limits (if arg-constant
			       (* (the sf arg-constant) (- voltage v-half) gamma)
			     (* (/ (* z 1.0e-3 FoverR) Temperature)
				(- voltage v-half) gamma))))))
			      

(defun gating-particle-beta-prime-function (base-rate gamma v-half voltage &key arg-constant (z 0.0) (temperature 1.0))
  (declare (optimize (speed 3) (space 0))
	   (single-float base-rate gamma v-half voltage z Temperature))
  (* base-rate 
     (the sf (exp-w-limits (if arg-constant
			       (* (the sf arg-constant) (- voltage v-half) (- gamma 1.0))
			       (* (/ (* z 1.0e-3 FoverR) Temperature) (- voltage v-half) (- gamma 1.0)))))))

(defun gating-particle-alpha-beta-values (type base-rate gamma v-half voltage
					       &key (arg-constant 0.0) (alpha_0 0.0) (beta_0 0.0) (tau_0 0.0)
					       (tau_0_role :all_kinetics) ; :v-dep_kinetics
					       always-calculate-rate)
  (declare (optimize (speed 3) (space 0))
	   (single-float base-rate gamma v-half voltage alpha_0 beta_0 tau_0 arg-constant))
  (cond ((and type (particle-type-IGNORE-TAU-VOLTAGE-DEPENDENCE type) (= tau_0 0) (or (> alpha_0 0) (> beta_0 0)))
         (values alpha_0 beta_0))
        ((and type (particle-type-IGNORE-TAU-VOLTAGE-DEPENDENCE type) (not always-calculate-rate))
         (if (= tau_0 0)
             (sim-error
              (format nil "Particle type ~A has zero tau_0, alpha_0 and beta_0, and IGNORE-TAU-VOLTAGE-DEPENDENCE set!"
                      type))
             (values (/ 1 (* 2 tau_0)) (/ 1 (* 2 tau_0)))))
	(t (let ((alpha-prime 
		  (gating-particle-alpha-prime-function base-rate gamma v-half voltage :arg-constant arg-constant))
		 (beta-prime 
		  (gating-particle-beta-prime-function base-rate gamma v-half voltage :arg-constant arg-constant)))
	     (declare (single-float alpha-prime beta-prime))
	     (case tau_0_role
	       (:all_kinetics
		(let* ((alpha (+ alpha-prime alpha_0))
		       (beta (+ beta-prime beta_0))
		       (denominator (+ (* tau_0 (+ alpha beta)) 1)))
		  (values
		   (/ alpha denominator)
		   (/ beta denominator))))
	       (:v-dep_kinetics
		(let ((denominator (+ (* tau_0 (+ alpha-prime beta-prime)) 1)))
		  (values
		   (+ alpha_0 (/ alpha-prime denominator))
		   (+ beta_0 (/ beta-prime denominator))))))))))

(proclaim '(inline gating-particle-beta-function gating-particle-alpha-function))
(defun gating-particle-alpha-function (type base-rate gamma v-half voltage &key arg-constant (z 0.0) (temperature 0.0)
					    (alpha_0 0.0) (beta_0 0.0) (tau_0 0.0) always-calculate-rate
					    (tau_0_role :all_kinetics) ; :v-dep_kinetics
					    )
  (declare (optimize (speed 3) (space 0))
	   (single-float base-rate gamma v-half voltage z Temperature alpha_0 beta_0 tau_0))
  (cond ((and type (particle-type-IGNORE-TAU-VOLTAGE-DEPENDENCE type) (= tau_0 0) (or (> alpha_0 0) (> beta_0 0)))
	 alpha_0)
	((and type (particle-type-IGNORE-TAU-VOLTAGE-DEPENDENCE type) (not always-calculate-rate))
	 (if (= tau_0 0)
	     (sim-error
	      (format nil "Particle type ~A has zero tau_0, alpha_0 and beta_0, and IGNORE-TAU-VOLTAGE-DEPENDENCE set!"
		      type))
	     (/ 1 tau_0)))
	(t (let ((alpha-prime
		  (gating-particle-alpha-prime-function base-rate gamma v-half voltage
							:arg-constant arg-constant :z z :temperature temperature)))
	     (if (or (and type (eq (particle-type-class type) :hh-ext-old)) (= tau_0 0))
		 (+ alpha-prime alpha_0)
		 (let ((beta-prime
			(gating-particle-beta-prime-function base-rate gamma v-half voltage
							     :arg-constant arg-constant :z z :temperature temperature)))
		   (case tau_0_role
		     (:all_kinetics
		      (/ (+ alpha-prime alpha_0)
			 (+ (* tau_0 (+ alpha-prime alpha_0 beta-prime beta_0)) 1)))
		     (:v-dep_kinetics
		      (+ alpha_0 (/ alpha-prime (+ (* tau_0 (+ alpha-prime beta-prime)) 1)))))))))))
		      

(defun gating-particle-beta-function (type base-rate gamma v-half voltage &key arg-constant (z 0.0) (temperature 0.0)
					   (alpha_0 0.0) (beta_0 0.0) (tau_0 0.0) always-calculate-rate
					   (tau_0_role :all_kinetics)) ; :v-dep_kinetics
  (declare (optimize (speed 3) (space 0))
	   (single-float base-rate gamma v-half voltage z Temperature alpha_0 beta_0 tau_0))
  (cond ((and type (particle-type-IGNORE-TAU-VOLTAGE-DEPENDENCE type) (= tau_0 0) (or (> alpha_0 0) (> beta_0 0)))
	 beta_0)
	((and type (particle-type-IGNORE-TAU-VOLTAGE-DEPENDENCE type) (not always-calculate-rate))
	 (if (= tau_0 0)
	     (sim-error
	      (format nil "Particle type ~A has zero tau_0, alpha_0 and beta_0, and IGNORE-TAU-VOLTAGE-DEPENDENCE set!"
		      type))
	   (/ 1 tau_0)))
	(t
	 (let ((beta-prime
		(gating-particle-beta-prime-function base-rate gamma v-half voltage
						     :arg-constant arg-constant :z z :temperature temperature)))
	   (if (or (and type (eq (particle-type-class type) :hh-ext-old)) (= tau_0 0))
	       (+ beta-prime beta_0)
	     (let ((alpha-prime
		    (gating-particle-alpha-prime-function base-rate gamma v-half voltage
							  :arg-constant arg-constant :z z :temperature temperature)))
	       (case tau_0_role
		 (:all_kinetics
		  (/ (+ beta-prime beta_0)
		     (+ (* tau_0 (+ alpha-prime alpha_0 beta-prime beta_0)) 1)))
		 (:v-dep_kinetics
		  (+ beta_0 (/ beta-prime (+ (* tau_0 (+ alpha-prime beta-prime)) 1)))))))))))
		   

(defun get-particle-look-up-table-sf-array ()
    (make-array *particle-look-up-table-length* :element-type 'single-float))

(defun get-particle-look-up-table-df-array ()
    (make-array *particle-look-up-table-length* :element-type 'double-float))

(defun check-particle-type-inf-and-tau-array-characteristics (type)
    (declare (optimize (safety 1) (speed 3) (space 0)))
    ;; If the precision of the look up tables has been changed, we may need to adjust either the temporary alpha and
    ;; beta arrays, and the particle type alpha and beta arrays. This also creates the first instances of the
    ;; particle-type arrays.
    (unless (and (arrayp (particle-type-inf-array type))	(arrayp (particle-type-tau-array type))
		 (= (length (the vec-flt (particle-type-inf-array type))) *particle-look-up-table-length*))
      (setf (particle-type-inf-array type) (get-particle-look-up-table-sf-array)
	    (particle-type-tau-array type) (get-particle-look-up-table-sf-array)))
    nil)

(defun make-v-particle-arrays (type)
  (let ((type (element type 'particle-type)))
    (when type
      (case (particle-type-class type)
	(:markov (fill-markov-transition-array type))
	(t (make-two-state-v-particle-arrays type))))))

(defun make-two-state-v-particle-arrays (type)
  (declare (optimize (safety 1) (speed 3) (space 0)))
  (let ((type (element type 'particle-type)))
    (when type
      (check-particle-type-inf-and-tau-array-characteristics type)
      (let ((alpha_0 (or (element-parameter type 'alpha_0) 0.0))
	    (beta_0 (or (element-parameter type 'beta_0) 0.0))
	    (tau_0_role (or (element-parameter type 'tau_0_applied_to) :all_kinetics))
	    (tau_0 (particle-type-tau-0 type))
	    (base-rate (particle-type-base-rate type))
	    (arg-constant		; This just saves crunching in the loop below. The factor of
					; 1.0e-3 is because the voltage units below are millivolts.
	     (/ (* 1.0e-3 FoverR (particle-type-z type))
		(the sf (or (and (element-parameter type 'use-Fixed-boltzmann-reference-temperature)
				 (+ 273.16 (the sf (element-parameter type 'Fixed-boltzmann-reference-temperature))))
			    *Temperature*))))
	    (q10-tau-factor (element-q10-tau-factor type)))
	(declare (type single-float arg-constant q10-tau-factor alpha_0 beta_0))
	(case (particle-type-class type)
	  ((:hh-ext-old :hh-ext)
	   (when (= alpha_0 beta_0 tau_0 (particle-type-base-rate type) 0)
	     (sim-error (format nil "~A has alpha_0, beta_0, base rate and tau-0 equal to 0!!" type)))))

	;; Now loop over the voltage range.
	(loop for voltage single-float from *particle-look-up-table-min-voltage*
	      by (the sf *particle-look-up-table-precision*)
	      for array-index fixnum from 0 to (1- (the fn *particle-look-up-table-length*)) do
	      (case (particle-type-class type)
		((:hh-ext-old :hh-ext)
		 (put-values-for-hh-tau-and-inf-array type base-rate voltage arg-constant alpha_0 beta_0 tau_0
						      array-index q10-tau-factor tau_0_role))
		(t (put-values-for-non-hh-tau-and-inf-array type voltage array-index q10-tau-factor))))))))
		    


(defun put-values-for-hh-tau-and-inf-array (type base-rate voltage arg-constant alpha_0 beta_0 tau_0
						 array-index q10-tau-factor tau_0_role)
  (declare (optimize (safety 1) (speed 3) (space 0))
	   (type single-float arg-constant q10-tau-factor alpha_0 beta_0 voltage)
	   (type fixnum array-index))
  (when (and (not (particle-type-IGNORE-Tau-VOLTAGE-DEPENDENCE type))
	     (= base-rate 0))
    (sim-error (format nil "~A has IGNORE-Tau-VOLTAGE-DEPENDENCE nil and (= base-rate 0)" type)))
  (multiple-value-bind (alpha beta)
      (gating-particle-alpha-beta-values
       type (if (= base-rate 0) 1.0 base-rate) (particle-type-gamma type) (particle-type-v-half type) voltage
       :arg-constant arg-constant :alpha_0 alpha_0 :beta_0 beta_0
       :tau_0_role tau_0_role
       :tau_0 (case (particle-type-class type)
		(:hh-ext tau_0)
		(:hh-ext-old 0.0))	; If limiting tau_0, consider it below.
       :always-calculate-rate t)
    ;; Do Q10 factor and tau limiting (if necessary) directly on the tau array.	
    (let* ((base-tau (/ 1.0 (+ (the sf alpha) (the sf beta))))
	   (inf (* alpha base-tau)))
      (declare (single-float alpha beta base-tau inf))
      (when (< inf 0) (sim-error (format nil "Particle type ~A has negative SS value @~AmV!" type voltage)))
      (setf (aref (the vec-flt (particle-type-tau-array type)) array-index)
	    (* q10-tau-factor
	       (if (particle-type-IGNORE-Tau-VOLTAGE-DEPENDENCE type)
		   tau_0
		 (case (particle-type-class type)
		   (:hh-ext-old (max tau_0 base-tau))
		   (:hh-ext base-tau)
		   (t 0.0)))))		; Dummy
      (setf (aref (the vec-flt (particle-type-inf-array type)) array-index) inf))))

(defun put-values-for-non-hh-tau-and-inf-array (type voltage array-index q10-tau-factor)
  (declare (optimize (safety 1) (speed 3) (space 0))
	   (type single-float q10-tau-factor voltage)
	   (type fixnum array-index))
  ;; For non-HH-EXT class particle types, either the alpha and beta rate functions or the
  ;; tau and steady-state functions must be supplied as part of the type definitions.
  (let ((alpha 0.0) (beta 0.0) (tau 0.0) (ss 0.0)
	(alpha-beta-functions-p (and (particle-type-alpha-function type) (particle-type-beta-function type))))
    (declare (type single-float alpha beta tau ss))
    (when alpha-beta-functions-p
      (setq alpha (funcall (the (function (single-float) single-float) (particle-type-alpha-function type)) voltage)
	    beta (funcall (the (function (single-float) single-float) (particle-type-beta-function type)) voltage)))
    (setq tau (if alpha-beta-functions-p
		(/ (particle-type-tau-coefficient type) (+ alpha beta))
		(funcall (the (function (single-float) single-float) (particle-type-tau-function type)) voltage)))
    (setq ss (if alpha-beta-functions-p
	       (* alpha tau)
	       (funcall (the (function (single-float) single-float) (particle-type-ss-function type)) voltage)))
    (setf (aref (the vec-flt (particle-type-tau-array type)) array-index) (* q10-tau-factor tau))
    (setf (aref (the vec-flt (particle-type-inf-array type)) array-index) (rectify ss))))


(defun make-needed-v-particle-arrays (&optional all)
  (mapcar 'make-v-particle-arrays (if all (particle-types) (delete-duplicates *make-needed-v-particle-arrays*)))
  (setq *make-needed-v-particle-arrays* nil))

;; Make sure that there is no voltage in which we get a singular markov rate matrix (e.g. rates = 0)
(defvar *overall-minimum-particle-rate-constant* 0.0d0 ; 1.0d-5
  )

(defun v-function-array (function-or-form
			 &key (min-voltage *particle-look-up-table-min-voltage*)
			 (voltage-increment *particle-look-up-table-precision*)
			 (array-length *particle-look-up-table-length*))
  (let* ((array (make-array array-length :element-type 'double-float))
	 (function-or-form-is-function (extract-function-from-atom function-or-form))
	 (funspec (if function-or-form-is-function
		      (extract-function-from-atom function-or-form)
		    (typecase function-or-form
			      (number nil)
			      (cons (extract-function-from-atom (car function-or-form)))))))
    (loop for voltage single-float from min-voltage by voltage-increment
	  for array-index fixnum from 0 to (1- array-length) 
	  do (setf (aref array array-index)
		   (d-flt (if (not funspec)
			      function-or-form ; It's a constant 
			    (if function-or-form-is-function
				(funcall (the function funspec) voltage)
			      (apply (the compiled-function funspec)
				     (cons voltage (cddr function-or-form))))))))
    array))

;; backward compatibility 5/12/99
(defun fill-v-function-array (function-or-form
			      &key (min-voltage *particle-look-up-table-min-voltage*)
			      (voltage-increment *particle-look-up-table-precision*)
			      (array-length *particle-look-up-table-length*))
  (v-function-array function-or-form
		    :min-voltage min-voltage
		    :voltage-increment voltage-increment
		    :array-length array-length))

      

(defun particle-type-v-function-array (type function-or-form
					    &key
					    (rate-p t) ; Otherwise, assume that this is for tau
					    (min-voltage *particle-look-up-table-min-voltage*)
					    (voltage-increment *particle-look-up-table-precision*)
					    (array-length *particle-look-up-table-length*))
  (declare (optimize (safety 1) (speed 3) (space 0)))
  (let ((q10-factor (if type
			(d-flt (if rate-p (element-q10-rate-factor type)
				   (element-q10-tau-factor type)))
			1.0d0))
	(array (v-function-array
		function-or-form
		:min-voltage min-voltage
		:voltage-increment voltage-increment
		:array-length array-length)))
    
    (declare (type double-float q10-factor))
    ;; Now adjust for q-ten.
    (loop for array-index fixnum from 0 to (1- array-length) 
	  do (setf (aref array array-index)
		   (let ((val (the df (* q10-factor (aref array array-index)))))
		     (if rate-p (max (the df *overall-minimum-particle-rate-constant*) val)
			 val))))
    array))


(defun plot-particle-type-rates (type &key concentration)
  (let ((type (element type 'particle-type)))
    (when type
      (case (particle-type-class type)
	(:markov (plot-markov-particle-type-rates type :separate-plots nil :use-menu t :concentration concentration))
	(t (plot-two-state-particle-type-rates type :separate-plots nil :user-menu t :concentration concentration))))))

(defun plot-particle-type-steady-states (prt-types-powers channel-type overlay new-plot)
  (loop for prt-type-power in prt-types-powers
	when (markov-p (car prt-type-power))
	do (plot-markov-particle-type-steady-state (car prt-type-power))
	else nconcing
	(loop for order from 1 to (cdr prt-type-power) by (max 1 (1- (cdr prt-type-power)))
	      collecting (v-inf-particle-plot-list (car prt-type-power) order))
	into volts-inf
	and nconcing
	(loop for order from 1 to (cdr prt-type-power) by (max 1 (1- (cdr prt-type-power)))
	      collecting (if (> order 1)
			     (format nil "~A-eff (^~d)" (element-name (car prt-type-power)) order)
			     (element-name (car prt-type-power))))
	into labels
	finally
	(when volts-inf
	  (plot-xy-data volts-inf labels
			:title (if channel-type
				   (format nil "~a Channel Particles: Steady State(V)"
					   (element-name channel-type))
				 (format nil "~a Particle Type: Steady State(V)"
					 (element-name (caar prt-types-powers))))
			:overlay (and overlay (not new-plot)) :prompt-for-overlay t
			:preserve-window-dimensions t
			:width *particle-kinetics-plot-width* :height *particle-kinetics-plot-height*
					; :reference-ticks-to-origin t
			:x-min  *particle-look-up-table-min-voltage*
			:x-max (particle-look-up-table-max-voltage)
			:x-origin  *particle-look-up-table-min-voltage*
			:x-inc 20.0 :x-origin-tick t :x-label "mV"
			:y-label-vertical-position :upper-right :x-are-fns t ; :include-y-tick-at-0 nil
			:y-inc 0.25 :y-max 1.0 :y-min 0.0 :y-origin-tick t :y-label "Steady State"
			:comment-position :upper-right
			:comment (when *include-channel-type-comment-in-particle-plots*
				   (channel-type-particle-plot-comment channel-type))))))


(defun plot-particle-type-time-constants (prt-types-powers channel-type overlay new-plot)
  (loop for prt-type-power in prt-types-powers
	unless (markov-p (car prt-type-power))
	collect (v-tau-particle-plot-list (car prt-type-power)) into volts-tau
	and
	collect (element-name (car prt-type-power)) into labels
	finally
	(when volts-tau
	  (plot-xy-data volts-tau labels
			:title (if channel-type
				   (format nil "~a Channel Particles: Tau(V)" (element-name channel-type))
				 (format nil "~a Particle Type: Tau(V)" (element-name (caar prt-types-powers))))
			:preserve-window-dimensions t
			:width *particle-kinetics-plot-width* :height *particle-kinetics-plot-height*			
			:overlay (and overlay (not new-plot)) :prompt-for-overlay t
					; :reference-ticks-to-origin t
			:x-origin *particle-look-up-table-min-voltage*
			:x-min *particle-look-up-table-min-voltage*
			:x-max (particle-look-up-table-max-voltage) :x-inc 20.0 :x-origin-tick t :x-label "mV"
			:x-are-fns t	; :include-y-tick-at-0 nil
			:y-origin-tick t :y-label-vertical-position :upper-right :y-min 0.0 :y-label "ms"))))

(defun plot-v-markov-particle (type &key channel-type (what '(:steady_state :tau))
				    (conc-steps 6) (log-conc-scale t) (min-conc 1.0e-5) (max-conc 1.0e0))
  (let ((dummy1 (or (element-parameter type 'conc-particle-plot-min-conc) min-conc))
	(dummy2 (or (element-parameter type 'conc-particle-plot-max-conc) max-conc))
	(dummy3 (or (element-parameter type 'conc-particle-plot-log-conc) log-conc-scale))
	(dummy4 (or (element-parameter type 'conc-particle-plot-conc-steps) conc-steps)))
    (when (element-parameter type 'concentration-particle-type)
      (choose-variable-values
       '((dummy1 "Minimum concentration [mM]" :float)
	 (dummy2 "Maxmum concentration [mM]" :float)
	 (dummy4 "Number of concentration steps" :integer)
	 (dummy3 "Logarithmic steps" :boolean))
       :title (format nil "Concentration parameters for ~A plot" (element-name type))))
    (let* ((step (/ (- (if dummy3 (log dummy2 10) dummy2)
		       (if dummy3 (log dummy1 10) dummy1))
		    (max 1 (1- dummy4))))
	   (start-conc (if dummy3 (log dummy1 10) dummy1))
	   (stop-conc (if dummy3 (log dummy2 10) dummy2))
	   (conc-comment (if (= start-conc stop-conc)
			     (format nil "Concentration: ~,2emM" dummy1)
			     (format nil "Concentration: ~,2e - ~,2emM" dummy1 dummy2)))
	   (from-idx-to-idxs (when (or (member :all what) (member :alpha_&_beta what))
			       (markov-label-from-index-to-indexs-menu type))))
      (cond-every
       ((or (member :all what) (member :alpha_&_beta what))
	(if (element-parameter type 'concentration-particle-type)
	    (progn
	      (loop for i from (min start-conc stop-conc) to (max start-conc stop-conc)
		    by (if (= start-conc stop-conc) 1 step)
		    for count from 1 do
		    (plot-markov-particle-type-rates
		     type :prompt-for-overlay (= count 1)
		     :from-idx-to-idxs from-idx-to-idxs
		     :separate-plots nil :use-menu nil :concentration (if dummy3 (expt 10.0 i) i)))
	      (add-comment *twin* conc-comment))
	    (plot-markov-particle-type-rates type :separate-plots nil :from-idx-to-idxs from-idx-to-idxs)))
       ((or (member :all what) (member :steady_state what))
	(if (element-parameter type 'concentration-particle-type)
	    (progn
	      (loop for i from (min start-conc stop-conc) to (max start-conc stop-conc)
		    by (if (= start-conc stop-conc) 1 step)
		    for count from 1 do
		    (plot-markov-particle-type-steady-state type :channel-type channel-type
							    :prompt-for-overlay (= count 1)
							    :concentration (if dummy3 (expt 10.0 i) i)))
	      (add-comment *twin* conc-comment))
	    (plot-markov-particle-type-steady-state type :channel-type channel-type)))))
    (when (element-parameter type 'concentration-particle-type)
      (element-parameter type 'conc-particle-plot-min-conc dummy1)
      (element-parameter type 'conc-particle-plot-max-conc dummy2)
      (element-parameter type 'conc-particle-plot-log-conc dummy3)
      (element-parameter type 'conc-particle-plot-conc-steps dummy4))
    nil
    ))


	  

		

(defun plot-v-particles (prt-types-powers &key channel-type (what '(:steady_state :tau)) overlay new-plot
					  (log-conc-scale t)
					  (min-conc 1.0e-5)
					  (max-conc 1.0))
  (let ((what (coerce-to-list what))
	(channel-type (element channel-type 'channel-type))
	(prt-types-powers (typecase prt-types-powers
			    (cons prt-types-powers)
			    (t (list (cons prt-types-powers 1))))))
    (loop for prt-type-power in prt-types-powers
	  with prt-type  with power 
	  do  (setq prt-type (if (consp prt-type-power) (car prt-type-power) prt-type-power)
		    power (if (consp prt-type-power) (cdr prt-type-power) 1))
	  ; (format t "~A ~A ~a~%" prt-type-power prt-type power)
	  unless (markov-p prt-type)
	  collect (cons prt-type power) into non-markov-prt-types-powers
	  else do
	  (plot-v-markov-particle prt-type :channel-type channel-type :what what
				  :log-conc-scale log-conc-scale :min-conc min-conc :max-conc max-conc)
	  finally 
	  (when (or (member :all what) (member :alpha_&_beta what))
	    (loop for prt-type-power in non-markov-prt-types-powers do (plot-particle-type-rates (car prt-type-power))))
	  (when (or (member :all what) (member :steady_state what))
	    (plot-particle-type-steady-states non-markov-prt-types-powers channel-type overlay new-plot))
	  (when (or (member :all what) (member :tau what))
	    (plot-particle-type-time-constants non-markov-prt-types-powers channel-type overlay new-plot)))))
	

#|
3/20/98 Strange problems all of a sudden (after misc file editing) - during compile:

In: DEFUN PLOT-TWO-STATE-PARTICLE-TYPE-RATES
  (GATING-PARTICLE-ALPHA-FUNCTION TYPE (PARTICLE-TYPE-BASE-RATE TYPE)
   (PARTICLE-TYPE-GAMMA TYPE) (PARTICLE-TYPE-V-HALF TYPE) ...)
--> BLOCK COND IF COND IF COND IF PROGN LET 
--> GATING-PARTICLE-ALPHA-PRIME-FUNCTION BLOCK * THE EXP-W-LIMITS IF * * * 
==>
  (/ (* Z 0.001 FOVERR) TEMPERATURE)
Warning: Lisp error during constant folding:


Error in function C::DO-CALL:
   Condition slot is not bound: CONDITIONS::OPERATION

Solved by local notinline of these functions. 
|#
(defun plot-two-state-particle-type-rates (type &key separate-plots user-menu concentration)
  (declare (ignore separate-plots user-menu concentration)
	   (notinline gating-particle-alpha-function gating-particle-beta-function))
  (let ((alpha_0 (or (element-parameter type 'alpha_0) 0.0))
	(beta_0 (or (element-parameter type 'beta_0) 0.0))
	(tau_0_role (or (element-parameter type 'tau_0_applied_to) :all_kinetics))
	(arg-constant (/ (* 1.0e-3 FoverR (particle-type-z type))
			 (or (and (element-parameter type 'use-Fixed-boltzmann-reference-temperature)
				  (+ 273.16 (element-parameter type 'Fixed-boltzmann-reference-temperature)))
			      *Temperature*)))
			     
	(tau_0 (particle-type-tau-0 type))
	(q10-rate-factor (element-q10-rate-factor type)))
    (loop for voltage from *particle-look-up-table-min-voltage* by *particle-look-up-table-precision*
	  for i from 0 to (1- (length (particle-type-inf-array type)))
	  collecting voltage into volts
	  collecting
	  (* q10-rate-factor
	     (case (particle-type-class type)
	       ((:hh-ext-old :hh-ext)
		(gating-particle-alpha-function
		 type (particle-type-base-rate type) (particle-type-gamma type) (particle-type-v-half type) voltage
		 :arg-constant arg-constant :alpha_0 alpha_0 :beta_0 beta_0 :tau_0 tau_0 :tau_0_role tau_0_role))
	       (t (if (and (particle-type-alpha-function type) (particle-type-beta-function type))
		      (funcall (particle-type-alpha-function type) voltage)
		    (let ((tau (funcall (particle-type-tau-function type) voltage))
			  (ss (funcall (particle-type-ss-function type) voltage)))
		      (/ ss tau))))))
	  into alphas
	  collecting
	  (* q10-rate-factor
	     (case (particle-type-class type)
	       ((:hh-ext-old :hh-ext)
		(gating-particle-beta-function
		 type (particle-type-base-rate type) (particle-type-gamma type) (particle-type-v-half type) voltage
		 :arg-constant arg-constant :alpha_0 alpha_0 :beta_0 beta_0 :tau_0 tau_0 :tau_0_role tau_0_role))
	       (t (if (and (particle-type-alpha-function type) (particle-type-beta-function type))
		      (funcall (particle-type-beta-function type) voltage)
		    (let ((tau (funcall (particle-type-tau-function type) voltage))
			  (ss (funcall (particle-type-ss-function type) voltage)))
		      (/ (- 1 ss) tau))))))
	  into betas
	  finally
	  (plot-timed-data (list alphas betas) (list "Alpha" "Beta") volts
			   :title (format nil "~A Alpha and Beta" (element-name type))
			   :prompt-for-overlay t :preserve-window-dimensions t
			   :width *particle-kinetics-plot-width* :height *particle-kinetics-plot-height*
			   :y-label-vertical-position :upper-right   :x-are-fns t :include-y-tick-at-0 nil
			   :x-inc 20.0 :x-origin 0.0 :x-origin-tick t :x-label "mV"
			   :y-label "1/ms"
			   :x-min *particle-look-up-table-min-voltage* :x-max (particle-look-up-table-max-voltage)
			   :y-min 0))))

(defun print-particle-state (prt)
  (format t "part: ~a, state-n ~f, state-n+1 ~f~%" (particle-name prt) (particle-state-n+1-double prt)
	  (particle-state-n-double prt)))

(defun working-particles-p ()
  (and *active*
       (loop for prt being the hash-value of (particle-hash-table)
	     unless (or (channel-type-block (channel-type (particle-channel prt)))
			(channel-block (particle-channel prt)))
	     do (return t))))

(defun update-particle-type-q10 (type)
  (let ((type (element type 'particle-type)))
    (when type
      (setf (particle-type-q10-rate-factor type) (element-q10-rate-factor type)))))
  
(defun update-particle-type-q10s ()
  (loop for type in (PARTICLE-TYPEs) do (update-particle-type-q10 type)))



(defun setup-particles ()
  (setq *particle-type-list*
	(delete-duplicates
	 (loop for ch-type in *channel-type-list*
	       when (CHANNEL-TYPE-ACTIVE-P ch-type)
	       nconc (loop for prt-type-power in (channel-type-particle-types-and-powers (element ch-type 'channel-type))
			   collect (car prt-type-power))
	       into prt-types
	       and
	       nconc (loop for prt-type-power in (channel-type-particle-types-and-powers (element ch-type 'channel-type))
			   collect (element-parameter (car prt-type-power) 'v-half-shift-particle-type))
	       into prt-types
	       finally (return (no-nils prt-types))))))
	

(defun remove-particle-type-arrays (type)
  (remove-element-parameters type '(active-particle-array active-particle-array-length)))

(proclaim '(inline particle-block))
(defun particle-block (prt)
  (declare (ignore prt))
  nil)

(defun advance-particles ()
  (declare (optimize (safety 0) (speed 3) (space 0) (compilation-speed 0)))
  (loop for type in *particle-type-list* do
	(particle-type-iterator
	 (prt type)
	 unless (particle-block prt) do
	 (setf (particle-state-n-double prt) (particle-state-n+1-double prt)
	       (particle-dsdt-n-1-double prt) (particle-dsdt-n-double prt))
	 (case (particle-type-class type)
	   (:markov (advance-markov-states prt)))))
  nil)
  
;; ******************* ******************* ******************* 
;; ******************* ******************* *******************
;;
;;                     Particle Evaluation
;;
;; ******************* ******************* *******************
;; ******************* ******************* *******************


;; The key equation for solving the new particle state with variable time steps.
;;
;;  (*half-delta-t[n-1]*) = (the df (* 0.5d0 (the sf (* *mrt* *last-time-step*))))
;;  (*delta-forward*) = (the df (* 0.5d0 (the sf (* *mrt* *time-step*))))
;;
;;  (*sum-delta-for-back*) = (the df (+ (*delta-forward*) (*half-delta-t[n-1]*)))
;;  (*half-sum-delta-for-back*) = (the df (* 0.5 (+ (*delta-forward*) (*half-delta-t[n-1]*))))
;;
(defmacro adapted-hines-1st-order-equation (tau inf previous-state)
  `(/ (+ (* (*delta-t-prime[n]*) ,inf)
       (* (- ,tau (*half-delta-t-prime[n]*)) ,previous-state))
    (+ ,tau (*half-delta-t-prime[n]*))))

;; Used in voltage particle evaluations
(proclaim '(inline adapted-hines-1st-order-equation-from-tau-inf))
(defun adapted-hines-1st-order-equation-from-tau-inf (tau inf previous-state initial-state prt)
  (declare (optimize (safety 0) (speed 3) (space 0) (compilation-speed 0))
	   (type single-float tau inf)
	   (type double-float previous-state))
  (if initial-state
      (d-flt (or (element-parameter prt 'initial-state)
		 (element-parameter (particle-type prt) 'initial-state)
		 inf))
      (adapted-hines-1st-order-equation tau inf previous-state)))


;; Used in conc-part evaluations

(defun get-particle-initial-state-for-adapted-hines (prt inf)
  (d-flt (or (element-parameter prt 'initial-state)
	     (element-parameter (element-type prt) 'initial-state)
	     inf)))

(proclaim '(inline adapted-hines-1st-order-equation-from-double-tau-inf))
(defun adapted-hines-1st-order-equation-from-double-tau-inf (tau inf previous-state initial-state prt)
  (declare (optimize (safety 0) (speed 3) (space 0) (compilation-speed 0))
	   (type double-float tau inf)
	   (type double-float previous-state))
  (if initial-state
      (the df (get-particle-initial-state-for-adapted-hines prt inf))
      (adapted-hines-1st-order-equation tau inf previous-state)))

(proclaim '(inline set-particle-aref-initial-states))
(defun set-particle-aref-initial-states (prt-df-array)
  (declare (optimize (safety 1) (speed 3) (space 0) (compilation-speed 0)))
  (setf (particle-aref-state-n prt-df-array) (particle-aref-state-n+1 prt-df-array)
	(particle-aref-dsdt-n prt-df-array) 0.0d0
	(particle-aref-dsdt-n-1 prt-df-array) 0.0d0)
  nil)

(proclaim '(inline calculate-particle-error))
(defun calculate-particle-error (prt-df-array name-or-prt)
  (declare (optimize (safety 0) (speed 3) (space 0) (compilation-speed 0)))
  (setf (particle-aref-dsdt-n prt-df-array)
	(/ (- (particle-aref-state-n+1 prt-df-array) (particle-aref-state-n prt-df-array))
	   (*delta-t-prime[n]*)))
  (let ((d2sdt2-numerator (- (particle-aref-dsdt-n prt-df-array) (particle-aref-dsdt-n-1 prt-df-array))))
    (when (and
	   (cond ((> d2sdt2-numerator (*maximum-particle-error-numerator*))
		  (setf (*maximum-particle-error-numerator*) d2sdt2-numerator))
		 ((> (the df (- d2sdt2-numerator)) (*maximum-particle-error-numerator*))
		  (setf (*maximum-particle-error-numerator*) (the df (- d2sdt2-numerator)))))
	   *debug-particle-error*)
      (setq *particle-w-max-error* (or (element-name name-or-prt) name-or-prt))))
  nil)

(proclaim '(inline finish-eval-particle-aref))
(defun finish-eval-particle-aref (prt-df-array initial-state &optional name-or-prt)
  (declare (optimize (safety 0) (speed 3) (space 0) (compilation-speed 0)))
  (cond (initial-state (set-particle-aref-initial-states prt-df-array))
	(*calculate-particle-error* (calculate-particle-error prt-df-array name-or-prt)))
  nil)

(proclaim '(inline finish-eval-markov-particle))
(defun finish-eval-markov-particle (prt initial-state)
  (declare (optimize (safety 0) (speed 3) (space 0) (compilation-speed 0)))
  (loop for state-index from 0 to (nb-states-1 prt)
	do (finish-eval-particle-aref
	    (prt-state prt state-index) initial-state
	    (when *debug-particle-error* (format nil "~A state ~A" (particle-name prt) state-index))))
  nil)

(proclaim '(inline finish-eval-particle))
(defun finish-eval-particle (prt initial-state &optional markov-p)
  (declare (optimize (safety 0) (speed 3) (space 0) (compilation-speed 0)))
  (finish-eval-particle-aref (particle-double-floats prt) initial-state (when *debug-particle-error* prt))
  (when markov-p (finish-eval-markov-particle prt initial-state))
  nil)
      
(proclaim '(inline v-half-shifted-particle-voltage-index))
(defun v-half-shifted-particle-voltage-index (prt v-half-shift-magnitude)
  (declare (optimize (safety 3) (speed 3) (space 0) (compilation-speed 0)))
  (let ((v-half-shift-particle
	 ;; stupid compiler....
	 (the particle (or (get-a-value 'v-half-shift-particle (particle-parameters prt)) prt))))
    (max 0 (min (the fn (1- *particle-look-up-table-length*))
		(+ (the fn (particle-v-index prt))
		   (VOLTAGE-DOUBLE-TO-VOLTAGE-INDEX-RELATIVE
		    (* (the sf v-half-shift-magnitude)
		       (- (particle-state-n-double v-half-shift-particle) 0.5))))))))
			   


(defun linear-markov-binomial (p n m)
  (declare (optimize (safety 0) (speed 3) (space 0) (compilation-speed 0))
	   (type fixnum n m)
	   (type double-float p))
  (loop for i fixnum from m to n
	sum (* (the df (expt p i))
	       (the df (expt (- 1 p) (- N i)))
	       (the fn (choose n i)))
	into result double-float
	finally (return result)))

(proclaim '(notinline eval-two-state-particle-type))
(defun eval-two-state-particle-type (prt-type initial-state)
  (declare (optimize (safety 0) (speed 3) (space 0) (compilation-speed 0)))
  (let* ((tau-array (the vec-flt (particle-type-tau-array prt-type)))
	 (inf-array (the vec-flt (particle-type-inf-array prt-type)))
	 (params (particle-type-parameters prt-type))
	 (v-half-shift-particle-type nil) ; (get-a-value 'v-half-shift-particle-type params)
	 (v-half-shift-magnitude (when v-half-shift-particle-type (get-a-value 'v-half-shift-magnitude params)))
	 (linear-markov-n (element-parameter prt-type 'linear-markov-n))
	 (linear-markov-m (element-parameter prt-type 'linear-markov-m)))
    (particle-type-iterator
     (prt prt-type)
     do
     (let* ((voltage-index (if v-half-shift-particle-type
			       (v-half-shifted-particle-voltage-index prt v-half-shift-magnitude)
			       (particle-v-index prt)))
	    (tau (aref tau-array voltage-index))
	    (inf (aref inf-array voltage-index)))
       (declare (type particle prt)
		(single-float tau inf)
		(fixnum voltage-index))
       (when *interpolate-particle-arrays*
	 (let ((remainder (particle-v-rem prt)))
	   (setf tau (+ (* (- 1.0 remainder) tau) (* remainder (aref tau-array (1+ voltage-index))))
		 inf (+ (* (- 1.0 remainder) inf) (* remainder (aref inf-array (1+ voltage-index)))))))
       ;; Now solve for the particle state.
       (setf (particle-state-n+1-double prt)
	     (adapted-hines-1st-order-equation-from-tau-inf tau inf (particle-state-n-double prt)
							    initial-state prt))
       (when linear-markov-n
	 (setf (particle-state-n+1-double prt)
	       (linear-markov-binomial (particle-state-n+1-double prt) linear-markov-n linear-markov-m)))
       (when *debug-eval-particle
	 (format t " eval-prt ~A state-n+1 ~f vindex ~a~%" prt (particle-state-n+1-double prt) voltage-index))
       (finish-eval-particle prt initial-state))))
  nil)


(defun eval-all-particles (&optional initial-state)
  (declare (optimize (safety 0) (speed 3) (space 0) (compilation-speed 0)))
  (loop for prt-type in *particle-type-list* do
	; (format t "~A~%" prt-type)
	(if (eq (particle-type-class prt-type) :markov)
	    (eval-markov-particle-type prt-type initial-state)
	    (eval-two-state-particle-type prt-type initial-state))))

(defun init-particles ()
  (eval-all-particles t))

#|
;; PARTICLE-ERROR-OK Adjust *PARTICLE-ERROR-MAX-TIME-STEP* to the maximum allowed by the particle
;; error criterium. Returns NIL if the current time step is greater than the global *MIN-STEP* and is
;; greater than the value allowed by the particle error criterium.
(defun particle-error-ok ()
  (declare (optimize			; (safety 1)
	    (speed 3) (space 0) (compilation-speed 0)))
  (or (not *calculate-particle-error*)	; Return "ok" if we don't care about particle error.
      (if (<= (*maximum-particle-error-numerator*) 1.0d-10) ; Getting problems with values around 1.0e-320.

	  (setq *particle-error-max-time-step* *max-step*) ; Error Ok

	(let* ((d2m-ndt2 (/ (* 4.0d0 (*maximum-particle-error-numerator*))
			    (* *mrt* (- (- (+ *sim-time-n+1* *sim-time-n*) *sim-time-n-1*) *sim-time-n-2*))))

	       ;; This is the maximum time step allowed given the specified *RELATIVE-PARTICLE-ERROR* and the current
	       ;; maximum particle state second derivative.
	       (particle-conduction-state-error
		(* (if *estimate-particle-error-with-full-time-step* 1.0 2.0)
		   (- (kernel::%sqrt (/ *twice-relative-particle-error* d2m-ndt2)) (*half-delta-t[n-1]*))))

	       (error-value (if (< (*markov-particle-state-delta-s-max-time-step*) 0)
				particle-conduction-state-error
			      (min (*markov-particle-state-delta-s-max-time-step*) particle-conduction-state-error)))

	       ;; The maximum time step allowed as above, in units of *mrt*.
	       (particle-error-max-time-step
		(if (< error-value 0)
		    *min-step*
		  (let ((float-step (coerce (the df (* *pick-time-step-fudge* (/ error-value *mrt*)))
					    'single-float)))
		    (if (< float-step most-positive-fixnum-float-version) (KERNEL:%UNARY-ROUND
									   ; round
									   (the sf float-step)) most-positive-fixnum)))))
	  (declare (type fixnum particle-error-max-time-step))
	  (unless (= particle-error-max-time-step *particle-error-max-time-step*)
	    (setq *particle-error-max-time-step* particle-error-max-time-step))
	  (when *debug-particle-error*
	    (format t "time ~f part ~A has Max error ~f~%" *real-time* *particle-w-max-error*
		    (* (*delta-t[n]-squared*) 0.5 d2m-ndt2)))
	  ;; Return NIL if we are stepping too far.
	  (or (<= *time-step* *min-step*)
	      (<= (*delta-t[n]*) error-value))))))

|#