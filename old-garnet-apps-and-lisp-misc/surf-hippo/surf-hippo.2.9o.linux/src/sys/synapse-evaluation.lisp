;;; -*- mode: lisp; Syntax: Common-lisp; package: surf ; base: 10; -*-
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


;;; SYS Source file: synapse-evaluation.lisp

(in-package "SURF-HIPPO")


;; ************************************************
;; ************* EVALUATION FUNCTIONS *************
;; ************************************************


;; Only light-dependent, autonomous, or voltage-controlled synapses get evaluated here. Channel
;; driven/dependent synapses do not have a syn. The activity of these synapses are evaluated by
;; their associated channels.


(defmacro static-v-dependence-value (node static-v-dependence)
  `(aref (the (simple-array single-float (1500)) ,static-v-dependence)
    (node-fixnum-aref-prt-v-index (node-fixnums ,node))))

;; This is used by all the eval synapse routines.
(defmacro finish-syn-eval (syn gbar-coeff static-v-dependence e-rev-update-enable)
  `(if (= 0.0 (the sf ,gbar-coeff))
    (unless (= 0.0d0 (synapse-conductance ,syn)) (setf (synapse-conductance ,syn) 0.0d0))
    (let ((node (synapse-node ,syn)))
      (when ,e-rev-update-enable (setf (synapse-e-rev ,syn) (element-e-rev-from-shells (synapse-conc-ints-params ,syn))))
      (let ((conductance
	     (the df (* (synapse-gbar ,syn) 
			(the sf ,gbar-coeff)
			(the sf (if ,static-v-dependence (static-v-dependence-value node ,static-v-dependence) 1.0))))))
	(unless (node-has-ideal-voltage-source node)
	  (let ((node-double-floats (node-double-floats node)))
	    (deccumulate-setf (node-aref-current node-double-floats) (* conductance (synapse-e-rev ,syn)))
	    (accumulate-setf (node-aref-jacobian node-double-floats) conductance)))
	(unless (= conductance (synapse-conductance ,syn)) (setf (synapse-conductance ,syn) conductance))))))

(defmacro finish-syn-eval-w-decay-factor (syn gbar-coeff static-v-dependence e-rev-update-enable decay-factor)
  `(let ((node (synapse-node ,syn)))
    (when ,e-rev-update-enable (setf (synapse-e-rev ,syn) (element-e-rev-from-shells (synapse-conc-ints-params ,syn))))
    (let ((conductance
	   (the df (* (+ (* (synapse-conductance ,syn) (the sf ,decay-factor))
			 (* ,gbar-coeff (synapse-gbar ,syn)))
		      (the sf (if ,static-v-dependence (static-v-dependence-value node ,static-v-dependence) 1.0))))))
      (unless (node-has-ideal-voltage-source node)
	(let ((node-double-floats (node-double-floats node)))
	  (deccumulate-setf (node-aref-current node-double-floats) (* conductance (synapse-e-rev ,syn)))
	  (accumulate-setf (node-aref-jacobian node-double-floats) conductance)))
      (unless (= conductance (synapse-conductance ,syn)) (setf (synapse-conductance ,syn) conductance)))))
				    



;; This returns a single float. LBG 7/6/99 Original modified from NG code.

;; This macro is evaluated within a loop in EVAL-AUTO-SYNAPSE over the entries in XFRMD-EVENTS, and returns the
;; appropriate value of the conductance wave associated with EVENT-TIME, which is derived from the CAR of
;; XFRMD-EVENTS.

;; If at the current time step EVENT-TIME occurs too late to be relevant, then this form nulls the local
;; XFRMD-EVENTS to stop the loop iteration for this synapse and returns 0.0. If at the *last* time step EVENT-TIME
;; occured too early to still be relevant, then it is removed from the XFRMD-EVENTS slot of the synapse, and this
;; form returns 0.0. If at the current time step EVENT-TIME occured too early to still be relevant, then this form
;; returns 0.0.

;; T-IN-INT and T-IN-FRACT are the integer and fractional parts, respectively, of the simulation time adjusted for
;; the time base of the synapse type conductance wave. Note that EVENT-TIMEs in this form are in units of the
;; waveform interval. For 1st-order-Depressing synapses, each entry in XFRMD-EVENTS is a list of the form
;; (EVENT-TIME WEIGHT).

(defmacro AUTO-SYNAPSE-ACCUMULATER (syn wave delta-wave 
					t-in-int t-in-fract ; Wave input time integer and fractional parts
					last-valid-t-in-int ; Last valid wave input time integer part
					wave-length
					xfrmd-events) ; Transformed event times
  `(let* ((event-time/weight (car ,xfrmd-events)) ; The next event in the queue.
	  (event-time (the fn (if (consp event-time/weight) (car event-time/weight) event-time/weight))))
    (declare (type (or fixnum cons) event-time/weight) (fixnum event-time))
    
    (if (> event-time ,t-in-int)
	
	;; Time hasn't reached the next event yet. Null the local XFRMD-EVENTS to exit loop, returning 0.0.
	(progn (setq ,xfrmd-events nil) 0.0)

	;; Now check if EVENT-TIME should be ignored since it referenced the end of the conductance wave the last go
	;; around. LAST-VALID-T-IN-INT is the value of T-IN-INT used in the last synapse type evaluation. This is used
	;; to calculate LAST-DELAYED-EFFECTIVE-TIME-INT, which is an index for the conductance wave.

	(if (and (numberp ,last-valid-t-in-int)
		 (>= (the fn (- (the fn ,last-valid-t-in-int) event-time)) ,wave-length))
						       
	    ;; If last valid integration step index overreaches wave, then we are permanently through with this
	    ;; event, and we throw it out and return a value of 0.0.
	    
	    (progn (setf (synapse-transformed-events ,syn) (cdr ,xfrmd-events)) 0.0)

	    ;; Further process wave. Calculate the wave index DELAYED-EFFECTIVE-TIME-INT, appropriate for the
	    ;; current time step.
	    
	    (let ((delayed-effective-time-int (- ,t-in-int event-time)))
	      (declare (fixnum delayed-effective-time-int))
	      (if (>= delayed-effective-time-int ,wave-length)

		  ;; If the current index overreaches wave, then we may be permanently through
		  ;; with this event, but we throw it out on the next time step in case a subsequent
		  ;; iteration during the current time step is small enough such that the event is
		  ;; still relevant. For this iteration, ignoring the event means that the returned
		  ;; value of the accumulate is 0.0.
		  0.0

		  ;; Good index value -> Accumulate wave.
		  (let ((interpolated-array-value (interpolated-array-value-with-delta-array
						   ,wave ,delta-wave delayed-effective-time-int ,t-in-fract)))
		    (if (consp event-time/weight) ; Consider weighted synapse g.
			(* (the sf (cdr event-time/weight)) (the sf interpolated-array-value))
			(the sf interpolated-array-value)))))))))


(defmacro AUTO-SYNAPSE-ACCUMULATER-w-decay-factor (syn
						   t-in-int ; Wave input time integer part
						   xfrmd-events ; Transformed event times
						   )
  `(let* ((event-time/weight (car ,xfrmd-events)) ; The next event in the queue.
	  (event-time (the fn (if (consp event-time/weight) (car event-time/weight) event-time/weight))))
    (declare (type (or fixnum cons) event-time/weight) (fixnum event-time))
    
    (if (> event-time ,t-in-int)
	
	;; Time hasn't reached the next event yet. Null the local XFRMD-EVENTS to exit loop, returning 0.0.
	(progn (setq ,xfrmd-events nil) 0.0)

	;; Process event once, and remove if from the synapse's :TRANSFORMED-EVENTS.

	(progn (setf (synapse-transformed-events ,syn) (cdr ,xfrmd-events))
	       (if (consp event-time/weight) ; Consider weighted synapse g.
		   (the sf (cdr event-time/weight))
		   1.0)))))

(proclaim '(inline EVAL-AUTO-SYNAPSE))
(defun EVAL-AUTO-SYNAPSE (syn static-v-dependence e-rev-update-enable
			      wave delta-wave wave-length 
			      wave-input-time-int wave-input-time-fract last-valid-wave-input-time-int)
  (declare (optimize (safety 0) (speed 3) (space 0) (debug 0))
	   (fixnum wave-length wave-input-time-int))
  (when (synapse-transformed-events syn)
    (let ((gbar-coeff 0.0))
      (declare (single-float gbar-coeff))
      ;; Use a DO so that AUTO-SYNAPSE-ACCUMULATER can set TRANSFORMED-EVENTS to NIL in order punt the loop prematurely.
      (do ((transformed-events (synapse-transformed-events syn) (cdr transformed-events)))
	  ((null transformed-events)
	   (finish-syn-eval syn gbar-coeff static-v-dependence e-rev-update-enable))
	(declare (type (or cons null) transformed-events))  
	(incf gbar-coeff (the sf (AUTO-SYNAPSE-ACCUMULATER
				  syn wave delta-wave 
				  wave-input-time-int wave-input-time-fract last-valid-wave-input-time-int
				  wave-length transformed-events)))))
    NIL))

(proclaim '(inline EVAL-AUTO-SYNAPSE-w-decay-factor))
(defun EVAL-AUTO-SYNAPSE-w-decay-factor
    (syn static-v-dependence e-rev-update-enable
	 wave-input-time-int decay-factor)
  (declare (optimize (safety 0) (speed 3) (space 0) (debug 0))
	   (fixnum wave-input-time-int))
  ;; NOT conditional on (synapse-transformed-events syn) since waveforms are always decaying....

  (let ((gbar-coeff 0.0))
    (declare (single-float gbar-coeff))
    ;; Use a DO so that AUTO-SYNAPSE-ACCUMULATER can set TRANSFORMED-EVENTS to NIL in order punt the loop prematurely.
    (do ((transformed-events (synapse-transformed-events syn) (cdr transformed-events)))
	((null transformed-events)
	 (finish-syn-eval-w-decay-factor syn gbar-coeff static-v-dependence e-rev-update-enable decay-factor))
      (declare (type (or cons null) transformed-events))  
      (incf gbar-coeff (the sf (AUTO-SYNAPSE-ACCUMULATER-w-decay-factor syn wave-input-time-int transformed-events)))))


    
  NIL)


;; (element-parameter type 'conductance-waveform-half-time-mrt)

;; This is used by eval-voltage-synapse whenever a new event is detected.
(proclaim '(inline add-event-to-syn-transformed-events))
(defun add-event-to-syn-transformed-events (syn transformed-event-time event-breakpoints-mrt)
  (declare (optimize (safety 0) (speed 3) (space 0) (debug 0)))
  ;; :TRANSFORMED-EVENTS = (1stevt 2ndevt 3rdevt ...)  NG, 3/11/97 rajoute la declaration fixnum apres list.  -
  ;; remplace le NCONC par un RPLACD plus efficace : conse deux fois moins, et est 20% plus rapide - rappel: la
  ;; forme 'list' alloue effectivement une nouvelle liste.
  (let ((listified-delayed-transformed-event-time
	 (list (the fn (+ (synapse-fixnum-delay syn) (the fn transformed-event-time))))))
    (if (synapse-transformed-events syn)
      (rplacd (last (synapse-transformed-events syn)) listified-delayed-transformed-event-time)
      (setf (synapse-transformed-events syn) listified-delayed-transformed-event-time)))
  (when event-breakpoints-mrt
    (let ((delay-mrt (kernel::sf-sb32-truncate (/ (synapse-delay syn) *mrt*))))
      (loop for event-breakpoint-mrt in event-breakpoints-mrt do
	    (insert-event-mrt-breakpoint (+ delay-mrt (the fn event-breakpoint-mrt)))))))


;; EVAL-VOLTAGE-SYNAPSE determines whether a new event occurs at the current time step for SYN. If so, then the event
;; time is saved in the :EVENT-TIMES list, and the transformed event time is saved in the :TRANSFORMED-EVENTS list
;; for SYN and, if SYN is an EVENT-GENERATOR, onto all the EVENT-FOLLOWERS associated with SYN. Note that the
;; transformation of events to synaptic conductance changes is done by EVAL-AUTO-SYNAPSE.
(proclaim '(inline eval-voltage-synapse))
(defun eval-voltage-synapse (syn type ignore-supra-threshold-duration conductance-waveform-event-breakpoints-mrt)
  (declare (optimize (safety 0) (speed 3) (space 0) (debug 0)))
  (let ((below-threshold-p (< (node-voltage-n (synapse-pre-synaptic-node syn)) (synapse-type-input-threshold type)))
	(t[n] (*t[n]*)))
    (declare (single-float t[n]))
    ;; Test if either below threshold or within the refractory period of the last event.
    (if (or below-threshold-p
	    (when (synapse-event-times syn)
	      (> (synapse-type-refractory-period type) (- t[n] (the sf (car (synapse-event-times syn)))))))

      ;; Still sub-threshold or in refractory period so update :SUB-THRESHOLD-TIME.
      (setf (synapse-sub-threshold-time syn) t[n])
	
      ;; Fire event if above threshold and if either SYNAPSE-TYPE-SUPRA-THRESHOLD-DURATION-MIN is ignored
      ;; (= 0), or if the current period above threshold is is greater than this time.
      (when (and (not below-threshold-p)
		 (or ignore-supra-threshold-duration
		     (> (the sf (- (*t[n+1]*) (synapse-sub-threshold-time syn)))
			(synapse-type-supra-threshold-duration-min type))))

	;; Push event time, t[n], and reset sub threshold time when threshold conditions are met.
	(push t[n] (synapse-event-times syn))
	(setf (synapse-sub-threshold-time syn) t[n])
	(let ((transformed-event-time (the (SIGNED-BYTE 32) (round (the sf (* t[n] (synapse-type-waveform-time-interval-inverse type))))))
	      (event-breakpoints-mrt
	       (when *dynamic-breakpoint-generation* (cons (convert-time-to-mrt-units t[n]) conductance-waveform-event-breakpoints-mrt))))
	  (if *ENABLE-EVENT-GENERATORS*
	    ;; Add event to all the event-followers of this synapse.
	    (loop for event-follower in (fast-syn-event-followers syn) do
		  (add-event-to-syn-transformed-events event-follower transformed-event-time event-breakpoints-mrt))
	    (add-event-to-syn-transformed-events syn transformed-event-time event-breakpoints-mrt))))))
  nil)

event-breakpoints-mrt
;; (element-parameter type 'conductance-waveform-half-time-mrt)




(proclaim '(inline eval-light-synapse))
(defun eval-light-synapse (syn static-v-dependence e-rev-update-enable
			       2dwave 2dwave-length fractional-time=0-OR-do-not-interpolate base-gain-array )
  (declare (optimize (safety 0) (speed 3) (space 0) (debug 0))
	   (type (simple-array single-float (*)) base-gain-array))
  (let* ((wave-index (synapse-wave-index syn))
	 (wave-shift (- (the fn *integer-time*) (synapse-wave-shift syn)))
	 (gbar-coeff (if (< wave-shift 0)
			 0.0
			 (* (aref base-gain-array wave-index)
			    (interpolated-array-slice-value 2dwave 2dwave-length wave-index wave-shift
							    (*fractional-time*) fractional-time=0-OR-do-not-interpolate)))))
    (declare (single-float gbar-coeff))
    (finish-syn-eval syn gbar-coeff static-v-dependence e-rev-update-enable))
     
  nil)




(proclaim '(inline eval-tonic-synapse))
(defun eval-tonic-synapse (syn static-v-dependence e-rev-update-enable)
  (declare (optimize (safety 0) (speed 3) (space 0) (debug 0))
	   (type boolean e-rev-update-enable))		     
  (finish-syn-eval syn 1.0 static-v-dependence e-rev-update-enable)
  nil)
  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Eval synapse types
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun save-syn-current-during-eval-p (type)
  (true-p (or (synapse-type-conc-int-type-params type)
	      (let ((element-iv-parameters (synapse-type-iv-parameters type)))
		(when element-iv-parameters
		  (eq (membrane-element-iv-parameters-iv-relation element-iv-parameters) :constant-field))))))

(defun first-iteration&var-e-rev-p (first-iteration type)
  (true-p (and first-iteration
	       (synapse-type-variable-e-rev type)
	       (synapse-type-conc-int-type-params type))))



(defun eval-all-synapses (&optional (first-iteration t))
  (declare (optimize (safety 0) (speed 3) (space 0) (compilation-speed 0)))
  (when *enable-synapses*
    (loop for type in *synapse-type-list* do
	  (case (synapse-type-control type)
	    (tonic (when *eval-all-synapses-this-iteration*
		     (eval-tonic-synapse-type type first-iteration)))
	    (light (when (and *eval-all-synapses-this-iteration* *enable-light*)
		     (eval-light-synapse-type type first-iteration)))
	    (light-auto (when (and *eval-all-synapses-this-iteration* *enable-light*)
			  (eval-auto-synapse-type type first-iteration)))
	    (auto (when *eval-all-synapses-this-iteration*
		    (eval-auto-synapse-type type first-iteration)))
	    (voltage (when (or *eval-all-synapses-this-iteration* first-iteration) 
		       (eval-voltage-synapse-type type first-iteration)))))))


(defun eval-tonic-synapse-type (type &optional (first-iteration t))
  (declare (optimize (safety 0) (speed 3) (space 0) (compilation-speed 0)))
  (let* ((params (synapse-type-parameters type))
	 (static-v-dependence (get-a-value 'static-voltage-dependence params))
	 (first-iteration&var-e-rev (first-iteration&var-e-rev-p first-iteration type)))
    (synapse-type-do
     (syn type)
     (unless (synapse-block syn)
       (eval-tonic-synapse syn static-v-dependence first-iteration&var-e-rev)))))

(defun eval-light-synapse-type (type &optional (first-iteration t))
  (declare (optimize (safety 0) (speed 3) (space 0) (compilation-speed 0)))
  (let* ((params (synapse-type-parameters type))
	 (static-v-dependence (get-a-value 'static-voltage-dependence params))
	 (e-rev-update-enable (first-iteration&var-e-rev-p first-iteration type))
	 (fractional-time=0-OR-do-not-interpolate
	  (or (= 0.0 (*fractional-time*)) (not *interpolate-light-synapse-conductance-waveforms*)))
	 (2dwave (get-a-value 'light-responses-ARRAY params))
	 (2dwave-length (array-dimension (the 2dfloat 2dwave) 1))
	 (base-gain-array (get-a-value 'light-inputs-base-gain params)))
    (synapse-type-do
     (syn type)
     (unless (or (synapse-block syn) (not (synapse-wave-ref syn)))
       (eval-light-synapse
	syn static-v-dependence e-rev-update-enable
	2dwave 2dwave-length fractional-time=0-OR-do-not-interpolate base-gain-array)))))


;; The value for WAVE-INPUT-TIME-FRACTIONAL-PART is common for all auto synapses of the same type at
;; a given time step, so this is calculated once and passed on to all the individual evals. This
;; function is applied to both LIGHT-AUTO and AUTO synapse types. 

;; NG, 31/10/97
;; - suppression d'appels multiples a synapse-node.
;; - ajout de declaration de types.
;; - suppression de multiple-value-bind pour truncate et optimisation.
;; - suppression de set-element-parameter-fast, qui est trop lent..
;; LBG 7/6/99 Original modified
(defun EVAL-AUTO-SYNAPSE-TYPE (type &optional (first-iteration t) time)
  (declare (optimize (safety 0) (speed 3)
		     (space 1)		; When space-1, avoids 3 notes about EQL.
		     (compilation-speed 0)))
  (let* ((params (synapse-type-parameters type))
	 ;; Future use.
	 ;; (evaluation-method-parameters (get-a-value 'evaluation-method-parameters params))
	 ;; New stuff from NG code
	 ;; For *OPTIMIZE-EXPO-CONDUCTANCE-WAVEFORMS*
	 (decay-factor (when *OPTIMIZE-EXPO-CONDUCTANCE-WAVEFORMS* (get-a-value 'CONDUCTANCE-DECAY-FACTOR params)))
	 (wave (get-a-value 'waveform params))
	 (delta-wave (get-a-value 'delta-waveform params))
	 (wave-length (length (the vec-flt wave)))
	 (static-v-dependence (get-a-value 'static-voltage-dependence params))
	 (e-rev-update-enable (first-iteration&var-e-rev-p first-iteration type))
	 (last-valid-wave-input-time-int (get-a-value 'last-valid-wave-input-time-integer-part params)))
    (let* ((wave-input-time (* (synapse-type-waveform-time-interval-inverse type) (the sf (or time (*input-time*)))))
	   (wave-input-time-int (ext:truly-the fn (kernel:%unary-truncate wave-input-time)))
	   (wave-input-time-fract (- wave-input-time wave-input-time-int)))
      (declare (fixnum wave-input-time-int) (single-float wave-input-time-fract wave-input-time))
      (let ((*ADVANCE-AUTONOMOUS-ELEMENTS* (or *ADVANCE-AUTONOMOUS-ELEMENTS* *first-time-step*)))
	(synapse-type-do
	 (syn type)
	 (unless (synapse-block syn)
	   (if decay-factor
	       (EVAL-AUTO-SYNAPSE-w-decay-factor
		syn static-v-dependence e-rev-update-enable wave-input-time-int decay-factor)
	       (EVAL-AUTO-SYNAPSE
		syn static-v-dependence e-rev-update-enable wave delta-wave wave-length 
		wave-input-time-int wave-input-time-fract last-valid-wave-input-time-int))))
	(set-synapse-type-parameter-slot-fast type 'last-wave-input-time-integer-part wave-input-time-int params))
      nil)))

(defun eval-voltage-synapse-type (type &optional (first-iteration t) time)
  (declare (optimize (safety 0) (speed 3) (space 1) (compilation-speed 0)))
  (let* ((params (synapse-type-parameters type))
	 (conductance-waveform (get-a-value 'waveform params))
	 (delta-wave (get-a-value 'delta-waveform params))
	 (conductance-waveform-length (length (the vec-flt conductance-waveform)))
	 (static-v-dependence (get-a-value 'static-voltage-dependence params))
	 (conductance-waveform-event-breakpoints-mrt (get-a-value 'conductance-waveform-event-breakpoints-mrt params))
	 (e-rev-update-enable (first-iteration&var-e-rev-p first-iteration type))
	 (ignore-supra-threshold-duration (zerop (synapse-type-supra-threshold-duration-min type)))
	 (last-valid-wave-input-time-int (get-a-value 'last-valid-wave-input-time-integer-part params))

	 ;; future use?
	 ;; (evaluation-method-parameters (get-a-value 'evaluation-method-parameters params))
	 
	 ;; New stuff from NG code
	 ;; For *OPTIMIZE-EXPO-CONDUCTANCE-WAVEFORMS*
	 (decay-factor (when *OPTIMIZE-EXPO-CONDUCTANCE-WAVEFORMS* (get-a-value 'CONDUCTANCE-DECAY-FACTOR params)))
					; (type-generators (get-a-value 'GENERATORS params))
	 )
    (let* ((wave-input-time (* (synapse-type-waveform-time-interval-inverse type) (the sf (or time (*input-time*)))))
	   (wave-input-time-int (ext:truly-the fn (kernel:%unary-truncate wave-input-time)))
	   (wave-input-time-fract (- wave-input-time wave-input-time-int)))
      (declare (fixnum wave-input-time-int) (single-float wave-input-time-fract wave-input-time))
      (let ((*ADVANCE-AUTONOMOUS-ELEMENTS* (or *ADVANCE-AUTONOMOUS-ELEMENTS* *FIRST-TIME-STEP*)))
	(synapse-type-do
	 (syn type)
	 (when (and first-iteration (eq syn (synapse-event-generator syn)))
	   (eval-voltage-synapse syn type ignore-supra-threshold-duration conductance-waveform-event-breakpoints-mrt))
	 (unless (synapse-block syn)
	   (when *eval-all-synapses-this-iteration*
	     (if decay-factor
	       (EVAL-AUTO-SYNAPSE-w-decay-factor
		syn static-v-dependence e-rev-update-enable wave-input-time-int decay-factor)
	       (EVAL-AUTO-SYNAPSE
		syn static-v-dependence e-rev-update-enable conductance-waveform delta-wave conductance-waveform-length 
		wave-input-time-int wave-input-time-fract last-valid-wave-input-time-int))))))

      ;; 'LAST-WAVE-INPUT-TIME-INTEGER-PART will be used for setting
      ;; 'LAST-VALID-WAVE-INPUT-TIME-INTEGER-PART if the integration step is successful.
      (set-synapse-type-parameter-slot-fast type 'last-wave-input-time-integer-part wave-input-time-int params))))





;; Called after a successful integration step. 'LAST-VALID-WAVE-INPUT-TIME-INTEGER-PART is used
;; during the next time step for the auto synapse evaluations to filter out old event times that are
;; no longer valid.
(defun after-time-step-synapse-cleanup ()
  (declare (optimize (safety 0) (speed 3) (space 0) (compilation-speed 0)))
  (when *enable-synapses*
    (loop for type in *synapse-type-list* do
	  (case (synapse-type-control type)
	    ((light-auto auto)
	     (let* ((params (synapse-type-parameters type))
		    (last-wave-int (get-a-value 'last-wave-input-time-integer-part params))
		    (last-valid-int (get-a-value 'last-valid-wave-input-time-integer-part params)))
	       (when (and last-wave-int last-valid-int ; Make sure these numbers exist.
			  (> (the fn last-wave-int) (the fn last-valid-int))) ; Is this check necessary??
		 (set-element-parameter-fast type 'last-valid-wave-input-time-integer-part last-wave-int params))))))))
		  



  

