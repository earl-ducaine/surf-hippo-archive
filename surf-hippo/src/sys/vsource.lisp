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


;;; SYS Source file: vsource.lisp
;
; the voltage source model
;

(in-package "SURF-HIPPO")



;; temporary hacks

(defvar *vsrnodevolt* nil)
(defvar *vsrvolt* nil)


(defun document-vsource (vsrc)
  (let ((vsource (element vsrc 'vsource)))
    (when vsource
      (format t "#|~%")
      (print-vsource vsource)
      (format t "|#~%")
      (print-create-vsource vsource))))

(defun print-create-vsource (vsource &optional (indent-output 0))
  (when (> indent-output 0) (dotimes (i indent-output) (format t " ")))
  (format t "(add-vsource ~s ~s)~%"
	  (element-name (element-cell-element vsource))
	  (element-name vsource))
  (when (element-parameter vsource 'waveform-function)
    (when (> indent-output 0) (dotimes (i indent-output) (format t " ")))
    (document-waveform-function vsource))
  (when (vsource-use-pulse-list vsource)
    (when (> indent-output 0) (dotimes (i indent-output) (format t " ")))
    (document-pulse-list vsource)))




(defun print-vsource (vsrc)
  (let ((vsrc (element vsrc 'vsource)))
    (when vsrc
      (if (element-parameter vsrc 'ideal-vsource)
	  (format t
		  "Vsource ~a (~A) [Ideal],"
		  (vsource-name vsrc)
		  (node-name (vsource-node vsrc)))
	  (format t
		  "Vsource ~a (~A), Internal resistance ~aKohms,"
		  (vsource-name vsrc)
		  (node-name (vsource-node vsrc))
		  (* 1000 (vsource-resistance vsrc))))
      (format t " Default mag ~AmV~%" (vclamp-default-magnitude vsrc))
       
      (case (element-parameter vsrc 'pulse-transition)
	(:fixed-slope
	 (format t "  Pulse transtion fixed by slope of ~A mV/msec~%"
		 (or (element-parameter vsrc 'pulse-transition-slope) 1000.0)))
	(:fixed-transition-time
	 (format t "  Pulse transtion fixed by transition time of ~A msec~%"
		 (or (element-parameter vsrc 'pulse-transition-time) 0.1))))
      (cond
	((and *vsource-intrinsic-current* (not (element-parameter vsrc 'ideal-vsource)))
	 (format t
		 "  Current calculated directly from voltage drop across source resistance.~%"))
	(*INCLUDE-VSOURCE-CURRENT-LINEAR-and-non-local-COMPONENT*
	 (format t 
		 "  Current calculated across individual source node elements.~%"))
	(t
	 (format t 
		 "  Current calculated across individual local source node elements, w/o node cap.~%")))


      (cond ((not (vsource-enabled vsrc))
	     (format t "  Source turned off~%"))
	    ((not (source-stimulus-p vsrc))
	     (format t "  No stimulus~%"))
	    ((vsource-use-pulse-list vsrc)
	     (cond-every
	      ((and (element-parameter vsrc 'enable-individual-pulses) (extract-pulse-list vsrc))
	       (loop for pulse in (extract-pulse-list vsrc)
		     do
		     (format
		      t
		      "       ~a mV pulse from ~a ms to ~a ms~%" (nth 2 pulse) (nth 0 pulse) (nth 1 pulse))))
	      ((and (element-parameter vsrc 'enable-pulse-train) (extract-pulse-train-args vsrc))
	       (print-pulse-train-info "mV" (extract-pulse-train-args vsrc)))))
	    ((extract-waveform-function vsrc)
	     (print-spaces t 4)
	     (document-function-args (extract-waveform-function vsrc)))
	    (t (format t "  Explicit waveform~%"))))))

(defun create-pwl-vsource (cell-element &key (WAVEFORM-time-interval *default-waveform-step*) name
					pulse-train-args enable-pulse-train
					(ideal t)
					(enable-individual-pulses t)
					pulse-list (use-pulse-list t)
					waveform-spec)
  (when (get-node-elements-of-type cell-element 'vsource)
    (sim-warning (format nil "create-vsource: node ~a already has vsource, ignoring" (element-name cell-element))))
  (let ((source-name (if name name (format nil "~a-vsrc" (element-name cell-element))))
	(cell-element (element-cell-element cell-element)))
    (or	(element source-name 'vsource)
	(let* ((model (gethash "vsource" *model-hash-table*))
	       (node (element-node cell-element))
	       (vsrc (make-vsource :name source-name :node node ; :plot-current t
				   :cell-element cell-element :enabled t :use-pulse-list use-pulse-list
				   :resistance (max *minimum-vsource-resistance* *vsource-resistance*))))
	  (element-parameter vsrc 'ideal-vsource ideal)
	  (element-parameter vsrc 'enable-pulse-train enable-pulse-train)
	  (element-parameter vsrc 'enable-individual-pulses enable-individual-pulses)
	  (element-parameter vsrc 'pulse-train-args pulse-train-args)
	  (element-parameter vsrc 'waveform-function waveform-spec)
	  (pulse-list vsrc pulse-list)
	  (setq *make-node-w/elements-array* t)
	  (push vsrc (node-elements node))
	  (setf (gethash source-name (VSOURCE-HASH-TABLE)) vsrc)
	  (when waveform-spec (add-waveform vsrc :WAVEFORM-time-interval WAVEFORM-time-interval :waveform-spec waveform-spec :use-menu nil))
	  (setq *vsource* vsrc)))))

    
(defun edit-vsource (source)
  (let* ((vsrc (element source 'vsource))
	 (name (element-name source 'vsource))
	 (dummy1 (* 1e6 (vsource-resistance vsrc)))
	 (dummy11 (or (element-parameter vsrc 'pulse-transition) :fixed-slope))
	 (dummy12 (or (element-parameter vsrc 'pulse-transition-slope) 1000.0))
	 (dummy13 (or (element-parameter vsrc 'pulse-transition-time) 0.1))
	 (dummy14 (element-parameter vsrc 'ideal-vsource))
	 (dummy15 (vsource-enabled vsrc))
	 dummy2)
    (choose-variable-values
     `((dummy14 "Ideal voltage source" :boolean)
       (dummy15 "Enable this source" :boolean)
       (dummy1 ,(format nil "Voltage Source Internal Resistance [ohms, > 0]~%(only for non-ideal sources)") :float)
       (dummy11 "Transition between pulse stimuli defined by:"
	:choose (:fixed-slope :fixed-transition-time))
       (dummy12 "Pulse transition slope (mV/msec)" :float)
       (dummy13 "Pulse transition time (msec)" :float)
       (*steady-state-linear-vclamp-enable* "Initialize with one-step steady state linear method." :boolean)
       (*vclamp-default-magnitude* "Default value for voltage sources (mV)" :float)
       ("Non-ideal source current calculation:" :comment)
       ; (*vsource-current-reference-time* "Reference time for current calculation:" :choose (:dt-midpoint :last-time-point))
       (*vsource-intrinsic-current*
	,(format nil "~A~%~A"
	  "Current calculated from delta V across source resistance"
	  "(otherwise calculated across individual elements):")
	:boolean)
       (*INCLUDE-VSOURCE-CURRENT-LINEAR-and-non-local-COMPONENT*
	,(format nil "~A~%~A"
	  "If current calculation across individual elements,"
	  "include linear and non-local component of voltage current:")
	:boolean)
       (*INCLUDE-local-CAP-CURRENT-IN-VSOURCE*
	,(format nil "~A~%~A"
	  "If current calculation across individual elements,"
	  "include source node capacitance (above flag must be set):")	  
	:boolean)
       (dummy2 "Edit stimulus" :boolean))
     :label (format nil "Setting up parameters of voltage source ~A" name))
    (setf (vsource-enabled vsrc) dummy15)
    (setf (vsource-resistance vsrc) (max (* 1e-6 dummy1) *minimum-vsource-resistance*))
    (when dummy2 (edit-source-stimulus source))
    (element-parameter vsrc 'ideal-vsource dummy14)
    (element-parameter vsrc 'pulse-transition dummy11)
    (element-parameter vsrc 'pulse-transition-slope dummy12)
    (element-parameter vsrc 'pulse-transition-time dummy13)))


(defun ideal-vsource (vsource)
  "Make VSOURCE ideal."
  (element-parameter vsource 'ideal-vsource t))

(defun non-ideal-vsource (vsource &optional resistance)
  "Make VSOURCE non-ideal. When RESISTANCE is included, set the internal resistance of VSOURCE to this
value [in Mohms]. If successful, returns the internal resistance of VSOURCE."
  (let ((vsource (element vsource 'vsource)))
    (when vsource
      (element-parameter vsource 'ideal-vsource nil)
      (when (numberp resistance)
	(setf (vsource-resistance vsource) (s-flt (max resistance *minimum-vsource-resistance*))))
      (vsource-resistance vsource))))



(defun menu-for-vsources ()
  (loop for name in (select-hash-values-menu (VSOURCE-HASH-TABLE) "Select Voltage Sources" :punt-if-only-one-entry t)
  	do (edit-vsource name))) 

(defun vsource-on-node (node)
  (loop for vsrc in (vsources) when (eq (vsource-node vsrc) node) do (return t)))
;
(proclaim '(notinline get-vsource-voltage))
;(proclaim '(inline get-vsource-voltage))
(defun get-vsource-voltage (src time)
					;  (declare (optimize (safety 0) (speed 3) (space 0)))
  (let ((time (if (/= 0.0 (vsource-period src))
		  (mod (coerce-to-double time) (vsource-period src))
		  (coerce-to-double time))))
    (declare (double-float time))
    (cond ((vsource-use-pulse-list src)
	   (extract-pwl-value time src))
	  ((vsource-waveform-array src)
	   (let ((array (the (simple-array single-float *) (vsource-waveform-array src))))
	     (multiple-value-bind (vsrc-time-int-part vsrc-time-fract-part)
		 (truncate (* (vsource-waveform-time-interval-inverse src) (- time (vsource-delay src))))
	       (declare (double-float vsrc-time-fract-part)
			(fixnum vsrc-time-int-part))
	       (cond ((< vsrc-time-int-part (the fn (- (length array) 1)))
		      (check-element-time-step-maximum (vsource-waveform-time-interval-mrt src))
		      (interpolated-array-value-double array vsrc-time-int-part vsrc-time-fract-part))
		     ((= vsrc-time-int-part (the fn (- (length array) 1)))
		      (interpolated-value-double
		       (aref array vsrc-time-int-part) *vclamp-default-magnitude* vsrc-time-fract-part))
		     (t (* 1.0d0 *vclamp-default-magnitude*))))))
	  (t (the double-float (vclamp-default-magnitude src))))))

(defun vclamp-default-magnitude (vsource &optional value)
  (when (numberp value)
    (element-parameter vsource 'default-magnitude (d-flt value)))
  (or (element-parameter vsource 'default-magnitude)
      (d-flt *vclamp-default-magnitude*)))



;; When this is t, not good. (*INPUT-TIME*) is either at the time step midpoint, or at the prediction time. 
(defvar *use-*input-time*-for-vsources* nil)
(defun vsource-ref-time ()
  (the sf (if *use-*input-time*-for-vsources*
	      (*input-time*)
	      (the sf (* *mrt* 0.5 (+ *time-step* *sim-time-n* *sim-time-n*))))))

(proclaim '(inline vsource-current-ref-time))
(defun vsource-current-ref-time ()
  (*t[n+1]*))


(defun ideal-vsources-p ()
  (loop for src in (vsources) when (element-parameter src 'ideal-vsource) do (return t)))

(defun check-fixed-voltage-nodes ()
  (let (reorder-matrix)
    (loop for src in (vsources) do
	  (setq reorder-matrix
		(or reorder-matrix
		    (xor
	   (node-has-ideal-voltage-source (vsource-node src))
	  (and 
	   (element-parameter src 'ideal-vsource)
	   (element-parameter src 'enabled t)
	   (vsource-enabled src)))
		    
		    ; (or (xor (node-has-ideal-voltage-source (vsource-node src))
;			     (element-parameter src 'ideal-vsource))
;			(and (element-parameter src 'ideal-vsource)
;			     (xor (vsource-enabled src) (element-parameter src 'enabled))))

		    

		    ))

	  

	  
	  (element-parameter src 'enabled (vsource-enabled src))
	  (setf (node-has-ideal-voltage-source (vsource-node src))
		(and (vsource-enabled src)
		     (element-parameter src 'ideal-vsource))))
    
    (when reorder-matrix
      (setq *make-segment-lists* t)
      (process-circuit-structure t))
    reorder-matrix)
  (unless (or (ideal-vsources-p) (= *num-nodes* *CORE-NODE-ARRAY-LENGTH*))
    (format t "Reprocessing changed circuit structure...~%")
    (process-circuit-structure t))
  )



	  




(proclaim '(inline eval-vsource))
;; Calculates the new voltage in time for a pwl voltage source. Evaluates vsource contribution to
;; node in the same manner as a channel. 
(defun eval-vsource (src)
  (declare (optimize (safety 0) (speed 3) (space 0)))
  (when (vsource-enabled src)
    (let* ((vsource-ref-time (the sf (vsource-ref-time)))
	   (vsource-ref-time-w-delay (the sf (if (vsource-use-pulse-list src)
						 vsource-ref-time
						 (the sf (- vsource-ref-time (vsource-delay src)))))))
      (when (> vsource-ref-time-w-delay 0.0)
	(let ((conductance (the df (/ 1.0d0 (vsource-resistance src)))) ; uS
	      (e-rev (get-vsource-voltage src vsource-ref-time)))
	  (setf ; (vsource-voltage src) e-rev
		(node-current (vsource-node src))
		(the df (+ (node-current (vsource-node src)) (the df (* conductance (- e-rev)))))
		(node-jacobian (vsource-node src))
		(the df (+ (node-jacobian (vsource-node src)) conductance)))
	  (setf (vsource-voltage src) (coerce-to-single (get-vsource-voltage src (vsource-current-ref-time))))
	  (when *debug-time-trace*
	    (format t "   VSRC: time ~A - v-e-rev: ~A, crnt: ~A ~%"
		    vsource-ref-time e-rev (the df (* conductance (- e-rev)))))))))
  nil)

(defun eval-fixed-voltage-node-vsource (src)
  (declare (optimize (safety 0) (speed 3) (space 0)))
  (when (vsource-enabled src)
    (let* ((vsource-ref-time (the sf (vsource-ref-time)))
	   (vsource-ref-time-w-delay
	    (if (vsource-use-pulse-list src) vsource-ref-time (- vsource-ref-time (vsource-delay src)))))
      (when (> vsource-ref-time-w-delay 0.0)
	(setf (vsource-voltage src) (coerce (get-vsource-voltage src (vsource-current-ref-time)) 'single-float)))
      nil)))


;; EVAL-FIXED-VOLTAGE-NODES Evaluation of ideal voltage sources - see numerical.doc.
(defun adjacent-nodes-and-g-axials (element)
  (let* ((elt (typecase element
		(vsource (vsource-cell-element element))
		(t (element-cell-element element))))
	 (params (element-parameters-fast elt)))
    (or (element-parameter-fast 'adjacent-nodes-and-g-axials params)
	(set-element-parameter-fast
	 elt 'adjacent-nodes-and-g-axials
	 (typecase elt
	   (segment
	    (cons (list (element-physical-node (proximal-cell-element elt))
			(segment-g-axial elt))
		  (loop for seg in (distal-segments elt)
			collect (list (element-physical-node seg) (segment-g-axial seg)))))
	   (soma (loop for seg in (trunk-segments (soma-cell elt))
		       collect (list (element-physical-node seg) (segment-g-axial seg)))))
	 params))))

(defun eval-fixed-voltage-nodes ()
  (declare (optimize (safety 0) (speed 3) (space 0)))
  (when *fixed-vsource-list* 
    (loop for src in *fixed-vsource-list* do
	  (let* ((node (vsource-node src))
		 (vsource-ref-time (the sf (vsource-ref-time)))
		 (vsource-ref-time-w-delay
		  (the sf (if (vsource-use-pulse-list src) vsource-ref-time (- vsource-ref-time (vsource-delay src))))))
	    (when (> vsource-ref-time-w-delay 0.0)
	      (let ((voltage (get-vsource-voltage src (vsource-current-ref-time))))
		(setf (vsource-voltage src) (coerce-to-single voltage))
		(element-parameter node 'fixed-voltage voltage)
		(setf (node-voltage-n+1 node) voltage)))
	    (let ((src-voltage (node-voltage-n+1 node)))
	      (loop for node-g-axial in (adjacent-nodes-and-g-axials src) do
		    (let ((destination-node (car node-g-axial))
			  (g-axial (the df (cadr node-g-axial))))
		      (setf (node-current destination-node) (- (node-current destination-node) (* g-axial src-voltage))
			    (node-jacobian destination-node) (+ (node-jacobian destination-node) g-axial)))))))
    nil))


#|
(defun eval-fixed-node-voltages ()
;  (declare (optimize			; (safety 0)
;	    (speed 3) (space 0)))
  
  (loop for src in *fixed-vsource-list* do
	(let* ((node (vsource-node src))
	       (vsource-ref-time (the sf (vsource-ref-time)))
	       (vsource-ref-time-w-delay (the sf (if (vsource-use-pulse-list src)
						     vsource-ref-time
						     (the sf (- vsource-ref-time (vsource-delay src)))))))
	  (when (> vsource-ref-time-w-delay 0.0)
	    (let ((voltage (get-vsource-voltage src (vsource-current-ref-time))))
	      (setf (vsource-voltage src) (coerce-to-single voltage))
	      (element-parameter node 'fixed-voltage voltage)
	      (setf (node-voltage-n+1 node) voltage)))
	
	  (let ((src-voltage (node-voltage-n+1 node)))
	    (loop for destination-elt in
		  (typecase (vsource-cell-element src)
		    (segment
		     (cons (proximal-cell-element (vsource-cell-element src))
			   (distal-segments (vsource-cell-element src))))
		    (soma (trunk-segments (soma-cell (vsource-cell-element src)))))
				  
		  do
		  (let ((destination-node (element-physical-node destination-elt))
			(g-axial (the df (segment-g-axial destination-elt))))
		    (setf (node-current destination-node)
			  (- (node-current destination-node) (* g-axial src-voltage))
			  (node-jacobian destination-node)
			  (+ (node-jacobian destination-node) g-axial)))))))	    
  nil)
|#

(defun eval-all-non-ideal-vsources ()
  (loop for src in *non-ideal-vsource-list* do (eval-vsource src))
  nil)

(defun advance-vsource (src)
  (setf (vsource-last-voltage src) (vsource-voltage src))
  nil)

(defun advance-vsources ()
  (loop for src in *fixed-vsource-list* do (advance-vsource src))
  (loop for src in *non-ideal-vsource-list* do (advance-vsource src)))


  
(defun init-vsource (src)
  (setf (vsource-last-voltage src) (float *vclamp-default-magnitude*)
	(vsource-voltage src) (float *vclamp-default-magnitude*))
  nil)

(defun INIT-VSOURCEs ()
  (loop for src in *non-ideal-vsource-list* do (init-vsource src))
  (loop for src in *fixed-vsource-list* do (init-vsource src)))




(defvar *INCLUDE-local-CAP-CURRENT-IN-VSOURCE* t)
(defvar *debug-vsource-current* nil)
(defvar *include-vsource-current-linear-and-non-local-component* t)
(defvar *vsource-voltage-reference-n* t)
(defvar *vsource-intrinsic-current* t)
(defvar *debug-vsource-voltage* nil)
(defvar *vsource-current-reference-time* :dt-midpoint)

;; GET-VSOURCE-CURRENT Called after a successful time step, from SAVE-VSOURCE-DATA.

(defun debug-get-soma-current-for-vsource-message (vsrc-node-soma soma-total-current)
  (format t "  soma ~A ~,2f~%" vsrc-node-soma soma-total-current))

(defun get-soma-current-for-vsource (vsrc-node-voltage vsrc-node-soma vsrc-node-dvdt)
  (declare (optimize (safety 0) (speed 3) (space 0))
	   (double-float vsrc-node-voltage vsrc-node-dvdt))
  (if *include-vsource-current-linear-and-non-local-component*
      (let* ((soma-shunt-current (if (soma-include-shunt vsrc-node-soma)
				     (* (soma-g-shunt vsrc-node-soma) vsrc-node-voltage)
				     0.0d0))
	     (soma-capacitance-current (* (soma-capacitance vsrc-node-soma) vsrc-node-dvdt))
	     (soma-leak-current (* (soma-g-leak vsrc-node-soma)
				   (- vsrc-node-voltage (soma-v-leak vsrc-node-soma))))
	     (soma-total-current (+ soma-shunt-current soma-capacitance-current soma-leak-current)))
	(when *debug-vsource-current* (debug-get-soma-current-for-vsource-message vsrc-node-soma soma-total-current))
	soma-total-current)
      0.0d0))


(defun debug-get-seg-current-for-vsource-message (vsrc-node-segment segment-total-current)
  (format t "  seg ~A ~,2f~%" vsrc-node-segment segment-total-current))

;; (proclaim '(inline get-seg-current-for-vsource))
(defun get-seg-current-for-vsource (vsrc-node-voltage vsrc-node-segment vsrc-node-dvdt vsrc-node)
  (declare (optimize (safety 0) (speed 3) (space 0))
	   (double-float vsrc-node-voltage vsrc-node-dvdt))
  (if *include-vsource-current-linear-and-non-local-component*
      (let* ((segment-distal-node (segment-node-2 vsrc-node-segment))
	     (vsrc-connected-to-segment-distal-node-p (eq vsrc-node segment-distal-node))
	     (segment-other-node (if vsrc-connected-to-segment-distal-node-p
				     (segment-node-1 vsrc-node-segment)
				     segment-distal-node))
	     (segment-other-node-voltage (case *vsource-current-reference-time*
					   (t ; :dt-midpoint
					    (node-voltage-n+1 segment-other-node))))
	     (segment-axial-current
	      (* (segment-g-axial vsrc-node-segment) (- vsrc-node-voltage segment-other-node-voltage)))
	     (segment-capacitance-current
	      (if vsrc-connected-to-segment-distal-node-p
		  (* (segment-capacitance vsrc-node-segment) (the df vsrc-node-dvdt))
		  0.0d0))
	     (segment-leak-current
	      (if vsrc-connected-to-segment-distal-node-p
		  (* (segment-g-leak vsrc-node-segment) (- vsrc-node-voltage (segment-v-leak vsrc-node-segment)))
		  0.0d0))
	     (segment-total-current (+ segment-leak-current segment-capacitance-current segment-axial-current)))
	(when *debug-vsource-current* (debug-get-seg-current-for-vsource-message vsrc-node-segment segment-total-current))
	segment-total-current)
      0.0d0))



(defun debug-get-isource-current-for-vsource-message (vsrc-node-isource)
  (format t "  isource ~,2f~%" (- (isource-current vsrc-node-isource))))

(defun get-isource-current-for-vsource (vsrc-node-isource)
  (when *debug-vsource-current* (debug-get-isource-current-for-vsource-message vsrc-node-isource))
  (the df (- 0.0d0 (get-isource-current vsrc-node-isource))))


(defun debug-get-ch-current-for-vsource-message (vsrc-node-ch ch-current)
  (format t "  ch ~A ~,2f~%" vsrc-node-ch ch-current))

(defun get-ch-current-for-vsource (vsrc-node-ch)
  (let ((ch-current (the df (get-channel-current-not-inline vsrc-node-ch))))
    (when *debug-vsource-current* (debug-get-ch-current-for-vsource-message vsrc-node-ch ch-current))
    ch-current))

(defun debug-get-syn-current-for-vsource-message (vsrc-node-syn syn-current)
  (format t " ~Ams: ~A ~,2f~%" *real-time* vsrc-node-syn syn-current))

(defun get-syn-current-for-vsource (vsrc-node-syn vsrc-node-voltage)
  (let ((syn-current (the df (* (synapse-conductance vsrc-node-syn)
				(- vsrc-node-voltage (synapse-e-rev vsrc-node-syn))))))
    (when *debug-vsource-current* (debug-get-syn-current-for-vsource-message vsrc-node-syn syn-current))
    syn-current))

;; Returns DF.
;; (proclaim '(inline collect-node-currents-for-vsource))
(proclaim '(notinline collect-node-currents-for-vsource))
(defun collect-node-currents-for-vsource (vsrc-node vsrc vsrc-voltage vsrc-node-voltage vsrc-node-dvdt)
  (declare (optimize (safety 0) (speed 3) (space 0))
	   (double-float vsrc-node-voltage vsrc-node-dvdt)
	   (single-float vsrc-voltage))
  (loop for vsrc-node-elt in (node-elements vsrc-node) summing
	(the df (typecase vsrc-node-elt
		  (isource (get-isource-current-for-vsource vsrc-node-elt))
		  (soma (get-soma-current-for-vsource vsrc-node-voltage vsrc-node-elt vsrc-node-dvdt))
		  (channel (get-ch-current-for-vsource vsrc-node-elt))
		  (synapse (get-syn-current-for-vsource vsrc-node-elt vsrc-node-voltage))
		  (segment (get-seg-current-for-vsource vsrc-node-voltage vsrc-node-elt vsrc-node-dvdt vsrc-node))
		  (t 0.0d0))) into result double-float
	finally (return result)))

(defun debug-vsource-messages (vsrc vsrc-voltage vsrc-node-voltage vsrc-node-dvdt current)
  (when *debug-vsource-voltage*
    (format t "time: ~A vsource voltage: ~A, node-voltage: ~A~%" *real-time* vsrc-voltage vsrc-node-voltage))
  (when *debug-vsource-current*
    (format t "time ~A vsource current (vsrc voltage ~,2f, node-volt ~,2f ~a):~% "
	    *real-time*	(vsource-last-voltage vsrc)
	    vsrc-node-voltage (if vsrc-node-dvdt
				  (format nil "dvdt ~,2f" vsrc-node-dvdt)
				  (format nil "dvdt not computed")))
    (format t "~%    total current: ~,2f ~%" (coerce-to-single current))))

(defun get-vsource-current-vsrc-voltage (vsrc)
  (the sf (case *vsource-current-reference-time*
	    (t				; :dt-midpoint
	     (vsource-voltage vsrc)
					; (* 0.5 (+ (vsource-voltage vsrc) (vsource-last-voltage vsrc)))
					;(vsource-voltage vsrc)
					;(vsource-last-voltage vsrc)
	     )
					; (:last-time-point (vsource-last-voltage vsrc))
	    )))
(defun get-vsource-current-vsrc-node-voltage (vsrc-node)
  (the df (case *vsource-current-reference-time*
	    (t				; :dt-midpoint
	     (node-voltage-n+1 vsrc-node)
	     )
					; (:last-time-point (node-voltage-n vsrc-node))
	    )))

(defun get-vsource-current-vsrc-node-dvdt (vsrc-node)
  (the df (if (and *include-vsource-current-linear-and-non-local-component*
		   *INCLUDE-local-CAP-CURRENT-IN-VSOURCE*)
	      (case *vsource-current-reference-time*
		(t			; :dt-midpoint
					; (coerce-to-single (node-dvdt-n vsrc-node))
		 (node-dvdt-n vsrc-node))
					; (:last-time-point (* 0.5 (+ (node-dvdt-n vsrc-node) (node-dvdt-n-1 vsrc-node))))
		)
	      0.0d0)))


(defun get-vsource-current (vsrc)
  (declare (optimize			; (safety 0)
	    (speed 3) (space 0)))
  (if (vsource-enabled vsrc)
      (let* ((vsrc-node (vsource-node vsrc))
	     (vsrc-voltage (the sf (get-vsource-current-vsrc-voltage vsrc)))
	     (vsrc-node-voltage (the df (get-vsource-current-vsrc-node-voltage vsrc-node))))
	(if (and *vsource-intrinsic-current* (not (element-parameter-fast 'ideal-vsource (vsource-parameters vsrc))))
	    (* 1.0d0 (/ (- vsrc-voltage vsrc-node-voltage) (vsource-resistance vsrc)))
	    (let* ((vsrc-node-dvdt (the df (get-vsource-current-vsrc-node-dvdt vsrc-node)))
		   (current (the df (collect-node-currents-for-vsource
				     vsrc-node vsrc vsrc-voltage vsrc-node-voltage vsrc-node-dvdt))))
	      (when (or *debug-vsource-voltage* *debug-vsource-current*)
		(debug-vsource-messages vsrc vsrc-voltage vsrc-node-voltage vsrc-node-dvdt current))
	      (coerce-to-single current))))
      0.0))

(defun vsource-current-value (vsource)
  (get-vsource-current vsource))

(defun vsource-voltage-value (vsource)
  (get-vsource-current-vsrc-voltage vsource))








#|
;;; not used
(defun vsource-return-current (vsrc)
  (let ((vsource-supplied-current 0)(voltage (node-voltage-n+1 (vsource-node vsrc))))
    (dolist (elt (node-elements (vsource-node vsrc)))
      (setq vsource-supplied-current
	    (+ vsource-supplied-current
	       (cond ((eq (named-structure-symbol elt) 'isource)
		      (isource-current elt))
		     ((eq (named-structure-symbol elt) 'segment)
		      (let (v1 v2 v-diff i1 i2)
			(setf v1 (get-segment-voltage-1 elt))
			(setf v2 (get-segment-voltage-2 elt))
			(setf v-diff (- v1 v2))
					; calculate the current
			(setf i1 (* v-diff (segment-g-axial elt)))
			(setf i2 (+ (- i1) (* (segment-g-leak elt)
					      (- v2 (segment-v-leak elt)))))
			(if (eq (vsource-node vsrc) (segment-node-1 elt))
			    (- i1) (- i2))
			))
    
		     ((eq (named-structure-symbol elt) 'soma)
		      (- (+ (* (soma-g-shunt elt) voltage)
			    (* (soma-g-leak elt)
			       (- voltage (soma-v-leak elt))))))
		     ((eq (named-structure-symbol elt) 'channel)
		      (channel-current elt))
		     (t 0)))))
    vsource-supplied-current))


(defun vsource-return-charge ())

(defun vsource-return-conductance ())
|#
