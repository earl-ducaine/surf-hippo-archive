;;; Common-lisp; package: surf ; base: 10;  -*-
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


;;; SYS Source file: source.lisp

(in-package "SURF-HIPPO")


;; Functions relevant to both current and voltage sources

;; ******* Includes inline functions for vsource.lisp and isource.lisp *************

(defun source-units-string (source)
  (typecase (element source)
    (vsource "mV")
    (isource "nA")))

(defun source-stimulus-p (source)
  (true-p (or (extract-waveform-function source)
	      (source-waveform-array source)
	      (pulse-list source)
	      (pulse-train source))))

(defun sources (&optional cell)
  "Return a list of all current and voltage sources."
  (nconc (isources cell) (vsources cell)))

(defun set-pwl-list (source pwl-list)
  (typecase source
    (isource (setf (isource-pwl-list source) pwl-list))
    (vsource (setf (vsource-pwl-list source) pwl-list))))


(defun source-waveform-time-interval-inverse (source)
  (let ((source (element source)))
    (typecase source
      (isource (isource-waveform-time-interval-inverse source))
      (vsource (vsource-waveform-time-interval-inverse source)))))

(defun source-use-pulse-list (source)
  (let ((source (element source)))
    (typecase source
      (isource (isource-use-pulse-list source))
      (vsource (vsource-use-pulse-list source)))))

(defun source-waveform-array (source)
  (let ((source (element source)))
    (typecase source
      (isource (isource-waveform-array source))
      (vsource (vsource-waveform-array source)))))

(defun set-source-use-pulse-list (source)
  (let ((source (element source)))
    (typecase source
      (isource (setf (isource-use-pulse-list source) t))
      (vsource (setf (vsource-use-pulse-list source) t)))))

(defun turn-on-source (source)
  (let ((source (element source)))
    (typecase source
      (isource (setf (isource-enabled source) t))
      (vsource (setf (vsource-enabled source) t)))))

(defun turn-off-source (source)
  (let ((source (element source)))
    (typecase source
      (isource (setf (isource-enabled source) nil))
      (vsource (setf (vsource-enabled source) nil)))))

(defun add-source (element type-name &optional name pulse-list)
  (atomize-list
   (loop for cell-element in (coerce-to-list (element-cell-element element)) collect
	 (case (if (stringp type-name) (read-from-string type-name) type-name)
	   (isource (create-pwl-isource cell-element :name name :pulse-list pulse-list))
	   (vsource (create-pwl-vsource cell-element :name name :pulse-list pulse-list))))))

(defun add-isource (element &optional name pulse-list)
  "Adds current source to cell elements associated with ELEMENT. Source is called NAME, if supplied,
or is given by eltname-isrc, where eltname is the name of cell-element. Creates new source only if
one does not exist of the derived name. Optional PULSE-LIST may also be supplied. Returns the
source\(s\). Note that if ELEMENT refers to a cell, then an isource is added to that cell's soma."
  (let* ((element element)
	 (cell-element (if (cell-p element)
			 (cell-soma element)
			 (element-cell-element element))))
    (when cell-element (create-pwl-isource cell-element :name name :pulse-list pulse-list))))

(defun add-vsource (element &optional name (ideal t) pulse-list default-magnitude)
  "Adds voltage source to cell elements associated with ELEMENT, if no voltage source is already
there. Source is called NAME, if supplied, or is given by eltname-vsrc, where eltname is the name of
cell-element. If IDEAL then ideal voltage sources are created. An optional PULSE-LIST may also be
supplied. Returns the source\(s\). When DEFAULT-MAGNITUDE is a number [mV, default NIL], this value
is used as the default magnitude for the source. Note that if ELEMENT refers to a cell, then a
vsource is added to that cell's soma."
  (let* ((element element)
	 (cell-element (if (cell-p element)
			 (cell-soma element)
			 (element-cell-element element))))
    (when cell-element
      (let ((vsource (create-pwl-vsource cell-element :name name :pulse-list pulse-list)))
	(element-parameter vsource 'ideal-vsource ideal)
	(vclamp-default-magnitude vsource default-magnitude)
	vsource))))

(defun cell-isource (element)
  "Returns a current source, from the soma preferably, associated with the cell associated with
ELEMENT, if there are any such sources."
  (let ((cell (element-cell element)))
    (when cell
      (or (car (element-isources (cell-soma cell)))
	  (loop for isrc in (isources)
		when (eq (element-cell isrc) cell)
		do (return isrc))))))

(defun cell-vsource (element)
  "Returns a voltage source, from the soma preferably, associated with the cell associated with
ELEMENT, if there are any such sources."
  (let ((cell (element-cell element)))
    (when cell
      (or (car (element-vsources (cell-soma cell)))
	  (loop for isrc in (vsources)
		when (eq (element-cell isrc) cell)
		do (return isrc))))))
  
(defun sources-menu ()
  (let (dummy1 dummy2 dummy3 dummy4)
    (choose-variable-values
     `((dummy1 "Add current sources" :boolean)
       (dummy3 "Edit current source parameters" :boolean)
       (dummy2 "Add voltage sources" :boolean)
       (dummy4 "Edit voltage source parameters" :boolean))
     :label "Editing current and voltage sources")
    (cond-every
     (dummy1 (add-sources-menu 'isource))
     (dummy3 (menu-for-isources))
     (dummy2 (add-sources-menu 'vsource))
     (dummy4 (menu-for-vsources)))))

(defun edit-source (source)
  "For editing all SOURCE parameters."
  (typecase (element source)
    (vsource (edit-vsource source))
    (isource (edit-isource source))))

(defun add-sources-menu (type-sym)
  (let* ((type-string (case type-sym (isource "current source") (vsource "voltage source")))
	 (original-sources (list-of-all-things type-sym))
	 (original-source-node-names (loop for source in original-sources
					   collect (element-name (element-physical-node source))))
	 (new-source-node-names
	  (nconc (if (ask-for-element-modifications (format nil "Add/remove segment ~ss" type-string)
						    (length (segments)) type-string)
		     (select-hash-values-menu (SEGMENT-HASH-TABLE) (format nil "Select Segments for ~a" type-string)
					      :selected-list (rem-not-in-keys original-source-node-names
									      (namelist-of-all-things 'segment)))
		   (rem-not-in-keys original-source-node-names (namelist-of-all-things 'segment)))
		 (select-hash-values-menu (SOMA-HASH-TABLE) (format nil "Select Soma for ~a" type-string)
					  :selected-list (rem-not-in-keys original-source-node-names
									  (namelist-of-all-things 'soma))))))
    (loop for original-source-node-name in original-source-node-names
	  unless (member original-source-node-name new-source-node-names :test 'equal)
	  do (erase-element (car (get-node-elements-of-type original-source-node-name type-sym))))
    (loop for node-name in new-source-node-names
	  unless (member node-name original-source-node-names :test 'equal)
	  do (case type-sym
	       (isource (create-pwl-isource node-name))
	       (vsource (create-pwl-vsource node-name))))))


(defun pulse-list (source &optional (pulse-list nil pulse-list-supplied))
  "For adding a PULSE-LIST to SOURCE, where the format of PULSE-LIST is either:

  (pulse-1 pulse-2 ...)

or for just a single pulse:

  pulse

where the format of each pulse is as follows:

  (start-time stop-time amplitude)

For example (4 6 .1) applied to a current source defines a 0.1nA pulse from 4 to 6 milliseconds.

The amplitude for current sources is in nA, and for voltage sources is in mV. This function will
also set the :USE-PULSE-LIST slot for the source. If called with only the SOURCE arg, the pulse-list
currently assigned to the source will be returned. If there is an explicit NIL PULSE-LIST arg any
pulse-list assigned to SOURCE will be cleared.

Pulse must be separated by a minimum time which is a function of the transition speed, and thus they
cannot overlap in time. Typically the minimum separation is on the order of 0.005 milliseconds."
  (let ((source (element source))
	(pulse-list (cleanup-pulse-list pulse-list)))
    (when (or (isource-p source) (vsource-p source))
      (if pulse-list-supplied
	  (progn (element-parameter source 'pulse-list pulse-list)
		 (element-parameter source 'enable-individual-pulses (true-p pulse-list))
		 (if pulse-list
		     (progn (set-source-use-pulse-list source)
			    (setup-source-values source))
		     (typecase source
		       (isource (setf (isource-pwl-list source) nil)
				(setf (isource-use-pulse-list source) nil))
		       (vsource (setf (vsource-pwl-list source) nil)
				(setf (vsource-use-pulse-list source) nil)))))
	  (element-parameter source 'pulse-list)))))

;; Backward compatibility
(defun add-pulse-list (source &optional pulse-list clear-if-nil)
  (if (and clear-if-nil (not pulse-list))
      (pulse-list source nil)
      (pulse-list source pulse-list)))


(defun cleanup-pulse-list (pulse-list)
      ;; Clean up the pulse list a bit by deleting pulses with duplicate start times and ordering
      ;; the pulses wrt time. Separate the pulse components into start times, stop times, and mags.
      ;; this nonsense to avoid "-0.0". also converts all numbers to single floats.
  (let ((pulse-list 
	 (loop for pulse in (if (consp (car pulse-list)) pulse-list (list pulse-list))
	       when (car pulse)
	       collect (list (s-flt (nth 0 pulse)) (s-flt (nth 1 pulse))
			     (s-flt (if (= 0 (nth 2 pulse)) 0.0 (nth 2 pulse)))))))
    (sort (delete-duplicates pulse-list :test '= :key 'car) #'> :key 'car)))

(defun document-pulse-list (source)
  (format t "(pulse-list ~s '~A)~%" (element-name source) (extract-pulse-list source)))

(defun document-waveform-function (source)
  (let (*print-pretty*)
    (format t "(element-parameter ~s 'waveform-function '~S)"
	    (element-name source)
	    (element-parameter source 'waveform-function))))

(defun print-create-sources-for-cell (cell &optional (indent-output 0))
  (loop for source in (sources cell) do
	(typecase source
	  (vsource (print-create-vsource source indent-output))
	  (isource (print-create-isource source indent-output)))))
			       

(defun print-pulse-train-info (units-string pulse-train-args)
  (format t " ~a ~a pulse train from ~a ms to ~a ms, pulse duration ~a ms, delay ~a ms, period ~a ms~%"
	  (or (cdr-assoc :amplitude pulse-train-args) 1.0)
	  units-string
	  (or (cdr-assoc :start pulse-train-args) 0.0)
	  (or (cdr-assoc :stop pulse-train-args) *user-stop-time*)
	  (or (cdr-assoc :duration pulse-train-args) 1.0)
	  (or (cdr-assoc :delay pulse-train-args) 0.0)
	  (or (cdr-assoc :period pulse-train-args) 2.0)))


;; EXTRACT-SOURCE-LISTs Given a source, looks at the :PARAMETERS slot of the source for the driving
;; function info (if USE-PULSE-LIST is true, then looks for the ASSOC of 'PULSE-LIST, otherwise for
;; the ASSOC of 'WAVEFORM-FUNCTION).
(defun extract-source-lists (source)
  (values (extract-pulse-list source)
	  (extract-pulse-train-args source)
	  (extract-waveform-function source)
	  (element-parameter (element source) 'user-pwl-list)))

(defun extract-pulse-list (source)
  (let ((source-list (element-parameter source 'pulse-list)))
    (when source-list (sort (copy-list source-list) '< :key 'car))))

(defun extract-waveform-function (source) (element-parameter source 'waveform-function))

(defun extract-pulse-train-args (source) (element-parameter source 'pulse-train-args)) 

(defun plot-source-waveform (source)
  (let* ((source (element source))
	 (delta-t (if (source-use-pulse-list source) 0.1 (/ 1.0 (source-waveform-time-interval-inverse source)))))
    (plot-timed-data (if (source-use-pulse-list source)
		       (list (loop for time from 0.0 to *user-stop-time* by delta-t
				   collect (or (extract-pwl-value time source) 0.0)))
		       (source-waveform-array source))
		     (list "") nil
		     :delta-t delta-t
		     :x-label "ms"
		     :title (format nil "~A for ~A"
				    (if (source-use-pulse-list source) "Pulse List Waveform"
					(car (extract-waveform-function source)))
				    (element-name source))
		     :y-label (typecase source
				(isource "nA")
				(vsource "mV")))))

;;; EDIT-SOURCE-STIMULI
(defun edit-source-stimuli ()
  (let ((constant-current-elements (constant-current-elements)))
    (loop for src-or-sym in
	  (choose-list-values-from-keys
	   (cons
	    (list (format nil "Edit constant currents~A"
			  (if constant-current-elements
			      (format nil "~%(~A element~:P now w/current)"
				      (length constant-current-elements))
			      ""))
		  :constant-current-element-edit)
	    (loop for src in (sources) collect (list (element-name src) src)))
	   nil :label "Choose Sources to Modify")
	  do (case src-or-sym
	       (:constant-current-element-edit
		(let ((constant-current-menu-directives
		       (choose-list-values-from-keys
			(cons (when constant-current-elements
				(list (format nil "First clear all~%constant currents") :clear-constant-currents))
			      (cons (list (format nil "Add constant current~%to cell elements")
					  :constant-current-element-global-edit)
				    (loop for constant-current-element in constant-current-elements
					  collect (list (element-name constant-current-element) constant-current-element))))
			nil  :label "Editing Constant Current")))
		  (when (member :clear-constant-currents constant-current-menu-directives)
		    (clear-constant-currents))
		  (constant-current-menu
		   (loop for elt-or-sym in constant-current-menu-directives
			 nconc (case elt-or-sym
				 (:constant-current-element-global-edit (cell-elements))
				 (t (list (element-cell-element elt-or-sym))))))))
	       (t (edit-source-stimulus src-or-sym))))))



(defun edit-source-stimulus (source)
  (when (and *surf-interactive* (not *automatic-run*))
    (let (dummy20)
      (loop until dummy20 do
	    (setup-source-values (element source))
	    (let* ((source (element source))
		   (dummy1 (if (source-use-pulse-list source) :pulses :waveform))
		   (dummy2 (element-enabled-p source))
		   (dummy3 (element-enabled-p source)))
	      (choose-variable-values '((dummy1 "Select form:" :choose (:pulses :waveform))
					(dummy2 "Turn on this source" :boolean)
					(dummy3 "Edit stimulus" :boolean))
				      :label (format nil "Form of Stimulus for ~A" (element-name source)))
	      (if dummy2 (turn-on source) (turn-off source))
	      (setq dummy20 t)
	      (when dummy3
		(let ((*automatic-run* nil)
		      (use-pulse-list (case dummy1 (:waveform nil) (:pulses t))))
		  (typecase source
		    (isource (setf (isource-use-pulse-list source) use-pulse-list))
		    (vsource (setf (vsource-use-pulse-list source) use-pulse-list)))
		  (if use-pulse-list
		      (edit-pulse-spec source)
		      (add-waveform (element-name source) :waveform-spec (extract-waveform-function source) :use-menu t))
		  (when (and
			 (source-stimulus-p source)
			 (go-ahead-menu (format nil "Plot ~A waveform" (element-name source)) "Authorization" nil))
		    (plot-source-waveform source)
		    (setq dummy20 (go-ahead-menu (format nil "Quit ~A edit" (element-name source))))))))))))

(defun edit-pulse-train-args (pulse-train-args source) 
  (let ((dummy1 (or (cdr-assoc :start pulse-train-args) 0.0))
	(dummy2 (or (cdr-assoc :stop pulse-train-args) *user-stop-time*))
	(dummy3 (or (cdr-assoc :delay pulse-train-args) 0.0))
	(dummy4 (or (cdr-assoc :duration pulse-train-args) 1.0))
	(dummy5 (or (cdr-assoc :period pulse-train-args) 2.0))
	(dummy6 (or (cdr-assoc :amplitude pulse-train-args) 1.0))
	(dummy7 (when pulse-train-args t)))
    (choose-variable-values
     `((dummy1 "Start time [ms]" :float)
       (dummy2 "Stop time [ms]" :float)
       (dummy3 "Delay for each pulse [ms]" :float)
       (dummy4 "Pulse duration [ms]" :float)
       (dummy5 "Pulse period [ms]" :float)
       (dummy6 ,(format nil "Pulse level ~a" (typecase source (isource "nA") (vsource "mV"))) :float))
     :label (format nil "Parameters for Source ~a Pulse Train" (element-name source)))
    (when (and (= (+ dummy1 dummy3) 0.0) dummy10) (setq *vclamp-default-magnitude* dummy6))
    (element-parameter source 'pulse-train-args
		       (list (cons :start dummy1)
			     (cons :stop dummy2)
			     (cons :delay dummy3)
			     (cons :duration dummy4)
			     (cons :period dummy5)
			     (cons :amplitude dummy6)))))

(defun square-wave-pulse-train (source pulse-period amplitude &optional (duration *user-stop-time*) (start 0.0) (delay 0.0))
  (PULSE-TRAIN source start duration delay (/ pulse-period 2) pulse-period amplitude))

(defun pulse-train (source &optional (start nil start-supplied) stop delay duration period amplitude)
  "When START, STOP, DELAY, DURATION, PERIOD, AMPLITUDE are numbers, assign the corresonding pulse
train specification to SOURCE and enable the pulse train. All time args in milliseconds. AMPLITUDE
is in nA or mV depending on whether SOURCE refers to a current or voltage source, respectively. If
called with only the SOURCE arg, the list of pulse train specs [same order as PULSE-TRAIN args]
currently assigned to the source will be returned. If there is an explicit NIL START arg any pulse
train assigned to SOURCE will be cleared, and pulse train will be disabled for this SOURCE. "
  (let ((source (element source)))
    (when (or (isource-p source) (vsource-p source))
      (if start-supplied
	  (progn
	    (element-parameter source 'enable-pulse-train (true-p start))
	    (if start
		(set-source-use-pulse-list source)
		(typecase source
		  (isource (setf (isource-pwl-list source) nil)
			   (setf (isource-use-pulse-list source) nil))
		  (vsource (setf (vsource-pwl-list source) nil)
			   (setf (vsource-use-pulse-list source) nil))))
	    (element-parameter source 'pulse-train-args
			       (when (and (numberp start)
					  (numberp stop)
					  (numberp delay)
					  (numberp duration)
					  (numberp amplitude))
				 (list (cons :start (s-flt start))
				       (cons :stop (s-flt stop))
				       (cons :delay (s-flt delay))
				       (cons :duration (s-flt duration))
				       (cons :period (s-flt period))
				       (cons :amplitude (s-flt amplitude))))))
	  (element-parameter source 'pulse-train-args)))))

(defun set-pulse-train-args (source start stop delay duration period amplitude)
  (element-parameter source 'enable-pulse-train t)
  (element-parameter source 'pulse-train-args
		     (list (cons :start (s-flt start))
			   (cons :stop (s-flt stop))
			   (cons :delay (s-flt delay))
			   (cons :duration (s-flt duration))
			   (cons :period (s-flt period))
			   (cons :amplitude (s-flt amplitude)))))



(defmacro pulse-number-1 (pulse-list pulse-number)
  `(nth (1- ,pulse-number) ,pulse-list))


(defun source-pulse-list-menu (pulse-list specified-start-value default-mag source)
  (let ((current-start 0.0) source-pulse end-pulses
	(total-pulses (pulse-number-from-menu pulse-list (element-name source)))
	pulse-number-1)
    (loop for pulse-number from 1 to total-pulses
	  do (setq pulse-number-1 (pulse-number-1 pulse-list pulse-number))
	  when (setq source-pulse
		     (multiple-value-bind (source-pulse-temp end-pulses-temp)
			 (single-PULSE-menu pulse-number source
					    (or (nth 0 pulse-number-1) current-start)
					    (or (nth 1 pulse-number-1) 0.0)
					    (or (nth 2 pulse-number-1) default-mag)
					    total-pulses)
		       (setq end-pulses end-pulses-temp)
		       (when source-pulse-temp
			 (setq current-start (nth 1 source-pulse-temp))
			 (when (and (= (nth 0 source-pulse-temp) 0.0) specified-start-value)
			   (setq *vclamp-default-magnitude* (nth 2 source-pulse-temp))))
		       source-pulse-temp))
	  collect source-pulse into output
	  when end-pulses do (return output) finally (return output))))

(defun enable-pulse-train (source)
  "Enables pulse train generation by SOURCE."
  (element-parameter source 'enable-pulse-train t))

(defun disable-pulse-train (source)
  "Disables pulse train generation by SOURCE."
  (element-parameter source 'enable-pulse-train nil))

(defun enable-individual-pulses (source)
  "Enables individual pulse generation by SOURCE."
  (element-parameter source 'enable-individual-pulses t))

(defun disable-individual-pulses (source)
  "Disables individual pulse generation by SOURCE."
  (element-parameter source 'enable-individual-pulses nil))

(defun edit-pulse-spec (source)
  ;; Extracts source-list for the SOURCE from the :PARAMETERS slot. Prompts user to change number of
  ;; pulses, and calls single-PULSE-menu to edit the parameters of each pulse in turn.
  (multiple-value-bind (pulse-list pulse-train-args waveform-spec user-pwl-list)
      (EXTRACT-SOURCE-LISTs source)
    (declare (ignore waveform-spec user-pwl-list))
    (let ((default-mag (typecase source (isource 0.0) (vsource *vclamp-default-magnitude*)))
	  (dummy1 (element-parameter source 'enable-pulse-train))
	  (dummy2 (element-parameter source 'enable-individual-pulses))
	  (dummy3 (element-parameter source 'enable-pulse-train))
	  (dummy4 (element-parameter source 'enable-individual-pulses))
	  (dummy10 (element-parameter source 'set-default-to-start-of-pulses)))
      (choose-variable-values
       `((dummy1 "Edit pulse train" :boolean)
	 (dummy3 "Enable pulse train" :boolean)
	 (dummy2 "Edit set of individual pulses" :boolean)
	 (dummy4 "Enable individual pulses" :boolean)
	 (dummy10 ,(format nil "Set initial source value~%to 0ms pulse value") :boolean))
       :label (format nil "Pulse menu for Source ~A" (element-name source))
       :text "Pulse train and individual pulses are added together.")
      (element-parameter source 'set-default-to-start-of-pulses dummy10)
      (element-parameter source 'enable-pulse-train dummy3)
      (element-parameter source 'enable-individual-pulses dummy4)
      (cond-every
       (dummy1 (setq pulse-train-args (edit-pulse-train-args pulse-train-args source)))
       (dummy2 (pulse-list source (source-pulse-list-menu pulse-list dummy10 default-mag source)))))))


(defun pulse-train-from-args (pulse-train-args)
  (when pulse-train-args
    (let ((start (cdr-assoc :start pulse-train-args))
	  (stop (cdr-assoc :stop pulse-train-args))
	  (delay (cdr-assoc :delay pulse-train-args))
	  (duration (cdr-assoc :duration pulse-train-args))
	  (period (cdr-assoc :period pulse-train-args))
	  (amplitude (cdr-assoc :amplitude pulse-train-args)))
      (loop for time from start to stop by (max period 0.1) 
	    collect (list (+ time delay) (+ time delay duration) amplitude)))))

;;; SINGLE-PULSE-MENU Generates a menu that prompts for the current/voltage pulse parameters. Returns
;;; a list '(start-time stop-time pulse-amplitude).
(defun single-PULSE-menu (pulse-number source start stop amplitude total-pulses)
  (let ((source (element source))
	(dummy1 (float start))
	(dummy2 (float stop))
	(dummy3 (float amplitude))
	dummy4)
    (choose-variable-values
     `((dummy1 "Pulse start time [ms]" :number :precision)
       (dummy2 "Pulse stop time [ms]" :number :precision)
       (dummy3 ,(format nil "Pulse level ~A" (typecase source (isource "[nA]") (vsource "[mV]"))) :number :precision)
       ,(unless (= total-pulses pulse-number) `(dummy4 "Cancel rest of pulses" :boolean)))
     :text (format nil "Clamp pulse ~d (out of ~A, stop time ~a ms)" pulse-number total-pulses *user-stop-time*)
     :label (format nil "Pulse for ~A" (element-name source)))
    (values (list dummy1 dummy2 dummy3) dummy4)))


;;; SETUP-SOURCE-VALUES Loads the source with either a PWL-LIST for pulses, or a waveform array. The
;;; initial call to EXTRACT-SOURCE-LIST also may set/reset the :USE-PULSE-LIST slot if the source
;;; has no waveform or pulse information in its :PARAMETERS slot. Depending on the resulting value
;;; of :USE-PULSE-LIST, ADD-WAVEFORM is called on the extracted SOURCE-LIST, otherwise a list of
;;; pulses is converted into PWL-LIST. Each pulse is approximated in a piece-wise linear fashion
;;; such that the slope of each transition is determined by *PWL-ISOURCE-DI-DT* or
;;; *PWL-VSOURCE-DV-DT*, as appropriate. The piece-wise-linear list PWL-LIST contains the
;;; breakpoints of the pwl approximation, as follows:

;;;    `((bp-time1  bp-time2  bp-time3  ... bp-timeN)
;;;      (bp-value1 bp-value2 bp-value3 ... bp-valueN)

;;;  This is a 2D list ([2,N], where N is the number of breakpoints), with time points in the first
;;;  sublist and source values in the second sublist.
(defun setup-source-values (source)
  (multiple-value-bind (pulse-list pulse-train-args waveform-spec user-pwl-list)         
      (EXTRACT-SOURCE-LISTs source)
    (when (or pulse-list pulse-train-args waveform-spec user-pwl-list)
      (let ((use-pulse-spec (and (or pulse-list pulse-train-args user-pwl-list)
				 (or (source-use-pulse-list source) ; (not waveform-spec)
				     )))
	    (enable-pulse-train (element-parameter source 'enable-pulse-train))
	    (enable-individual-pulses (element-parameter source 'enable-individual-pulses)))
	(typecase source
	  (isource (setf (isource-use-pulse-list source) use-pulse-spec))
	  (vsource (setf (vsource-use-pulse-list source) use-pulse-spec)))
	(if use-pulse-spec
	    (if user-pwl-list
		(set-pwl-list source user-pwl-list)
		(convert-and-add-pulse-list-to-PWL-LIST
		 source
		 (sort (concatenate 'list
				    (when enable-individual-pulses pulse-list)
				    (when enable-pulse-train (pulse-train-from-args pulse-train-args)))
		       '< :key 'car)))
	    (let ((*automatic-run* t))	; Kill menus.
	      (add-waveform source :waveform-spec waveform-spec :use-menu nil)))))))

;; CONVERT-AND-ADD-PULSE-LIST-TO-PWL-LIST convert a list of pulses (start stop mag) into a list of
;; breakpoints and amplitudes, taking into account limits on transition slopes.
;;
;; Pulse-list format is - '((10.0 20.0 2.0) (100.0 110.0 -5.0) ... )
(defun convert-and-add-pulse-list-to-PWL-LIST (source &optional (pulse-list (pulse-list source)))
  (let* ((default-mag (typecase source (isource 0.0) (vsource *vclamp-default-magnitude*)))
	 (max-source-slope (or (element-parameter source 'PULSE-TRANSITION-SLOPE)
			       (typecase source (isource *pwl-isource-di-dt*) (vsource *pwl-vsource-dv-dt*))))
	 (transition-time (or (element-parameter source 'PULSE-TRANSITION-time) 0.1))
	 (minimum-source-transition-time (or (element-parameter source 'minimum-source-transition-time)
					     *minimum-source-transition-time*
					     0.0))
	 (pulse-transition (or (element-parameter source 'pulse-transition) :fixed-slope))
	 (last-mag default-mag)
	 (last-stop-time 0.0)
	 start-times stop-times amplitudes temp-results)

    ;; Clean up the pulse list a bit by deleting pulses with duplicate start times and ordering the
    ;; pulses wrt time. Separate the pulse components into start times, stop times, and mags.

    (loop for pulse in (sort (delete-duplicates pulse-list :test '= :key 'car) #'> :key 'car) do
	  (push (s-flt (nth 0 pulse)) start-times)
	  (push (s-flt (nth 1 pulse)) stop-times)
	  (push (s-flt (nth 2 pulse)) amplitudes))

    (do ((starts start-times (cdr starts))
	 (stops stop-times (cdr stops))
	 (mags amplitudes (cdr mags)))
	((null mags))
      (let* ((this-start-time (d-flt (car starts)))
	     (next-start-time (cadr starts))
	     (this-stop-time (d-flt (car stops)))
	     (this-mag (car mags))
	     (next-mag (cadr mags))
	     (default-mag (if (and (= 0 this-start-time) (element-parameter source 'set-default-to-start-of-pulses))
			    (setq last-mag this-mag)
			    default-mag))
	     (this-start-transition-time (min minimum-source-transition-time
					      (case pulse-transition
						(:fixed-slope (abs (/ (- this-mag last-mag) max-source-slope)))
						(:fixed-transition-time transition-time))))
	     (stop-trans-to-default (min minimum-source-transition-time
					 (case pulse-transition
					   (:fixed-slope (abs (/ (- this-mag default-mag) max-source-slope)))
					   (:fixed-transition-time transition-time))))
	     (stop-trans-to-spec-mag (when next-mag
				       (min minimum-source-transition-time
					    (case pulse-transition
					      (:fixed-slope (abs (/ (- this-mag next-mag) max-source-slope)))
					      (:fixed-transition-time transition-time)))))
	     (inter-pulse-time (- (or next-start-time *user-stop-time*) this-stop-time)))
					;	(format t "stop-trans-to-default ~a~%" stop-trans-to-default)
	(when (and (< last-stop-time this-stop-time) (> this-stop-time this-start-time))
	  (push (list this-start-time last-mag) temp-results) ; (format t "temp-results ~A~%" temp-results)
	  (push (list (+ this-start-time this-start-transition-time) this-mag) temp-results) ; 		 (format t "temp-results ~A~%" temp-results)
	  (cond ((and inter-pulse-time (> inter-pulse-time stop-trans-to-default))
		 (push (list (+ this-stop-time stop-trans-to-default) default-mag) temp-results)
		 ; (format t "inter (+ this-stop-time stop-trans-to-default)  ~A, temp-results ~A~%" (+ this-stop-time stop-trans-to-default) temp-results)
		 )
		((and stop-trans-to-spec-mag (< inter-pulse-time (min stop-trans-to-default stop-trans-to-spec-mag)))
		 ;; (setq starts (cons this-start-time (cons (+ this-stop-time stop-trans-to-spec-mag) (cddr starts)))) 
		 (setq this-stop-time next-start-time)
		 (push (list (+ this-stop-time stop-trans-to-spec-mag) next-mag) temp-results))
		((and stop-trans-to-spec-mag (<= stop-trans-to-default stop-trans-to-spec-mag))
		 (push (list (+ this-stop-time stop-trans-to-default) default-mag) temp-results)))
	  (push (list this-stop-time this-mag) temp-results) ; (format t "temp-results ~A~%" temp-results)
	  (setq last-stop-time this-stop-time))))

    (loop for time-mag in
	  (sort (delete-duplicates (reverse (push (list 0.0 default-mag) temp-results)) :test '= :key 'car :from-end t)	#'< :key 'car)
	  collect (car time-mag) into times
	  collect (cadr time-mag) into mags
	  finally
	  (let ((pwl-list (list times mags)))
	    (typecase source
	      (isource (setf (isource-pwl-list source) pwl-list))
	      (vsource (setf (vsource-pwl-list source) pwl-list)))
	    (return pwl-list)))))

(defun are-there-sources ()
  (true-p (or *isource* *vsource*)))

(defun set-sources () (when *advance-sources* (set-isources)))

(defun source-stimulus-p (source)
  (true-p
   (if (typecase (element source)
	 (isource (isource-use-pulse-list (element source)))
	 (vsource (vsource-use-pulse-list (element source))))
       (or (and (element-parameter source 'enable-individual-pulses) (extract-pulse-list source))
	   (and (element-parameter source 'enable-pulse-train) (extract-pulse-train-args source)))
       (or (extract-waveform-function source)
	   (source-waveform-array source)))))

(defun setup-sources ()
  (let* ((active-non-ideal-vsources
	  (loop for src in (vsources)
		when (and (vsource-enabled src) (not (vsource-use-pulse-list src)))
		do (setf (vsource-waveform-time-interval-mrt src)
			 (round (/ 1.0 (* *mrt* (vsource-waveform-time-interval-inverse src)))))
		when (and (source-stimulus-p src) (not (element-parameter src 'ideal-vsource)) (vsource-enabled src))
		collect src))
	 (fixed-active-vsources
	  (loop for src in (vsources)
		when (and ; (source-stimulus-p src)
		      (element-parameter src 'ideal-vsource) (vsource-enabled src))
		collect src))
	 (active-isources
	  (loop for src in (isources)
		when (and (isource-enabled src) (not (isource-use-pulse-list src)))
		do (setf (isource-waveform-time-interval-mrt src)
			 (round (/ 1.0 (* *mrt* (isource-waveform-time-interval-inverse src))))
			 (isource-current src) 0.0)
		(when (numberp (element-parameter src :bridge-balance))
		  (element-parameter src :bridge-balance
				     (s-flt (element-parameter src :bridge-balance))))
		when (and (isource-enabled src) (source-stimulus-p src)) 
		collect src)))
    (setq *non-ideal-vsource-list* active-non-ideal-vsources
	  *fixed-vsource-list* fixed-active-vsources
	  *isource-list* active-isources))
  nil)

(defun init-sources ()
  (loop for src-list in (list *non-ideal-vsource-list* *fixed-vsource-list* *isource-list*) do
	(loop for src in src-list do (setup-source-values src))
	(fix-source-bps src-list))
  (INIT-VSOURCEs))

(defun fix-source-bps (srcs)
  (loop for src in srcs do
	(cond ((element-slot 'pwl-list src)
	       (queue-pwl-source-break-points (element-slot 'pwl-list src)
					      (if (= 0 (element-slot 'period src))
						(* 2.0 *user-stop-time*)
						(element-slot 'period src))
					      (element-slot 'delay src)))
	      ((element-parameter src 'breakpoints)
	       (queue-breakpoint-times (element-parameter src 'breakpoints))))))

;; for sources, Resulting waveform array is put into :WAVEFORM-ARRAY slot of DESTINATION.
(defun add-waveform (destination &key (WAVEFORM-time-interval *default-waveform-step*) delay waveform-spec use-menu)
  "Add a waveform to DESTINATION, which can refer to either current or voltage sources, or synapse types.
WAVEFORM-SPEC is either a sequence of numbers or a function specification (lambda list) which returns a number
sequence. If not included, or if WAVEFORM-SPEC is a function spec and USE-MENU is T, the function WAVEFORM-MENU is
called. WAVEFORM-TIME-INTERVAL [ms, default *DEFAULT-WAVEFORM-STEP*] is the time base for WAVEFORM-SPEC. DELAY, when
not NIL [default] is in milliseconds, and sets the destination :DELAY slot of current or voltage sources, or adds a
delay to the actual waveform used in synapse types.  DESTINATION may also be an electrode, in which case the actual
destination is extracted with the function ELECTRODE-SOURCE."
  (let* ((destinations (flatten-list (loop for destination in (coerce-to-list (element destination))
					   collect (if (electrode-p destination) (electrode-source destination) destination))))
	 (waveform-spec (sequence-to-list waveform-spec))
	 (waveform-spec-is-number-seq (number-sequence-p waveform-spec))
					; (use-menu (or (not waveform-spec) (and use-menu (not waveform-spec-is-number-seq))))
	 (*automatic-run* (not use-menu))
	 (waveform (when waveform-spec-is-number-seq waveform-spec))
	 (WAVEFORM-time-interval (s-flt WAVEFORM-time-interval)))
    (loop for destination in destinations do
	  (when (or (isource-p destination) (vsource-p destination) (synapse-type-p destination))
	    (unless waveform-spec-is-number-seq
	      (multiple-value-bind (wave interval new-delay new-waveform-spec)
		  (waveform-menu (or waveform-spec (element-parameter destination 'waveform-function))
				 (format nil "Waveform for ~A" (element-name destination))
				 use-menu
					; (typecase destination (synapse-type "") (isource "nA") (vsource "mV"))
				 )
		(cond-every
		 (interval (setq WAVEFORM-time-interval interval))
		 (new-delay (setq delay new-delay)))
		(setq waveform wave
		      waveform-spec new-waveform-spec)))
	    (element-parameter destination 'waveform-function (unless waveform-spec-is-number-seq waveform-spec))
	    (when (and (synapse-type-p destination) (numberp delay))
	      (setq waveform (add-delay-to-waveform waveform delay waveform-time-interval)))
	    (let ((waveform-time-interval-inverse (/ 1.0 WAVEFORM-time-interval))
		  (waveform-array (sequence-to-float-array waveform)))
	      (typecase destination
		(synapse-type
		 (element-parameter destination 'waveform-time-interval waveform-time-interval)    
		 (element-parameter destination 'waveform waveform-array)
		 (element-parameter destination 'delta-waveform (delta-wave-array waveform-array)))
		(isource
		 (setf (isource-waveform-array destination) waveform-array
		       (isource-use-pulse-list destination) nil
		       (isource-waveform-time-interval-inverse destination) waveform-time-interval-inverse))
		(vsource
		 (setf (vsource-waveform-array destination) waveform-array
		       (vsource-use-pulse-list destination) nil
		       (vsource-waveform-time-interval-inverse destination) waveform-time-interval-inverse)))
	      (when (numberp delay)
		(let ((delay (s-flt delay)))
		  (typecase destination
		    (isource (setf (isource-delay destination) delay))
		    (vsource (setf (vsource-delay destination) delay)))
		  ;; Need to adjust this for synapse types.
		  (element-parameter destination 'breakpoints (list delay)))))))))


(defun source-pwl-list (src)
  (typecase src (isource (isource-pwl-list src)) (vsource (vsource-pwl-list src))))

;; Derive source value from PWL list at TIME. Returns double-float value unless
;; RETURN-SINGLE-FLOAT is supplied. The SRC pwl-list times are dfs and values are single-floats.
(proclaim '(inline extract-pwl-value))
(defun extract-pwl-value (time src &optional pwl-list return-single-float)
  (declare (optimize (safety 0) (speed 3) (space 0)))
  (let* ((time (d-flt time))
	 (pwl-list (or pwl-list (source-pwl-list src)))
	 (value
	  (if pwl-list
	    (do ((times (nth 0 pwl-list) (cdr times))
		 (values (nth 1 pwl-list) (cdr values)))
		((or (null (cadr times)) (<= (the df (car times)) time (the df (cadr times))))
		 (let ((prior-time (car times))
		       (prior-mag (car values)))
		   (declare (double-float prior-time)
			    (single-float prior-mag))
		   (if (null (cadr times))
		     prior-mag
		     (let ((later-mag (cadr values))
			   (later-time (cadr times)))
		       (declare (double-float later-time) (single-float later-mag))
		       (if (/= later-time 0.0)
			 (if (<= prior-time 0.0)
			   prior-mag
			   (+ prior-mag ; Interpolate between breakpoints.
			      (* (- time prior-time)
				 (/ (- later-mag prior-mag) (- later-time prior-time)))))
			 later-mag))))))
	    (or (element-parameter src 'default-mag)
		(typecase src (isource 0.0d0) (vsource *vclamp-default-magnitude*))))))
    (if return-single-float (s-flt value) (d-flt value))))

(proclaim '(inline extract-pwl-value-single))
(defun extract-pwl-value-single (time src &optional pwl-list)
  (extract-pwl-value time src pwl-list t))

;;; SET-USER-PWL-LIST
;;;
;;; The optional PWL-LIST is '((time-points)(data-points))), e.g.
;;;
;;;    (list *sim-reverse-plot-time-list* (element-data *soma*))
;;
;;; If this arg is missing, tries to find a pwl-list in the SOURCE :PARAMETERS slot under the
;;; reference 'USER-PWL-LIST. 
;;;
;;; This function also sets the :USE-PULSE-LIST slot for SOURCE if a pwl-list is found.
(defun user-pwl-list (source &optional user-pwl-list)
  (let ((pwl-list (or user-pwl-list (element-parameter source 'user-pwl-list))))
    (when pwl-list
      (set-source-use-pulse-list source)
      (element-parameter source 'pwl-list pwl-list))))


;; Erases the PWL-LIST :PARAMETERS reference for SOURCE. 
(defun reset-user-pwl-list (source)
  (element-parameter source 'pwl-list nil))


;; Looks at the pwl breakpoints, and puts each time on the queue of break points so that the
;; simulation can be sure to step there.
(defun queue-pwl-source-break-points (value-array period delay)
  (dotimes (i (ceiling (/ *user-stop-time* period)))
    (typecase value-array
      (array (loop for index from 0 to (1- (nth 1 (array-dimensions value-array))) do
		   (queue-breakpoint-time (s-flt (+ (aref value-array 0 index) (* i period) delay)))))
      (cons (loop for val in (nth 0 value-array) do (queue-breakpoint-time (s-flt (+ val (* i period) delay))))))))


(defun source-type-on-node (node)
  (loop for elt in (node-elements node)
	when (isource-p elt) do (return 'isource)
	when (vsource-p elt) do (return 'vsource)))