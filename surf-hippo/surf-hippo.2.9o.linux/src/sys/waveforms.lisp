;; -*- mode: lisp; Syntax: Common-lisp; package: surf ; base: 10;  -*-
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


;;; SYS Source file: waveforms.lisp

(in-package "SURF-HIPPO")

;; Functions for creating and manipulating one and two dimensional waveforms.


(defun 1st-non-zero-elt-index (array)
  (declare (optimize (safety 0) (speed 3) (space 1))
	   (type (simple-array single-float (*)) array))
  (loop for i from 0 to (the fn (1- (length array)))
	when (not (= (aref array i) 0))
	do (return i)))

(defun last-non-zero-elt-index (array)
  (declare (optimize (safety 0) (speed 3) (space 1))
	   (type (simple-array single-float (*)) array))
  (loop for i from (the fn (1- (length array))) downto 0
	when (not (= (aref array i) 0))
	do (return i)))

(defun find-first-non-zero-element (array) (1st-non-zero-elt-index array))
(defun find-last-non-zero-element (array) (last-non-zero-elt-index array))
  
(defun float-array-support (array)
  (let ((first (1st-non-zero-elt-index array)))
    (if first (- (1+ (last-non-zero-elt-index array)) first) 0)))

(defun transfer-float-array (from-array to-array &optional (first 0) last)
  (declare (optimize (safety 0) (speed 3) (space 1))
	   (type VEC-FLT from-array to-array)
	   (fixnum first))
  (unless last (setq last (1- (length from-array))))
  (do ((i first (1+ (the fn i)))) 
      ((= (the fn i) (the fn last)))
    (setf (aref to-array i) (aref from-array i))))


;; DELTA-WAVE-ARRAY, DOUBLE-DELTA-WAVE-ARRAY Given a n-valued sequence with values
;;
;;     [x1 x2 x3 ... xn]
;;
;; returns an (n-1)-valued array with values
;;
;;     [x2-x1, x3-x2, ... x(i+1)-xi, ... xn-x(n-1)]
;;
;; with the values of the returned array single or double float, respectively.
(defun delta-wave-array (wave)
  (list-to-array (differentiate-wave wave 1)))


(defun double-delta-wave-array (wave)
  (LIST-TO-ARRAY-DOUBLE (differentiate-wave wave 1)))


(defun nth-derivative (data time-ref N)
  "Returns as values the Nth derivative of DATA, and the associated time list, derived from TIME-REF, which can be a list of single floats or a single
number, in which case it is taken as dt. Recursively calls DIFFERENTIATE-WAVE."
  (let ((time-ref (expand-time-reference time-ref (length data))))
    (case N
      (0 (values data time-ref))
      (1 (values (differentiate-wave data time-ref) (midpoints time-ref)))
      (t (nth-derivative (differentiate-wave data time-ref) (midpoints time-ref) (1- N))))))


(defun differentiate-wave (wave &optional (time-spec 1.0))
  "Given a n-valued sequence WAVE with values

 [x0 x1 x2 ... xn-1]

returns an (n-1)-valued list with values

 [(x1-x0)/TIME-SPEC, (x2-x1)/TIME-SPEC, ... (x(i)-x(i-1))/TIME-SPEC, ... (x(n-1)-x(n-2))/TIME-SPEC]

if TIME-SPEC is a number (corresponding to delta-T). Otherwise, if TIME-SPEC is a sequence, then returns

 [(x1-x0)/(T1-T0), (x2-x1)/(T1 - T2), ... (x(n-1)-x(n-2))/(T(n-1)-T(n-2)]

where Tn = (NTH n TIME-SPEC).
"
  (let ((wave (sequence-to-list wave))
	(time-spec (typecase time-spec
		     (number time-spec)
		     (t (sequence-to-list time-spec))))
	last-x
	last-t)
    (if (consp time-spec)
	(loop for value in wave
	      for time in time-spec
	      for count from 0
	      when (> count 0)
	      collect (/ (- value last-x)
			 (- time last-t))
	      into out 
	      do (setq last-x value
		       last-t time)
	      finally (return out))
	(loop for value in wave
	      for count from 0
	      when (> count 0)
	      collect (/ (- value last-x)
			 time-spec)
	      into out 
	      do (setq last-x value)
	      finally (return out)))))
	
#|

t0          t1            t2            t3             t4   ...
----------------------------------------------------------------
x0          x1            x2            x3             x4   ...

    x1-x0         x2-x1         x3-x2         x4-x3   ...

       (x2-2x1+x0)    (x3-2x2+x1)   (x4-2x3+x2)  ...


|#

(defun midpoints (wave)
  "Given a n-valued sequence WAVE with values

 [x0 x1 x2 ... xn-1]

returns an (n-1)-valued list with values

 [(x0 + x1)/2, (x1 + x2)/2, ... (xn-2 + xn-1)/2]

"
  (mapcar '+ wave (differentiate-wave wave 2.0)))


(defun element-data-window (element start stop &key
				    (dt 0.1) data-list
				    data-type type
				    (reference-time-list (current-sim-plot-time-list))
				    state-index)
  "Returns element data for the time window defined between START and STOP, in milliseconds.
Remaining arguments are as for ELEMENT-DATA-DTED. If DATA-LIST is supplied, the sampled data is
taken directly from this list, which is assumed to be on a time grid of DT."
  (atomize-list
   (loop for element in (coerce-to-list element)
	 collect
	 (loop for time from 0.0 by dt
	       for data in (or data-list (element-data-dted element dt data-type type reference-time-list state-index))
	       when (>= stop time start)
	       collect data))))

(defun expand-time-reference (reference-time-list length &optional (start-time 0.0))
  (typecase reference-time-list
    (cons reference-time-list)
    (t (list-of-nums length (s-flt start-time) reference-time-list))))
  
(defun element-spike-times (element &key (spike-threshold -20.0) (supra-threshold-duration-min 0.1)
				    (sub-threshold-time 0.5)
				    type
				    data-list start-time
				    (reference-time-list (current-sim-plot-time-list)))
  "Returns a list of detected spikes from the voltage of the soma or segment associated with ELEMENT of TYPE, according to the SPIKE-THRESHOLD [mV],
SUPRA-THRESHOLD-DURATION-MIN, SUB-THRESHOLD-TIME. The voltage trace from ELEMENT is resampled at a time step DT with reference to
REFERENCE-TIME-LIST. All times are in milliseconds, and are referenced from the time that the voltage last went above SPIKE-THRESHOLD. If DATA-LIST
is supplied, the sampled data is taken directly from this list, which is assumed to be on a time grid of DT."
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (let* ((element (element-cell-element element))
	 (data (or data-list (element-data element 'voltage type)))
	 (reference-time-list (expand-time-reference reference-time-list (length data) (or start-time 0.0)))
	 (spike-threshold (s-flt spike-threshold))
	 (sub-threshold-time (s-flt sub-threshold-time))
	 (supra-threshold-duration-min (s-flt supra-threshold-duration-min))
	 spike-now
	 (spike-times '())
	 (supra-threshold-duration-min-zerop (<= supra-threshold-duration-min 0)))
    (unless (and (cell-element-p element) data)
      (sim-error (format nil "~A has no voltage data to process!" (element-name element))))
    (loop for voltage single-float in data
	  for time single-float in reference-time-list
	  do
	  (if (< voltage spike-threshold)
	    (setq sub-threshold-time time spike-now nil)
	    (when (and (not spike-now)
		       (or (and supra-threshold-duration-min-zerop (>= voltage spike-threshold))
			   (> (- time sub-threshold-time) supra-threshold-duration-min)))
	      (push time spike-times)
	      (setq sub-threshold-time time spike-now t)))
	  finally (return (reverse spike-times)))))

(defun element-spike-heights (element &key (spike-threshold -20.0) (supra-threshold-duration-min 0.1)
				      (sub-threshold-time 0.5)
				      type
				      data-list
				      element-spike-times 
				      (reference-time-list (current-sim-plot-time-list)))
  "Finds subsequent maximum voltages [when dV/dT = 0] referenced from spikes as generated by calling the function ELEMENT-SPIKE-TIMES. Otherwise, the
spike times can be supplied by an explicit list of ELEMENT-SPIKE-TIMES. Remaining arguments are as for the function ELEMENT-SPIKE-TIMES. Returns as
values a list of the max voltages and a list of times for the maximum voltages."
  (let* ((voltages (or data-list (element-data element 'voltage type)))
	 (reference-time-list (expand-time-reference reference-time-list (length voltages)))
	 (spike-times (or ELEMENT-SPIKE-TIMES
			  (element-spike-times element :spike-threshold spike-threshold :supra-threshold-duration-min supra-threshold-duration-min
					       :sub-threshold-time sub-threshold-time
					       :type type
					       :data-list voltages 
					       :reference-time-list reference-time-list))))
    (find-maxs voltages spike-times reference-time-list :before/after :subsequent)))

(defun element-data-d2dt2 (element &key (time-ref (current-sim-plot-time-list)) data-type type state-index)
  (nth-derivative (element-data element data-type type state-index) time-ref 2))

(defun element-data-d2dt2-dted (element dt &key (time-ref (current-sim-plot-time-list)) data-type type state-index)
  (nth-derivative (element-data-dted element dt data-type type time-ref state-index) dt 2))

  
#|
(defun data-d2dt2 (data time-ref &key plot)
  (let* ((orig-dxdt (differentiate-wave data time-ref))
	 (orig-dxdt-time-ref (midpoints time-ref))
	 (orig-d2xdt2 (differentiate-wave orig-dxdt orig-dxdt-time-ref))
	 (orig-d2xdt2-time-ref (midpoints orig-dxdt-time-ref)))
    (when plot
      (plot-timed-data data nil time-ref :title "DATA-D2DT2 Orig-DATA" :x-min 0.0)
      (plot-timed-data orig-dxdt nil orig-dxdt-time-ref :x-min 0 :title "DATA-D2DT2 Orig-DXDT")
      (plot-timed-data orig-d2xdt2 nil orig-d2xdt2-time-ref :x-min 0 :title "DATA-D2DT2 Orig-D2XDT2"))
    (values orig-d2xdt2 orig-d2xdt2-time-ref)))

(defun element-data-d2dt2-dted (element dt &key (time-ref (current-sim-plot-time-list)) data-type type state-index plot)
  (let* ((data (element-data element data-type type state-index))
	 (dvdt (differentiate-wave data time-ref))
	 (dvdt-time-ref (midpoints time-ref))
	 (d2vdt2 (differentiate-wave dvdt dvdt-time-ref))
	 (d2vdt2-time-ref (midpoints dvdt-time-ref))
	 (d2vdt2-dted-and-times (convert-data-time-lists d2vdt2 d2vdt2-time-ref dt t t))
	 (d2vdt2-dted (nth 0 d2vdt2-dted-and-times))
	 (d2vdt2-dted-time-ref (nth 1 d2vdt2-dted-and-times)))
    (when plot
      (plot-timed-data data nil time-ref :title 'data :x-min 0.0)
      (plot-timed-data dvdt nil dvdt-time-ref :title 'dvdt :x-min 0.0)
      (plot-xy-data (list (list d2vdt2-time-ref d2vdt2) (list d2vdt2-dted-time-ref d2vdt2-dted)) '(ORIGINAL dted) :title 'd2vdt2 :x-min 0.0))
    (values d2vdt2-dted d2vdt2-dted-time-ref)))
|#


(defun element-spike-thresholds (element &key (spike-threshold -20.0) (supra-threshold-duration-min 0.1)
					 (sub-threshold-time 0.5)
					 type
					 data-list
					 element-spike-times
					 (minimum-d2vdt2 1.0)
					 (reference-time-list (current-sim-plot-time-list)))
  "Returns a list of the precendent threshold values [maximum d2V/dT2 greater than MINIMUM-MAX-D2VDT2, mV/ms2, default 1.0e3] referenced from spikes as
generated by calling the function ELEMENT-SPIKE-TIMES. Otherwise, the spike times can be supplied by an explicit list of ELEMENT-SPIKE-TIMES.
Remaining arguments are as for the function ELEMENT-SPIKE-TIMES. Returns as values a list of the threshold voltages and a list of times for the
thresholds."
  (let* ((voltages (or data-list (element-data element 'voltage type)))
	 (reference-time-list (expand-time-reference reference-time-list (length voltages)))
	 (spike-times (or ELEMENT-SPIKE-TIMES
			  (element-spike-times element :spike-threshold spike-threshold :supra-threshold-duration-min supra-threshold-duration-min
					       :sub-threshold-time sub-threshold-time :data-list voltages :reference-time-list reference-time-list))))
    ;; (format t "spikes ~A~%" spike-times)
    (multiple-value-bind (d2vdt2 d2vdt2-time-ref)
	 (nth-derivative voltages reference-time-list 2) ; (data-d2dt2 voltages reference-time-list)
      (let ((voltages-win (plot-timed-data voltages nil reference-time-list :title 'sampled-voltages))
	    ; (d2vdt2-plot-win (plot-timed-data d2vdt2 nil d2vdt2-time-ref :title 'thresh-d2dvdt2 :x-min 0))
	    )
	(multiple-value-bind (maxs times)
	    (find-maxs d2vdt2 spike-times d2vdt2-time-ref :before/after :previous :min-max-value minimum-d2vdt2 :window 1.0)
	  ;; (format t "find-maxs maxs ~a~%   times ~a~%" maxs times)
	  (loop for voltage in voltages
		for time in reference-time-list
		unless times do (progn	; (add-markers-with-data d2vdt2-plot-win threshold-times maxs :add-point t)
				  (add-markers-with-data voltages-win threshold-times thresholds :add-point t)
					; (format t "out thresholds ~a~%   times ~a~%"  thresholds threshold-times)
				  (return (values thresholds threshold-times)))
		when (> time (car times))
		collect voltage into thresholds and collect time into threshold-times and do (setq times (cdr times))))))))


;; In reference to each time in REFERENCE-TIMES return the preceeding or following (according to BEFORE/AFTER) maximum value (greater than or equal to
;; MIN-MAX-VALUE) during a time period of WINDOW (in ms) in VALUES. VALUES-TIME-REF must be either a list of single floats, or a single single float,
in which case it is taken as the dt.
(defun find-maxs (values reference-times values-time-ref &key (before/after :subsequent) (min-max-value 0.0) (window 2.0) (start-time 0.0))
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (let* ((values (case before/after
		   (:subsequent values)
		   (:previous (reverse values))))
	 (num-values (length values))
	 (values-time-ref (typecase values-time-ref
			    (cons values-time-ref)
			    (t (list-of-nums num-values start-time values-time-ref))))
	 (reference-times (case before/after
			    (:subsequent reference-times)
			    (:previous (reverse reference-times))))
	 (MAX-VALUE 0.0) (reference-time 0.0) max-time LOOKING REVERSE-MAX-VALUES reverse-max-value-times)
    (declare (single-float max-value min-max-value reference-time))
    (loop for value single-float in values
	  for time single-float in (case before/after
				     (:subsequent values-time-ref)
				     (:previous (reverse values-time-ref)))
	  for count fixnum from 1
	  unless (or looking reference-times) do (return (case before/after
							   (:subsequent (values (reverse reverse-max-values) (reverse reverse-max-value-times)))
							   (:previous (values reverse-max-values reverse-max-value-times))))
	  when looking do
	  ;; (format t "looking at value ~A, current max ~A time ~A~%" value max-value time)
	  (if (or (= count num-values)
		  (and (> max-value min-max-value)
		       (> (abs (- time reference-time))	window)))
	    (progn			; (format t "at time ~A, max-value ~A, max-time ~a~%" time max-value max-time)
	      (push max-value reverse-max-values)
	      (push max-time  reverse-max-value-times)
	      (setq looking nil))
	    (progn			; (format t "value ~A, max-value ~A~%" value max-value)
	      (when (and (> value max-value) (> value min-max-value))
		(setq max-value value max-time time))))
	  else when (case before/after
		      (:subsequent (> time (the sf (car reference-times))))
		      (:previous (< time (the sf (car reference-times)))))
	  do				; (format t "starting to look - ~A ~A~%" (car reference-times) value)
	  (setq reference-time (car reference-times))
	  (setq reference-times (cdr reference-times) looking t max-time reference-time max-value value)
	  finally (return (case before/after
			    (:subsequent (values (reverse reverse-max-values) (reverse reverse-max-value-times)))
			    (:previous (values reverse-max-values reverse-max-value-times)))))))
	  
(defun element-firing-frequency (element &key (spike-threshold -20.0) (supra-threshold-duration-min 0.1)
					 (sub-threshold-time 0.5) 
					 type data-list
					 (reference-time-list (current-sim-plot-time-list))
					 (start-time 0) (end-time *user-stop-time*))
  "Returns the firing frequency in Hz from spikes detected from the voltage of the soma or segment associated with ELEMENT of TYPE, between START-TIME
[ms] and END-TIME. Keyword arguments for spike detection as used by ELEMENT-SPIKE-TIMES."
  (* 1000
     (/ (loop for spike-time in 
	      (element-spike-times element 
				   :spike-threshold  spike-threshold
				   :supra-threshold-duration-min supra-threshold-duration-min
				   :sub-threshold-time sub-threshold-time
				   :type type
				   :data-list data-list
				   :reference-time-list reference-time-list)
	      when (< start-time spike-time end-time) sum 1)
	(- end-time start-time))))
	      


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Element data analysis functions based on ELEMENT-EXTREME and thus DATA-EXTREME.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun element-extreme (element &key data-type (MIN-TIME 0.0) (MAX-TIME *USER-STOP-TIME*) dt maxp
				(what :value) data-list element-type
				(time-list (current-sim-plot-time-list)))
  "For data in DATA-LIST, if supplied, otherwise from data of DATA-TYPE of ELEMENT of ELEMENT-TYPE,
call DATA-EXTREME with remaining arguments."
  (data-extreme :MIN-TIME (s-flt MIN-TIME) :MAX-TIME (s-flt MAX-TIME) :dt dt :maxp maxp :what what
		:data-list (or data-list
			       (if dt
				   (element-data-dted element dt data-type element-type time-list)
				   (element-data element data-type)))
		:time-list time-list))


(defun data-extreme (&key (MIN-TIME 0.0) (MAX-TIME *USER-STOP-TIME*) dt maxp
			  (what :value) ; Also :Slope, :1st-derivative (same as :slope), :2nd-derivative.
			  data-list
			  (time-list (current-sim-plot-time-list)))
  "Analysis of DATA-LIST, considered with respect to a time base of step DT [ms], if supplied,
otherwise from times in TIME-LIST. The maximum [respectively minimum], depending on MAXP of WHAT
[:SLOPE, 1ST-DERIVATIVE (same as :SLOPE), :2ND-DERIVATIVE, :VALUE \(default\)], within a time window
between MIN-TIME [ms] and MAX-TIME [ms]. Returns as values the extreme value and the time for which
that value was detected. If no extreme was detected, then returns as values 0.0 and MIN-TIME. Data
units are as appropriate for the type of data."
  (declare (optimize (safety 0) (speed 3) (space 1))
	   (single-float min-time max-time))
  (let ((extreme 0.0) (extreme-time 0.0) (time-n-1 0.0) (time-n-2 0.0) (data-n-1 0.0) (data-n-2 0.0)
	(time-n 0.0) first-test-iteration)
    (declare (single-float extreme extreme-time time-n-1 time-n-2 data-n-1 data-n-2 time-n))
    (loop for data-n single-float in data-list
	  for count fixnum from 0
	  do (setq time-n (the sf (if dt
				      (if (= count 0) 0.0 (+ time-n (the sf dt)))
				      (nth count time-list))))
	  when (> time-n min-time) do
	  (let ((current-result
		 (case what
		   (:2ND-DERIVATIVE
		    (unless (< count 2)
		      (/ (- (/ (- data-n data-n-1)
			       (the sf (or dt (- time-n time-n-1))))
			    (/ (- data-n-1 data-n-2)
			       (the sf (or dt (- time-n-1 time-n-2)))))
			 (- time-n time-n-2))))
		   ((:1ST-DERIVATIVE :SLOPE)
		    (unless (< count 1)
		      (/ (- data-n data-n-1)
			 (the sf (or dt (- time-n time-n-1))))))
		   (:VALUE data-n))))
	    (when (and current-result (or (not first-test-iteration)
					  (funcall (if maxp #'< #'>)
						   extreme (the sf current-result))))
	      (setq extreme (the sf current-result)
		    extreme-time (case what
				   (:2nd-derivative time-n-1)
				   ((:1st-derivative :slope) (/ (+ time-n time-n-1) 2))
				   (t time-n)))))
	  (setq first-test-iteration t)
	  (setq data-n-2 data-n-1
		data-n-1 data-n
		time-n-2 time-n-1
		time-n-1 time-n)
	  when (> time-n max-time) 
	  do (return (values (or extreme 0.0) (or extreme-time min-time)))
	  finally (return (values (or extreme 0.0) (or extreme-time min-time))))))

(defun element-amplitude (element &key data-type (MIN-TIME 0.0) (MAX-TIME *USER-STOP-TIME*) dt
				  (time-list (current-sim-plot-time-list))
				  data-list element-type negative-p base-level)
  "Returns the amplitude in units appropriate for the type of data in DATA-LIST, if supplied,
otherwise to the data of DATA-TYPE of ELEMENT of ELEMENT-TYPE. The reference level for the rise time
is given by BASE-LEVEL [assumed to be in the units corresponding to that of the data] if supplied,
otherwise the reference is taken as the minimum \(respectively maximum\) when NEGATIVE-P is NIL,
\(respectively T\). The measured event amplitude is either the maximum or minimum value thereafter,
again depending on NEGATIVE-P. Additional arguments are as for ELEMENT-EXTREME."
  (data-amplitude :MIN-TIME (s-flt MIN-TIME) :MAX-TIME (s-flt MAX-TIME) :dt dt :time-list time-list
		  :data-list
		  (or data-list
		      (if dt
			  (element-data-dted element dt data-type element-type time-list)
			  (element-data element data-type)))
		  :negative-p negative-p :base-level base-level))
  

(defun data-amplitude (&key (MIN-TIME 0.0) (MAX-TIME *USER-STOP-TIME*) dt
			    (time-list (current-sim-plot-time-list))
			    data-list negative-p base-level)
  "Same as ELEMENT-AMPLITUDE, except that DATA-LIST must be supplied." 
  (let ((base-level
	 (or base-level
	     (apply (if negative-p 'data-max 'data-min)
		    (list :data-list data-list :MIN-TIME MIN-TIME :MAX-TIME MAX-TIME :dt dt :time-list time-list))))
	(extreme
	 (apply (if negative-p 'data-min 'data-max)
		(list :data-list data-list :MIN-TIME MIN-TIME :MAX-TIME MAX-TIME :dt dt :time-list time-list))))
    (abs (- extreme base-level))))

(defun element-10-90-rise-time (element &key data-type (MIN-TIME 0.0) (MAX-TIME *USER-STOP-TIME*) dt
					(time-list (current-sim-plot-time-list))
					data-list element-type negative-p base-level)
  "Returns the time in milliseconds for the 10% to 90% rise time applied to DATA-LIST, if supplied,
otherwise to the data of DATA-TYPE of ELEMENT of ELEMENT-TYPE. Remaining arguments are as for
ELEMENT-AMPLITUDE and ELEMENT-EXTREME."
  (let* ((data-list
	  (or data-list
	      (if dt
		  (element-data-dted element dt data-type element-type time-list)
		  (element-data element data-type))))
	 (time-list (if dt (loop for time from min-time to max-time by dt collect time) time-list))
	 (base-level (or base-level
			 (apply (if negative-p 'element-max 'element-min)
				(list element :data-type data-type :data-list data-list 
				      :MIN-TIME MIN-TIME :MAX-TIME MAX-TIME :dt dt
				      :time-list time-list))))
	 (amp (element-amplitude element :data-type data-type :base-level base-level
				 :data-list data-list :negative-p negative-p
				 :MIN-TIME MIN-TIME :MAX-TIME MAX-TIME :dt dt :time-list time-list))
	 
	 (10%amp (if negative-p
		     (- base-level (* 0.1 amp))
		     (+ base-level (* 0.1 amp))))

	 (90%amp (if negative-p
		     (- base-level (* 0.9 amp))
		     (+ base-level (* 0.9 amp))))
	 10%time)
    (loop for data in data-list
	  for time in time-list 
	  when (and (<= min-time time max-time)
		    (not 10%time) (if negative-p (< data 10%amp) (> data 10%amp)))
	  do (setq 10%time time)
	  when (and (<= min-time time max-time) (if negative-p (< data 90%amp) (> data 90%amp)))
	  do (return (- time 10%time)))))

(defun element-10-90-slope (element &key data-type (MIN-TIME 0.0) (MAX-TIME *USER-STOP-TIME*) dt
				    (time-list (current-sim-plot-time-list))
				    data-list element-type negative-p base-level)
  "Returns the slope in units/ms for the 10% to 90% rise time applied to the DATA-LIST, if supplied,
otherwise to the data of DATA-TYPE of ELEMENT of ELEMENT-TYPE. Remaining arguments are as for
ELEMENT-AMPLITUDE and ELEMENT-EXTREME."
  (let* ((data-list
	  (or data-list
	      (if dt
		  (element-data-dted element dt data-type element-type time-list)
		  (element-data element data-type))))
	 (time-list (if dt (loop for time from min-time to max-time by dt collect time) time-list))
	 (base-level (or base-level
			 (apply (if negative-p 'element-max 'element-min)
				(list element :data-type data-type :data-list data-list 
				      :MIN-TIME MIN-TIME :MAX-TIME MAX-TIME :dt dt
				      :time-list time-list))))
	 (amp (element-amplitude element :data-type data-type :base-level base-level
				 :data-list data-list :negative-p negative-p
				 :MIN-TIME MIN-TIME :MAX-TIME MAX-TIME :dt dt :time-list time-list))
	 (10%amp (if negative-p
		     (- base-level (* 0.1 amp))
		     (+ base-level (* 0.1 amp))))
	 (90%amp (if negative-p
		     (- base-level (* 0.9 amp))
		     (+ base-level (* 0.9 amp))))
	 10%time)

    (loop for data in data-list
	  for time in time-list 
	  when (and (<= min-time time max-time)
		    (not 10%time) (if negative-p (< data 10%amp) (> data 10%amp)))
	  do (setq 10%time time)
	  when (and (<= min-time time max-time) (if negative-p (< data 90%amp) (> data 90%amp)))
	  do (return
	       (unless (= 0 (- time 10%time))
		 (/ (* 0.8 amp) (- time 10%time)))))))
	 

	

(defun element-max-slope (element &key data-type (MIN-TIME 0.0) (MAX-TIME *USER-STOP-TIME*) dt
				  (time-list (current-sim-plot-time-list))
				  data-list)
  "Returns the maximum slope in units/ms applied to the DATA-LIST, if supplied, otherwise to the data
of DATA-TYPE of ELEMENT of ELEMENT-TYPE. Remaining arguments are as for ELEMENT-EXTREME."
  (element-extreme element :data-type data-type :min-time min-time :max-time max-time
		   :data-list data-list :dt dt :maxp t :what :slope :time-list time-list))

(defun data-max-slope (&key (MIN-TIME 0.0) (MAX-TIME *USER-STOP-TIME*) dt
			    (time-list (current-sim-plot-time-list))
			    data-list)
  "Same as ELEMENT-MAX-SLOPE, except that DATA-LIST must be supplied." 
  (data-extreme :min-time (s-flt min-time) :max-time (s-flt max-time)
		:data-list data-list :dt dt :maxp t :what :slope :time-list time-list))

(defun element-min-slope (element &key data-type (MIN-TIME 0.0) (MAX-TIME *USER-STOP-TIME*) dt
				  (time-list (current-sim-plot-time-list))
				  data-list)
  "Returns the minimum slope in units/ms applied to the DATA-LIST, if supplied, otherwise to the data
of DATA-TYPE of ELEMENT of ELEMENT-TYPE. Remaining arguments are as for ELEMENT-EXTREME."
  (element-extreme element :data-type data-type :min-time (s-flt min-time) :max-time (s-flt max-time)
		   :data-list data-list :dt dt :maxp nil :what :slope :time-list time-list))

(defun data-min-slope (&key (MIN-TIME 0.0) (MAX-TIME *USER-STOP-TIME*) dt
			    (time-list (current-sim-plot-time-list))
			    data-list)
  "Same as ELEMENT-MIN-SLOPE, except that DATA-LIST must be supplied." 
  (data-extreme :min-time (s-flt min-time) :max-time (s-flt max-time)
		:data-list data-list :dt dt :maxp nil :what :slope :time-list time-list))

(defun element-max (element &key data-type (MIN-TIME 0.0) (MAX-TIME *USER-STOP-TIME*) dt
			    (time-list (current-sim-plot-time-list))
			    data-list)
  "Returns the maximum applied to the DATA-LIST, if supplied, otherwise to the data of DATA-TYPE of
ELEMENT of ELEMENT-TYPE. Remaining arguments are as for ELEMENT-EXTREME."
  (element-extreme element :data-type data-type :min-time (s-flt min-time) :max-time (s-flt max-time)
		   :data-list data-list :dt dt :maxp t :what :value :time-list time-list))

(defun data-max (&key (MIN-TIME 0.0) (MAX-TIME *USER-STOP-TIME*) dt
		      (time-list (current-sim-plot-time-list))
		      data-list)
  "Same as ELEMENT-MAX, except that DATA-LIST must be supplied." 
  (data-extreme :min-time (s-flt min-time) :max-time (s-flt max-time)
		:data-list data-list :dt dt :maxp t :what :value :time-list time-list))

(defun element-min (element &key data-type (MIN-TIME 0.0) (MAX-TIME *USER-STOP-TIME*) dt
			    (time-list (current-sim-plot-time-list))
			    data-list)
  "Returns the maximum applied to the DATA-LIST, if supplied, otherwise to the data of DATA-TYPE of
ELEMENT of ELEMENT-TYPE. Remaining arguments are as for ELEMENT-EXTREME."
  (element-extreme element :data-type data-type :min-time (s-flt min-time) :max-time (s-flt max-time)
		   :data-list data-list :dt dt :maxp nil :what :value :time-list time-list))

(defun data-min (&key (MIN-TIME 0.0) (MAX-TIME *USER-STOP-TIME*) dt
		      (time-list (current-sim-plot-time-list))
		      data-list)
  "Same as ELEMENT-MIN, except that DATA-LIST must be supplied." 
  (data-extreme :min-time (s-flt min-time) :max-time (s-flt max-time)
		:data-list data-list :dt dt :maxp nil :what :value :time-list time-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun element-integrated-data (element &optional data-type type)
  "According to the plot data and time points of the last simulation, returns the sum of the integrals
of the data of type DATA-TYPE of each element in ELEMENT, of element type TYPE, where the default
DATA-TYPE is given in the documentation for the ELEMENT-DATA function. ELEMENT can either be a
single element or a list of elements."
  (loop for element in (coerce-to-list element) sum
	(let ((data (element-data element data-type type)))
	  (if data
	      (INTEGRATE-X-Y data (current-sim-plot-time-list))
	      0.0))))

(defun variance-of-wave (y-sequence x-sequence)
  (let (last-x delta-x)
    (loop for y in y-sequence
	  for x in x-sequence
	  when last-x do (setq delta-x (abs (- x last-x)))
	  do (setq last-x x)
	  when delta-x sum (* y delta-x) into integral and
	  sum (* x x y delta-x) into squared-xs and
	  sum (* x y delta-x) into mean-xs
	  finally
	  (return
	    (values
	     (/ (- squared-xs (/ (square mean-xs) integral)) integral) ; variance
	     ; (/ squared-xs integral)	;
	     ; (/ (square mean-xs) integral)
	     (/ mean-xs integral)	; mean
	     integral			; integral
	     )))))
	  
(defun gaussian-wave (x-sequence mean variance)
  (loop for x in x-sequence collect (gaussian x mean variance)))

(defun normalize-wave-area (wave integral)
  (loop for val single-float in wave collect (the sf (/ val (the sf integral)))))


#|
(multiple-value-bind (xy-lists labels)
    (extract-plot-window-data)
  (let ((y-lists (loop for xy-list in xy-lists
		       collect (nth 1 xy-list))))
    (CROSS-CORRELATION y-lists :labels labels :dc-bias '(2.1 2.1)
					:delta-t 1.0 :return-result nil :normalize t
					:print-std-dev t :plot-gaussian-fit t
					:plot-result t)))
|#
(defun half-width-at-half-max (time-list y-list max-reference-time)
  (let (maximum)
    (loop for y in y-list
	  for time in time-list
	  when (and (not maximum) (>= time max-reference-time))
	  do (setq maximum y)
	  when (and maximum (<= y (/ maximum 2)))
	  do (return time))))


;; From Siebert, p.505
(defun auto-correlation-duration (time-list Rh max-reference-time)
  (let (maximum last-time)
    (loop for y in Rh
	  for time in time-list
	  when last-time
	  sum (* y (- time last-time)) into integral
	  do (setq last-time time)
	  when (and (not maximum) (>= time max-reference-time))	; Rh(0)
	  do (setq maximum y)		; (format t "max ~A time ~A~%" y time)
	  finally (return (/ integral maximum)))))
	  


	  
(defun cross-correlation (waves &key wave2s labels return-result (normalize t)
				plot-result title x-label y-label (print-std-dev t) plot-gaussian-fit
				(delta-t 1.0) (dc-bias 0.0))
  (declare (optimize (safety 0) (speed 3) (space 1))
					; (single-float delta-t)
	   )
  (let ((delta-t (s-flt delta-t)))
    (declare (single-float delta-t))
    (loop for wave in (if (consp (car waves)) waves (list waves))
	  for wave2 in (if (consp (car (or wave2s waves))) (or wave2s waves) (list (or wave2s waves)))
	  for count from 0
	  for bias =  (s-flt (case dc-bias
			       (:estimate (car wave))
			       (t (if (consp dc-bias)
				      (nth count dc-bias)
				      (or dc-bias 0.0)))))
	  collect
	  (let* ((wave (if (zerop (the sf bias))
			   (sequence-to-list wave)
			   (mapcar #'(lambda (val)
				       (declare (optimize (safety 0) (speed 3) (space 1)))
				       (- (the sf val) (the sf bias))) (sequence-to-list wave))))
				   
		 (wave-length (length (the cons wave)))
		 (reverse-wave (if wave2
				   (if (zerop (the sf bias))
				       (sequence-to-list wave2)
				       (mapcar #'(lambda (val)
						   (declare (optimize (safety 0) (speed 3) (space 1)))
						   (- (the sf val) (the sf bias))) (sequence-to-list wave2)))
				   wave))
		 (reverse-wave-length (length (the cons reverse-wave)))
		 (duration (max wave-length reverse-wave-length))
		 (x-sequence (typecase delta-t
			       (cons delta-t)
			       (number
				;(list-of-sf-nums
				 ;(* delta-t (+ wave-length reverse-wave-length))
				 ;(* delta-t (- reverse-wave-length))
				 ;delta-t)
				
				 (loop for x single-float from (* delta-t (- reverse-wave-length)) to (* delta-t wave-length)
				     by delta-t collect x)
				)))
		 (result
		  (loop for shift fixnum from (- reverse-wave-length) to wave-length collect
			(let ((left-wave (if (< shift 0) reverse-wave wave))
			      (right-wave (if (< shift 0) wave reverse-wave)))
			  (do ((left-wave left-wave (cdr left-wave))
			       (count 0 (1+ (the fn count))))
			      ((= (the fn count) (abs shift))
			       (/ (the sf
				       (loop for left-value single-float in left-wave
					     for right-value single-float in right-wave
					     sum (the sf (* left-value right-value))
					     into out single-float
					     finally (return out)))
				  duration))))))
		 (result-at-0
		  (loop for val in result
			for time in x-sequence
			when (>= (the sf time) 0) do (return val))))
	    (list result x-sequence result-at-0))
	  into results-x-seqs
	  finally
	  (let ((return-value
		 (loop for result-x-seq in results-x-seqs
		       collect (let ((x-seq (nth 1 result-x-seq))
				     (result-seq (nth 0 result-x-seq)))
				 (multiple-value-bind (variance mean integral)
				     (variance-of-wave result-seq x-seq)
				   (list (if normalize (normalize-sequence result-seq integral) result-seq)
					 x-seq mean variance
					 (half-width-at-half-max x-seq result-seq 0.0)
					 (auto-correlation-duration x-seq result-seq 0.0))))
		       into normal-results-x-seq-mean-var-half-width-auto-corr-durations
		       finally
		       (when plot-result
			 (loop for normal-results-x-seq-mean-var-half-width-auto-corr-duration
			       in normal-results-x-seq-mean-var-half-width-auto-corr-durations
			       for count from 0
			       for mean = (nth 2 normal-results-x-seq-mean-var-half-width-auto-corr-duration)
			       for variance = (nth 3 normal-results-x-seq-mean-var-half-width-auto-corr-duration)
			       for half-width = (nth 4 normal-results-x-seq-mean-var-half-width-auto-corr-duration)
			       for acrl-duration = (nth 5 normal-results-x-seq-mean-var-half-width-auto-corr-duration)
			       for orig-label = (nth count labels)
			       collect (nth 0 normal-results-x-seq-mean-var-half-width-auto-corr-duration) into out
			       collect (nth 1 normal-results-x-seq-mean-var-half-width-auto-corr-duration) into x-sequences
			       collect (format nil "~A CC" orig-label) into new-labels
			       when (and (> variance 0) plot-gaussian-fit)
			       collect (gaussian-wave (nth 1 normal-results-x-seq-mean-var-half-width-auto-corr-duration)
						      mean variance)
			       into out and 
			       collect (nth 1 normal-results-x-seq-mean-var-half-width-auto-corr-duration) into x-sequences
			       and collect (format nil "~A Gaussian" orig-label) into new-labels
			       when print-std-dev
			       collect (format nil "~A Std dev:~a HWHM:~,3f, ACdur: ~,3f" orig-label
					       (when (> variance 0) (sqrt variance) " Neg var")
					       half-width acrl-duration)
			       into std-dev-strings
			       finally
			       (let ((plot-win (plot-timed-data out new-labels x-sequences
								:x-label x-label :y-label y-label :title title)))
				 (when print-std-dev (add-comment plot-win
								  (concatenate-string-list std-dev-strings :lf-count 1))))))
		       (return (values normal-results-x-seq-mean-var-half-width-auto-corr-durations)))))
	    (when return-result (return return-value))))))
  
(defun integrate-wave (wave &optional (delta-t 1.0) (x-0 0.0))
  "Given WAVE, an array or list of numbers assumed to be spaced evenly by DELTA-T with respect to the
independent variable, returns a list which is the cumulative integral of WAVE, with the initial
conditions given by the optional argument X-0."
  (setq wave (sequence-to-list wave))
  (let ((output))
    (do ((wave wave (cdr wave))
	 (x (+ x-0 (* (car wave) delta-t)) (+ x (* (car wave) delta-t))))
	((null wave) (reverse output))
      (push x output))))

(defun clip-wave (wave min max)
  (loop for val in wave collect (min max (max val min))))


(defun list-mins (wave &optional (delta-t 1.0) (min 0.0) (min-min-time 0.0))
  "For list in WAVE, with time steps DELTA-T, returns two lists as values. These lists are the MIN
negative-going and positive-going crossing times, respectively, whenever the duration framing a
particular negative-positive pair of MIN crossings is greater than MIN-MIN-TIME."
  (let (min-reached min-end-times min-begin-times)
    (loop for val in wave
	  for time from 0 by delta-t
	  do (if min-reached
		 (when (> val min)
		   (setq min-reached nil)
		   (if (> (- time (car min-begin-times)) min-min-time)
		       (push time min-end-times)
		     (setq min-begin-times (cdr min-begin-times))))
	       (when (< val min)
		 (setq min-reached t)
		 (push time min-begin-times))))
    (list (reverse min-begin-times)(reverse min-end-times))))

(defun list-maxs (wave &optional (delta-t 1.0) (max 0.0) (min-max-time 0.0))
  "For list in WAVE, with time steps DELTA-T, returns two lists as values. These lists are the
MAX positive-going and negative-going crossing times, respectively, whenever the duration
framing a particular positive-negative pair of MAX crossings is greater than MIN-MAX-TIME."
  (let (max-reached max-end-times max-begin-times)
    (loop for val in wave
	  for time from 0 by delta-t
	  do (if max-reached
		 (when (< val max)
		   (setq max-reached nil)
		   (if (> (- time (car max-begin-times)) min-max-time)
		       (push time max-end-times)
		       (setq max-begin-times (cdr max-begin-times))))
		 (when (> val max)
		   (setq max-reached t)
		   (push time max-begin-times))))
    (list (reverse max-begin-times)(reverse max-end-times))))

(defun frame-min-maxs (wave max-min-wave &optional (delta-t 1.0) (max 0.0) (min 0.0)
			    (min-min-max-time 0.0)  messages)
  "Strip epochs in WAVE (time step of DELTA-T) according to analysis applied to MAX-MIN-WAVE. Epochs
are detected by applying LIST-MINS and LIST-MAXS to WAVE, using MAX and MIN, respectively, and
MIN-MIN-MAX-TIME as for the MIN-MIN-TIME and MIN-MAX-TIME arguments, respectively. Returns the
processed wave."
  (let* ((list-mins (LIST-MinS max-min-wave delta-t min min-min-max-time))
	 (list-maxs (LIST-MaxS max-min-wave delta-t max min-min-max-time))
	 (start-mins (car list-mins))
	 (start-maxs (car list-maxs))
	 (end-mins (cadr list-mins))
	 (end-maxs (cadr list-maxs))
	 within-neg-event
	 within-pos-event
	 output hold-value)
    (do* ((wave wave (cdr wave))
	  (time 0.0 (+ time delta-t))
	  (this-x (car wave) (car wave)))
	 ((null wave))
      (when messages
	(when within-neg-event (format t "time:~A Within neg event...~%" time))
	(when within-pos-event (format t "time:~A Within pos event...~%" time)))
      (push (cond ((and			; (not within-pos-event)
		    (member time start-mins))
		   (setq within-neg-event t) 
		   (if within-pos-event hold-value (setq hold-value this-x)))
		  ((and ; (not within-neg-event)
		    (member time start-maxs))
		   (setq within-pos-event t)
		   (if within-neg-event hold-value (setq hold-value this-x)))
		  ((member time end-mins)
		   (setq within-neg-event nil)
		   (if within-pos-event hold-value this-x))
		  ((member time end-maxs)
		   (setq within-pos-event nil)
		   (if within-neg-event hold-value this-x))
		  ((and hold-value (or within-neg-event within-pos-event))
		   hold-value)
		  (t this-x))
	    output))
    (reverse output)))

(defun upstroke-times (wave delta-t threshold)
  (let (over-threshold)
    (loop for dvdt in (differentiate-wave wave delta-t)
	  for time from 0.0 by delta-t
	  ;; do (format t "time ~A, dvdt ~A~%" time dvdt)
	  when (and (not over-threshold) (> dvdt threshold))
	  collect time into out and do (setq over-threshold t)
	  else do (unless (> dvdt threshold) (setq over-threshold nil))
	  finally (return out))))

(defun strip-spikes (wave delta-t threshold)
  (let ((upstroke-times (upstroke-times wave delta-t threshold))
	hold-value in-spike)
    (loop for value in wave
	  for time from 0.0 by delta-t
	  when (and upstroke-times (= time (car upstroke-times)))
	  do (setq hold-value value upstroke-times (cdr upstroke-times) in-spike t)
	  and collect hold-value into out
	  else when (and in-spike (> value hold-value)) collect hold-value into out
	  else do (setq in-spike nil) and collect value into out
	  finally (return out))))
		 
	


(defun find-zero-crossings (wave &optional (delta-t 1.0) (min-difference-from-0 0.0))
  "For data in WAVE with time step DELTA-T, return a list of the zero-crossing times. True zero crossings are detected when they are framed by
alternating polarity amplitudes of at least MIN-DIFFERENCE-FROM-0 [default 0.0]." 
  (let (output
	previous-ampltiude-above-minimum-p
	candidate-zero-crossing)
    (do* ((time 0.0 (+ time delta-t))
	  (time-1 0.0 time)
	  (wave wave (cdr wave))
	  (x-2 nil x-1)
	  (x-1 nil this-x)
	  (this-x (car wave) (car wave)))
	((null wave) (reverse output))
      (when x-2
	(when (and candidate-zero-crossing
		   previous-ampltiude-above-minimum-p
		   (> (abs this-x) min-difference-from-0))
	  (push candidate-zero-crossing output)
	  (setq candidate-zero-crossing nil))
	  
	(if (or (and (>= this-x x-1 x-2)
		     (> this-x 0.0 x-2))
		(and (<= this-x x-1 x-2)
		     (< this-x 0.0 x-2)))
	  (setq candidate-zero-crossing time)
	  (when (> (abs this-x) min-difference-from-0)
	    (setq previous-ampltiude-above-minimum-p t)))))
    (reverse output)))
	



  
;; ************* ************* ************* *************
;;
;;    2D Array Related Functions
;;
;; ************* ************* ************* *************

(defun nil-2d-array (array)
  (dotimes (i (array-dimension array 0))
    (dotimes (j (array-dimension array 1))
      (setf (aref array i j) nil)))
  array)


(defun add-offset-2d-array (array offset)
  (dotimes (i (array-dimension array 0))
    (dotimes (j (array-dimension array 1))
      (setf (aref array i j)
	    (+ (aref array i j) offset))))
  array)

(defun add-offset-2d-array-float (array offset)
  (declare (optimize (safety 0) (speed 3) (space 1))
	   (type single-float offset))
  (dotimes (i (array-dimension array 0))
    (dotimes (j (array-dimension array 1))
      (setf (aref array i j) (the sf (+ (the sf (aref array i j)) offset)))))
  array)

	      
(defun array-vol (array grid-side &optional other-grid-side)
  "Given 2D ARRAY, with sides GRID-SIDE X OTHER-GRID-SIDE (when optional OTHER-GRID-SIDE supplied),
GRID-SIDE X GRID-SIDE otherwise, returns volume of array."
  (* grid-side (or other-grid-side grid-side)
     (loop for i from 0 to (1- (array-dimension array 0))
	   summing (loop for j from 0 to (1- (array-dimension array 1)) summing (aref array i j)))))

(defun array-min (array)
  (loop for i from 0 to (1- (array-dimension array 0))
	minimizing (loop for j from 0 to (1- (array-dimension array 1)) minimizing (aref array i j))))

(defun array-max (array)
  (loop for i from 0 to (1- (array-dimension array 0))
	maximizing (loop for j from 0 to (1- (array-dimension array 1)) maximizing (aref array i j))))


;; ************* ************* ************* *************
;;
;;    Interpolation (Eg For Synapse Evaluation) Array Related Functions
;;
;; ************* ************* ************* *************



;; INTERPOLATED-ARRAY-VALUE Note that the units of the INTEGER-TIME and FRACTIONAL-TIME arguments
;; are assumed to be consistent with the time base of the WAVEFORM array, i.e. the index of the
;; WAVEFORM array is in units of INTEGER-TIME.
(proclaim '(inline interpolated-array-value))
(defun interpolated-array-value (waveform integer-time fractional-time)
  (declare (optimize (safety 0) (speed 3) (space 1))
	   (type (simple-array single-float (*)) waveform)
	   (fixnum integer-time)
	   (single-float fractional-time))
  (if (= 0.0 fractional-time)
      (aref waveform integer-time)
      (+ (aref waveform integer-time)
	 (* fractional-time
	    (- (aref waveform (1+ integer-time))
	       (aref waveform integer-time))))))

(defun interpolated-array-value (waveform integer-time fractional-time)
  (declare (optimize (safety 0) (speed 3) (space 1))
	   (type (simple-array single-float (*)) waveform)
	   (fixnum integer-time)
	   (single-float fractional-time))
  (if (= 0.0 fractional-time)
      (aref waveform integer-time)
      (+ (aref waveform integer-time)
	 (* fractional-time
	    (- (aref waveform (1+ integer-time))
	       (aref waveform integer-time))))))
					 

(defun interpolated-array-value-double (waveform integer-time fractional-time)
  (declare (optimize (safety 0) (speed 3) (space 1))
	   (type (simple-array single-float (*)) waveform)
	   (fixnum integer-time)
	   (double-float fractional-time))
  (if (= 0.0d0 fractional-time)
      (coerce-to-double (aref waveform integer-time))
      (+ (aref waveform integer-time)
	 (* fractional-time
	    (- (aref waveform (1+ integer-time))
	       (aref waveform integer-time))))))

(proclaim '(inline interpolated-array-value-with-delta-array))
(defun interpolated-array-value-with-delta-array (waveform delta-waveform integer-time fractional-time)
  (declare (optimize (safety 0) (speed 3) (space 1))
	   (type (simple-array single-float (*)) waveform delta-waveform)
	   (fixnum integer-time)
	   (single-float fractional-time))
  (if (= 0.0 fractional-time)
      (aref waveform integer-time)
      (+ (aref waveform integer-time) (* fractional-time (aref delta-waveform integer-time)))))
				
(proclaim '(inline double-interpolated-array-value-with-delta-array))
(defun double-interpolated-array-value-with-delta-array (waveform delta-waveform integer-time fractional-time)
  (declare (optimize (safety 0) (speed 3) (space 1))
	   (type 1d-df waveform delta-waveform)
	   (fixnum integer-time)
	   (double-float fractional-time))
  (if (= 0.0d0 fractional-time)
      (aref waveform integer-time)
      (+ (aref waveform integer-time)
	 (* fractional-time (aref delta-waveform integer-time)))))
				

(defun interpolated-value (value1 value2 fractional-time)
  (declare (optimize (safety 0) (speed 3) (space 1))
	   (single-float fractional-time value1 value2))
  (+ value1 (* fractional-time (- value2 value1))))


(defun interpolated-value-double (value1 value2 fractional-time)
  (declare (optimize (safety 0) (speed 3) (space 1))
	   (single-float value1 value2)
	   (double-float fractional-time))
  (+ value1 (* fractional-time (- value2 value1))))


(proclaim '(inline generic-interpolated-array-value))
(defun generic-interpolated-array-value (array index)
  (declare (optimize (safety 0) (speed 3) (space 0) (compilation-speed 0)))
  (typecase index
    (fixnum (aref (the vec-flt array) index))
    (float (multiple-value-bind (integer-part fractional-part)
	       (kernel::sf-sb32-truncate (the sf index))
	     ;; (truncate (the sf index))
	     (interpolated-array-value (the vec-flt array) integer-part fractional-part)))
    (t 0.0)))


(defvar *interpolate-light-synapse-conductance-waveforms* t)

(proclaim '(inline interpolated-array-slice-value))
(defun interpolated-array-slice-value (waveform-array waveform-length slice-index integer-time fractional-time
						      fractional-time=0-OR-do-not-interpolate)
  (declare (optimize (safety 0) (speed 3) (space 0)(debug 0))
	   (type 2dfloat waveform-array)
	   (fixnum integer-time slice-index waveform-length)
	   (single-float fractional-time))
  (let ((1+integer-time (1+ integer-time)))
    (declare (fixnum 1+integer-time))
    (if (or fractional-time=0-OR-do-not-interpolate (>= 1+integer-time waveform-length))
	(aref waveform-array slice-index integer-time)
	(+ (aref waveform-array slice-index integer-time)
	   (* fractional-time
	      (- (aref waveform-array slice-index 1+integer-time) (aref waveform-array slice-index integer-time)))))))



;; ************* ************* ************* *************
;;
;;   Array Information Functions
;;
;; ************* ************* ************* *************

(defun wave-name-from-type (type)
  (case type
    (impulse-FUNCTION "Impulse Function")
    (linear-impulse-FUNCTION "Linear Stage Impulse Function")
    (waveform-function "Conductance Reference Waveform")
    (static-voltage-dependence-function "Static Voltage-dependence")
    (t (format nil "~A" type))))


(defun 2d-spatial-rf-info (function-list)
  (format t " Spatial RF Function: ~a" (car function-list))
  (when (car function-list)
    (format t " - ARGS:")
    (loop for arg in (cdr function-list)
	  do (format t " ~A " arg))
    (format t "~%")))

#|
(defun 1d-impulse-info (impulse-function-list)
  (format t "Impulse Function ~a" (car impulse-function-list))
  (case (car impulse-function-list)
    ('double-alpha
     (format t " [tau1: ~amsec, tau2: ~amsec, Proportion: ~a]~%"
	     (nth 0 (cdr impulse-function-list))
	     (nth 1 (cdr impulse-function-list))
	     (nth 2 (cdr impulse-function-list))))

    ((alpha-impulse alpha-array)
     (format t " [Duration: ~Amsec, tau: ~amsec, tau power: ~a]~%"
	     (nth 0 (cdr impulse-function-list))
	     (nth 1 (cdr impulse-function-list))
	     (nth 2 (cdr impulse-function-list))))))
|#

(defun 1d-impulse-info (impulse-function-list)
  (format t "Impulse Function ")
  (document-function-args impulse-function-list))
  
  

(defun print-waveform-from-waveargs (waveargs &optional (indent 0))
  (when waveargs
    (let* ((dummy1 (car waveargs))
	   (waveargs (cdr waveargs))
	   (dummy2 (or (cdr-assoc 'step waveargs) 0.2))
	   (dummy3 (or (cdr-assoc 'tau waveargs) 10.0))
	   (dummy4 (or (cdr-assoc 'duration waveargs) 1.0))
	   (dummy10 (or (cdr-assoc 'delay waveargs) 0.0)))
      (print-spaces t indent)
      (format t "~A Waveform - ~%" dummy1)
;      (format t "Time step ~ams, Delay for start of waveform ~ams, Duration ~ams~%"  dummy2 dummy10 dummy4)
      (print-spaces t indent)
      (loop for arg in waveargs
	    for arg-count from 1
	    do (format t "~A: ~A" (car arg) (cdr arg))
	    when (< arg-count (length waveargs)) do (format t ", ")))))



;; Misc functions for various waveforms

(defun add-delay-to-waveform (waveform delay &optional (waveform-time-step 1.0) (delay-value 0.0))
  "Adds a series of numbers, given by DELAY-VALUE [default 0.0] to the head of the sequence WAVEFORM.
The length of this series is given by DELAY divided by WAVEFORM-TIME-STEP [default 1.0].  The
returned sequence is of the same type [cons or array] as the original WAVEFORM."
  (let* ((waveform-arrayp (arrayp waveform))
	 (new-seq (nconc (list-of-nums (/ delay waveform-time-step) delay-value 0.0)
			 (sequence-to-list waveform))))
    (if waveform-arrayp (list-to-array new-seq) new-seq)))

(defun extract-waveform-function-interval (funspec &optional function-args)
  (let ((parsed-args (extract-funspec-args funspec function-args t)))
    (or (cadr (or (find 'step parsed-args :key 'car)
		  (find 'interval parsed-args :key 'car)
		  (find 'grid-size parsed-args :key 'car)))
	; *default-waveform-step*
	)))

(defun extract-waveform-function-delay (funspec &optional function-args)
  (let ((parsed-args (extract-funspec-args funspec function-args t)))
    (and nil
	 (or (cadr (or (find 'delay parsed-args :key 'car)
		       (find 'start parsed-args :key 'car)))
					; 0.0
	     ))))
  
(defun waveform-menu (&optional funspec comment (use-menus t))
  (let* ((*automatic-run* (or *automatic-run* (not use-menus)))
	 (function-name (if (consp funspec) (car funspec) funspec))
	 new-function-name 
	 (dummy1 (when funspec
		   (read-from-string (format nil ":~A" (if (consp funspec) (car funspec) funspec)))))
	 (basics `(:exponential-array :double-exponential-array :impulse :alpha-array :double-alpha :sinewave))
	 (candidates (if funspec (delete-duplicates (cons dummy1 basics)) basics)))
    (choose-variable-values `((dummy1 "" :choose ,candidates :vertical)) :label "Choose Waveform Type"
			    :text comment)
    (setq new-function-name (read-from-string (format nil "~A" dummy1)))
    (unless (eq new-function-name function-name)
      (setq funspec new-function-name
	    function-name new-function-name))
    (setq funspec (when function-name (cons function-name (edit-function-args funspec nil :extra-text comment))))
    (values (when (extract-function function-name) (apply (car funspec) (cdr funspec)))	; Function array
	    (extract-waveform-function-interval funspec)
	    (extract-waveform-function-delay funspec)
	    funspec)))



;; ************* ************* ************* *************
;;
;;   Some Specific Waveforms
;;
;; ************* ************* ************* *************


(defun sinewave (&optional (amplitude 1.0) (duration *user-stop-time*) (frequency 1.0)
			   &key (phase 0.0) (offset 0.0) (step 0.2) (start 0.0) zero-before-start)
  "FREQUENCY is in cycles per unit time, as given by STEP [default 0.2]. PHASE is in degrees. Returns
a single-float array. Function times less than START [default 0.0] return 0.0 when ZERO-BEFORE-START
is T, otherwise OFFSET. Time argument given to the sin function is relative to START:

            sin [{2pi * FREQUENCY * (TIME - START)} + PHASE]

"
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (let ((start (s-flt start))
	(amplitude (s-flt amplitude))
	(frequency (s-flt frequency))
	(duration (s-flt duration))
	(step (s-flt step))
	(offset (s-flt offset))
	(phase (deg-to-rad (s-flt phase))))
    (list-to-array
     (loop for time single-float from 0.0 to (the sf duration) by (the sf step)
	   collect (+ offset
		      (if (< time (the sf start))
			  (if zero-before-start (- offset) 0.0)
			  (* amplitude (sin (+ (the sf phase) (* 2 pi-single (- time start) frequency))))))))))

(defun sin-array (amplitude duration frequency &key (phase 0.0) (offset 0.0) (step 0.2) (start 0.0) zero-before-start) ;
  (sinewave amplitude duration frequency :phase phase :offset offset :step step :start start
	    :zero-before-start zero-before-start))

  
;;; SIGMOID-ARRAY Array length is given by (VMAX - VMIN) / VRES.
(defun sigmoid-array (v-half slope vmin vmax vres)
  (list-to-array
   (loop for voltage from vmin to vmax by vres
	 collecting (/ 1.0 (+ 1.0 (exp-w-limits (max -50.0 (min 50.0 (* -1 slope (- voltage v-half))))))))))
				 
(defun impulse-array (&optional (amplitude 1.0) (duration 1) (delay 0) (step 1.0))
  (let ((array (make-array (round (/ duration step)) :element-type 'single-float)))
    (setf (aref array (round (/ delay step))) (s-flt amplitude))
    array))

(defun impulse (&optional (amplitude 1.0) (length 1) (delay 0) (step 1.0))
  (impulse-array (s-flt amplitude) (s-flt length) (s-flt delay) (s-flt step)))


(defun pulse (delay pulse-duration amplitude total-duration step)
  (let ((amplitude (s-flt amplitude)))
    (loop for time from 0.0 by step
	  until (> time total-duration)
	  collect (cond
		    ((< time delay) 0.0)
		    ((< time (+ delay pulse-duration)) amplitude)
		    (t 0.0)))))


(defvar *wave-cutoff* (exp -6) "Relative max cut off value for various waveform creation functions, e.g. ALPHA-ARRAY.")
(defvar *alpha-cutoff-threshold-wrt-max* *wave-cutoff*)

(defun exponential-array-unit-area (&optional (tau 1.0) (step 1.0) length)
  "Returns an array filled with a decaying exponential, whose amplitude is adjusted so that its area
is 1.0.  and time base increment given by STEP [default 1.0], and OFFSET [0.0]. The length of array
is given by LENGTH, if positive, otherwise when the amplitude without the OFFSET is less than
*WAVE-CUTOFF* times the AMPLITUDE. Values before the START [default 0.0] of waveform are given by
OFFSET. Function time"
  (let* ((original-exponential (exponential-array tau step length))
	 (area (* 1 ; step
		  (loop for x single-float across original-exponential sum x))))
    (loop for index from 0 to (1- (length original-exponential)) do
	  (setf (aref original-exponential index)
		(/ (aref original-exponential index) area)))
    original-exponential))


  
(defun exponential-array (&optional (tau 1.0) (step 1.0) (length 0) (offset 0.0) (amplitude 1.0) (start 0.0))
  "Returns an array filled with a decaying exponential, of AMPLITUDE [default 1.0] and time base
increment given by STEP [default 1.0], and OFFSET [0.0]. The length of array is given by LENGTH, if
positive, otherwise when the amplitude without the OFFSET is less than *WAVE-CUTOFF* times the
AMPLITUDE. Values before the START [default 0.0] of waveform are given by OFFSET. Function time
argument is referenced from START [default 0.0]."
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (let* ((step (s-flt step))
	 (start (s-flt start))
	 (tau (s-flt tau))
	 (amplitude (s-flt amplitude))
	 (threshold (s-flt (* (abs amplitude) *wave-cutoff*)))
	 (value 0.0)
	 (length (and length
 		      (> length 0)
		      (round length))))
    (declare (single-float value threshold))
    (without-floating-underflow-traps
     (list-to-array 
      (loop for time single-float from 0.0 by step
	    for count fixnum from 1
	    do (setq value (if (< time start) 0.0 (exp-w-limits (* -1.0 (/ (- time start) tau)))))
	    collect (* amplitude value) into result
	    when (if length (= count (the fn length))
		   (and (< (abs value) threshold)
			(> time tau)
			(> time start)))
	    do (return (ADD-VAL-TO-FLOAT-LIST (s-flt offset) result)))))))



(defun double-exponential-array (&optional (tau-rise 1.0) (tau-fall 1.0)
					   &key (amplitude 1.0) normalize (step 1.0) (length 0) (offset 0.0)
					   (start 0.0))
  "Returns an array with the difference of two decaying exponentials:

          AMPLITUDE * [Exp(-t/TAU-FALL) - Exp(-t/TAU-RISE)] + OFFSET

AMPLITUDE has a default value of 1.0. Array time base increment is given by STEP [default 1.0].
OFFSET has a default value of 0.0. If NORMALIZE is non-NIL, then the waveform is adjusted and the
peak given by (AMPLITUDE + OFFSET). The length of array is given by LENGTH, if positive [default 0],
otherwise when the larger of the two exponential terms is less than *WAVE-CUTOFF*. Function time
argument is referenced from START [default 0.0]."
;  (declare (optimize (safety 0) (speed 3) (space 1)))
  (let* ((step (s-flt step))
	 (start (s-flt start))
	 (tau-rise (s-flt tau-rise))
	 (tau-fall (s-flt tau-fall))
	 (offset (s-flt offset))
	 (amplitude (s-flt amplitude))
	 (threshold (s-flt *wave-cutoff*))
	 (falling-term 0.0)
	 (rising-term 0.0)
	 (difference-term 0.0)
	 (length (and length
		      (> length 0)
		      (round length))))
    (declare (single-float difference-term threshold rising-term falling-term))
    (multiple-value-bind (list max)
	(loop for time single-float from 0.0 by step
	      for count fixnum from 1
	      do
	      (when (>= time start)
		(setq falling-term (exp-w-limits (* -1.0 (/ (- time start) tau-fall)))
		      rising-term (exp-w-limits (* -1.0 (/ (- time start) tau-rise)))))
	      (setq difference-term
		    (if (< time start) 0.0
		      (* amplitude (- falling-term rising-term))))
	      maximize (abs difference-term) into max single-float
	      collect difference-term into list
	      when (if length
		       (= count (the fn length))
		       (and (< (max rising-term falling-term) threshold)
			    (> time tau-rise)
			    (> time start)))
	      do (return (values list max)))
      (declare (single-float max))
      (list-to-array
       (if (= max 0) list
	 (ADD-VAL-TO-FLOAT-LIST (s-flt offset)
				(if normalize (SCALE-FLOAT-LIST (abs (/ amplitude max)) list) list)))))))

(defun foo ()
  (loop for x single-float from 1.0 to 10.0 maximize x into max  single-float
	))

(defun double-exponential (&optional (tau-rise 1.0) (tau-fall 1.0)
				     &key (amplitude 1.0) normalize (step 1.0) (length 0) (offset 0.0)
				     (start 0.0))
  (double-exponential-array tau-rise tau-fall
			    :amplitude amplitude :normalize normalize
			    :step step :length length :offset offset :start start))
	 
(defun alpha-integral (tau time-exponent)
  (* (factorial (round time-exponent))
     (expt tau (1+ time-exponent))))
  
  
(defun alpha-max-value (tau time-exponent)
  (/ (expt (* tau time-exponent) time-exponent)
     (exp-w-limits time-exponent)))

(defun alpha-array (&optional
		    (tau 1.0)
		    &key
		    (TIME-EXPONENT 1)
		    (adjustment :NORMALIZE) (step 1.0) (duration 0.0) (offset 0.0) (amplitude 1.0) (delay 0.0))
  "Returns an array of an alpha function (K * time^A * e^(-time/tau)) with time constant TAU [ms],
starting at time = DELAY (value prior to DELAY is OFFSET [default 0.0]).
The exponent for the leading time coefficient, A, is given by TIME-EXPONENT [default 1]
ADJUSTMENT [default :NORMALIZE] determines the value of K as follows:

 :NORMALIZE  -  K set so that function amplitude is given by AMPLITUDE
 :UNIT-AREA  -  K set so that function area is given by AMPLITUDE
 ELSE         -  K = 1

STEP [ms, default 1.0] gives the time step of the array. The array length is given by DURATION [ms,
default 0.0] if positive, otherwise the length is set when the function value is less than
*WAVE-CUTOFF* times the maximum. OFFSET adds an offset to the returned array, after the above
constraints have been met."
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (without-floating-underflow-traps
   (let* ((tau (s-flt tau))
	  (offset (s-flt offset))
	  (duration (s-flt duration))
	  (delay (s-flt delay))
	  (amplitude (s-flt amplitude))
	  (TIME-EXPONENT (s-flt TIME-EXPONENT))
	  (step (s-flt step))
	  (adjustment-numeric-value (case adjustment
				      (:normalize (the sf (/ 1 (alpha-max-value tau time-exponent))))
				      (:unit-area (/ 1.0 (alpha-integral tau time-exponent)))
				      (t 1.0)))
	  (minimum (s-flt (abs (* (alpha-function-float tau tau adjustment-numeric-value) *wave-cutoff*)))))
     (declare (single-float adjustment-numeric-value minimum))

     (loop for time single-float from 0.0 by step
	   collect (+ offset
		      (if (< time delay) 0.0
			(* amplitude
			   (the sf (alpha-function-float tau (- time delay) adjustment-numeric-value
							 TIME-EXPONENT
							 )))))
	   into values
	   when (if (and duration (> duration 0)) 
		    (>= time duration)
		  (and (> (- time delay) tau) (< (abs (- (the sf (car (last values))) offset)) minimum)))
	   do (return (list-to-array values))))))

(defun alpha-list (&optional (tau 1.0)
			     &key (adjustment :NORMALIZE) (step 1.0) (duration 0)
			     (offset 0.0) (amplitude 1.0) (delay 0.0))
  "As in ALPHA-ARRAY, but returns a list." 
  (array-to-list
   (alpha-array tau :adjustment adjustment :step step :duration duration :offset offset :delay delay :amplitude amplitude)))

#|
(defun alpha (&optional (tau 1.0)
			&key (adjustment :NORMALIZE) (step 1.0) (duration 0) (offset 0.0) (delay 0.0) (amplitude 1.0))
  "As in ALPHA-ARRAY." 
  (alpha-array tau :adjustment adjustment :step step :duration duration :offset offset :delay delay :amplitude amplitude))
|#

(defun alpha-impulse (tau tau-power)
  (declare (ignore tau-power))
  (alpha-array tau))


(defun alpha-function (tau time &optional (adjustment :normalize))
  (* (if (numberp adjustment)
	 adjustment
       (case adjustment
	 (:normalize (/ (exp-w-limits 1.0) tau))
	 (:unit-area (/ 1.0 (* tau tau)))
	 (t 1.0)))
     time
     (exp-w-limits (/ (- 0.0 time) tau))))


(defun alpha-function-float (tau time &optional (adjustment :normalize) (TIME-EXPONENT 1.0))
  (declare (optimize (safety 0) (speed 3) (space 1))
	   (single-float tau time TIME-EXPONENT))
  (* (the sf (if (numberp adjustment)
		 adjustment
		 (case adjustment
		   (:normalize (the sf (/ 1 (alpha-max-value tau time-exponent))))
		   (:unit-area (/ 1.0 (alpha-integral tau time-exponent)))
		   (t 1.0))))
     (if (= TIME-EXPONENT 1) time (expt time TIME-EXPONENT))
     (s-flt (exp-w-limits-double (/ (- 0.0d0 time) tau)))))


(defun general-alpha-function (tau time-power time)
  (* (/ (expt time time-power) tau)
     (exp-w-limits (/ (- 0.0 time) tau))))


(defun DOUBLE-ALPHA-array (&optional (tau1 1.0) (tau2 1.0) (alpha-proportion 1.0)
				     &key (offset 0.0) (step 1.0) (tau1-alpha-area 1.0) (start 0.0))
  "Returns an array with the difference of two alpha functions, defined by TAU1 and TAU2 [ms]
respectively. The area of the first alpha function is defined with TAU1-ALPHA-AREA [default 1.0],
with the relative area of the second given by ALPHA-PROPORTION [default 1.0]. Thus the total
integral is equal to (TAU1-ALPHA-AREA * (1 - ALPHA-PROPORTION)). The length of the array is
determined when the value of the component with the longest time constant is less than than
*WAVE-CUTOFF* times its maximum. A correction term is added to the waveform in order to give the
proper integral despite the truncated length. The value of OFFSET [default 0.0] is added to the
final waveform. Function time argument is referenced from START [default 0.0]."
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (let* ((tau1 (s-flt tau1))
	 (tau2 (s-flt tau2))
	 (start (s-flt start))
	 (alpha-proportion (s-flt alpha-proportion))
	 (tau1-alpha-area (s-flt tau1-alpha-area))
	 (offset (s-flt offset))
	 (tau1-array
	  (alpha-array tau1 :adjustment :unit-area :step step :amplitude tau1-alpha-area
		       :delay start))
	 (tau2-array
	  (alpha-array tau2 :adjustment :unit-area :step step :amplitude (* tau1-alpha-area ALPHA-PROPORTION)
		       :delay start))
	 (max-tau1-array-index (the fn (1- (length tau1-array))))
	 (max-tau2-array-index (the fn (1- (length tau2-array))))
	 (max-index (the fn (max max-tau1-array-index max-tau2-array-index)))
	 (longest-array (if (> max-tau1-array-index max-tau2-array-index) tau1-array tau2-array)))
    (loop for index fixnum from 0 to max-index do
	  (setf (aref longest-array index)
		(- (the sf (if (> index max-tau1-array-index) 0.0 (aref tau1-array index)))
		   (the sf (if (> index max-tau2-array-index) 0.0 (aref tau2-array index))))))
    (let* ((actual-area (loop for index fixnum from 0 to max-index
			      sum (the sf (aref longest-array index))
			      into result single-float finally (return result)))
	   (area-adjustment (the sf (/ (- actual-area (* TAU1-ALPHA-AREA (- 1 ALPHA-PROPORTION)))
				       (1+ max-index)))))
      (loop for index fixnum from 0 to max-index
	    do (setf (aref longest-array index)
		     (the sf (+ offset
				(aref longest-array index)
				(- area-adjustment)))))
      longest-array)))

(defun DOUBLE-ALPHA (&optional (tau1 1.0) (tau2 1.0) (alpha-proportion 1.0)
			       &key (offset 0.0) (step 1.0) (tau1-alpha-area 1.0) (start 0.0))
  (DOUBLE-ALPHA-array tau1 tau2 alpha-proportion
		      :offset offset :step step :tau1-alpha-area tau1-alpha-area :start start))


			 
















;;;;;;;;;;;;;; Misc Waveforms ;;;;;;;;;;;;;;;;;




(defvar *factorials*)

;;;This function calculates factorials of n up to 10.  It loads it in the
;;;global list fac.

(defun FACT ()        
  (setq *factorials* '(1))
  (do ((i 1 (+ i 1)))
      ((= i 11))
    (setq *factorials* (cons (* i (car *factorials*)) *factorials*)))
  (setq *factorials* (reverse *factorials*)))

;;;The following function evaluates the factorial-list upon loading.

(EVAL-WHEN (load) (fact))



;;; GAMMA-DISTRIBUTION returns an array of length 'length of a normalized gamma distribution function.

(defun GAMMA-DISTRIBUTION (length tau power)
  (without-floating-underflow-traps		
   (let ((*excitatory-facilitation-impulse-array (make-array length :element-type 'single-float)))
     (dotimes (i length)
       (setf (aref *excitatory-facilitation-impulse-array i)
	     (* (/ 1.0 (* (nth power *factorials*) tau)) (expt (/ i tau) power) (exp-w-limits (- 0.0 (/ i tau))))))
     *excitatory-facilitation-impulse-array)))


;;; The following function is the convolution of two normalized alpha function with time constants
;;; tau1 and tau2.

(defun DISTORTED-ALPHA (length tau1 tau2)
  (without-floating-underflow-traps		
   (let ((*excitatory-facilitation-impulse-array (make-array length :element-type 'single-float)))
     (dotimes (i length)
       (let ((e1 (exp-w-limits (- 0.0 (/ i tau1))))
	     (e2 (exp-w-limits (- 0.0 (/ i tau2)))))	      
	 (setf (aref *excitatory-facilitation-impulse-array i)
	       (/ (+ (* i (+ e1 e2)) (/ (* -2 tau1 tau2 (- e1 e2)) (- tau1 tau2)))
		  (* (- tau1 tau2) (- tau1 tau2))))))
     *excitatory-facilitation-impulse-array)))

;;; The following function is the convolution of a normalized alpha function with time constant tau1
;;; and a normalized exponential distribution with time constant tau2.

(defun DISTORTED-EXPONENTIAL (length tau1 tau2)
  (without-floating-underflow-traps		
   (let ((*excitatory-facilitation-impulse-array (make-array length :element-type 'single-float)))
     (dotimes (i length)
       (let ((e1 (exp-w-limits (- 0.0 (/ i tau1))))
	     (e2 (exp-w-limits (- 0.0 (/ i tau2)))))	      
	 (setf (aref *excitatory-facilitation-impulse-array i)
	       (/ (- (/ (* tau1 tau2 (- e2 e1)) (- tau2 tau1)) (* i e1)) (* tau1 (- tau2 tau1))))))
     *excitatory-facilitation-impulse-array)))



;;;ZERO-TRIPLE-ALPHA returns an array of length 'length of the difference between one alpha function
;;;and the sum of two others.  The first alpha function is fast ('tau1) while the other two are
;;;slow.  The second alpha function is faster ('tau2) than the third ('tau3).  The total area is
;;;zero.  The second alpha function contains 'alpha-proportion proportion of the total area of the
;;;negative functions.
(defun ZERO-TRIPLE-ALPHA (length tau1 tau2 tau3 alpha-proportion)
  (without-floating-underflow-traps		
   (let ((exc-fac-impulse (make-array length :element-type 'single-float)))
     (dotimes (i length)
       (setf (aref exc-fac-impulse i)
	     (- (*                        (/ i (* tau1 tau1)) (exp-w-limits (- 0.0 (/ i tau1))))
		(* alpha-proportion       (/ i (* tau2 tau2)) (exp-w-limits (- 0.0 (/ i tau2))))
		(* (- 1 alpha-proportion) (/ i (* tau3 tau3)) (exp-w-limits (- 0.0 (/ i tau3)))))))
     exc-fac-impulse)))

;;; TRIPLE-ALPHA Returns an array of length 'length of the difference of two alpha functions, with a
;;; total area of 0, plus another alpha function of area 1. The amplitude of the alpha difference
;;; (the transient part) is scaled by the the optional argument 'transient-amplitude.
(defun triple-alpha (length tau1 tau2 sus-tau &optional (transient-amplitude 1))
  (without-floating-underflow-traps		
   (let ((array (make-array length :element-type 'single-float)))
     (dotimes (i length)
       (setf (aref array i)
	     (+ (* transient-amplitude
		   (- (* (/ i (* tau1 tau1)) (exp-w-limits (- 0.0 (/ i tau1))))
		      (* (/ i (* tau2 tau2)) (exp-w-limits (- 0.0 (/ i tau2))))))
		(* (/ i (* sus-tau sus-tau)) (exp-w-limits (- 0.0 (/ i sus-tau)))))))
     array)))

;;; D-ALPHA-ARRAY Returns an array of length length of the derivitave of an alpha function whose
;;; maximum amplitude is 1.
(defun d-alpha-array (length tau tau-power)
  (declare (ignore length tau tau-power))
  (let ((d-alpha-array (make-array 5 :element-type 'single-float)))
    (setf (aref d-alpha-array 0) 1.0)
    (setf (aref d-alpha-array 1) 0.0)
    (setf (aref d-alpha-array 2) -.5)
    (setf (aref d-alpha-array 3) -.3)
    (setf (aref d-alpha-array 4) -.2)
    d-alpha-array))

;;; TRANS-SUS-ARRAY
(defun trans-sus-array (length tau tau-power)
  (declare (ignore length tau tau-power))
  (let ((array (make-array 5 :element-type 'single-float)))
    (setf (aref array 0) 1.25)
    (setf (aref array 1) 0.0)
    (setf (aref array 2) -.5)
    (setf (aref array 3) -.3)
    (setf (aref array 4) -.2)
    array))

