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


;;; SYS Source file: analysis.lisp

(in-package "SURF-HIPPO")

;;					
;; Main function for on-line analysis of simulation results
;;
;; related functions in misc.lisp and others




(defun apply-function-to-plot-DATA-plot-list-data (function)
  (no-nils
   (loop for plot-list-info in *PLOT-LISTS-INFO*
	 when (and (plot-list-info-enable-var plot-list-info)
		   (plot-list-info-structures plot-list-info)
		   (eq 'voltage (plot-list-info-structure-slot plot-list-info))
		   )
	 collect
	 (let ((plot-data 
		(retrieve-plot-data (list 
				     (list (symbol-value (plot-list-info-structures plot-list-info))
					   (plot-list-info-structure-slot plot-list-info))))))
	   (when plot-data
	     (funcall function
		      plot-data
		      (symbol-value (plot-list-info-names plot-list-info))))))))



(defun print-analysis ()
  (let (results)
    (cond-every
     (*print-linear-analysis*
      (loop for result-list in (APPLY-FUNCTION-TO-PLOT-DATA-PLOT-LIST-DATA 'INTEGRATE-PLOT-DATA)
	    do (loop for result in result-list do (push result RESULTS)))
      (loop for result in (INTEGRATE-plot-DATA
			   (retrieve-plot-data (list (list *analysis-nodes* `voltage)))
			   *analysis-nodes* *x-integrate-min* *x-integrate-max*)
	    do (push result RESULTS))
      (format t "~%"))
     (*print-nonlinear-analysis*
      (loop for result-list in (APPLY-FUNCTION-TO-PLOT-DATA-PLOT-LIST-DATA 'max-min-plot-data)
	    do (loop for result in result-list do (push result RESULTS)))
      (loop for result in (max-min-plot-DATA
			   (retrieve-plot-data (list (list *analysis-nodes* `voltage)))
			   *analysis-nodes* *x-integrate-min* *x-integrate-max*)
	    do (push result results))
      (format t "~%"))
     ((and *print-axon-spike-times* (> (hash-table-count (AXON-HASH-TABLE)) 0))
      (PRINT-AXON-SPIKE-TIMES))
     ((and *print-synapse-event-times* (> (hash-table-count (SYNAPSE-HASH-TABLE)) 0))
      (PRINT-SYNAPSE-EVENT-TIMES))
     ((and *print-synapse-total-events* (> (hash-table-count (SYNAPSE-HASH-TABLE)) 0))
      (print-synapse-total-events))
     (*DUMP-ANALYSIS-TO-FILE*
      (dump-analysis-file results)))
    results)
  )




(defun plot-iv (&optional cells)
  (let* ((cells (if cells
		    (loop for cell in (typecase cells
					(cons cells)
					(t (list cells)))
			  collect (element cell 'cell))
		    (cells)))
	 (ivs (loop for cell in cells collect (i-v-characteristic cell)))
	 *create-new-plot-windows*
	 (*plot-window-width* 450) (*plot-window-height* 350))
    (plot-timed-data (list (loop for iv in ivs collect (car iv)))
			   
		     (loop for cell in cells collect
			   (format nil "~A Somatic IV" (element-name cell)))
		     (cadr (car ivs))
			   
		     :prompt-for-overlay t
		     :title (format nil "~ASomatic IV~:P"
				    (if (= (length cells) 1)
					(format nil "~A " (cell-name (car cells))) ""))
		     :x-min -150.0 :x-max 50.0 :x-inc 25.0 :x-origin-tick t :y-origin-tick nil
		     :y-label "nA" :x-label "mV"
		     :y-label-vertical-position :upper-right
		     :accomodate-all-overlays t
		     :overlay dummy2	; :width 450 :height 350
		     )))

(defun plot-ivs (&optional cells (use-menu t) use-one-window)
  (let* ((names (loop for cell in (or cells (cells)) collect (element-name cell 'cell)))
	 (*automatic-run* (or *automatic-run* (not use-menu)))
	 (cells (choose-list-values names (if use-menu nil names) :label "Choose cells for IV curve(s)"))
	 (use-one-window (if (and (> (length cells) 1) use-menu)
			     (go-ahead-menu "Plot all IVs in one window?" "IV Details" use-one-window) 
			     use-one-window)))
    (if use-one-window (plot-iv cells)
	(loop for cell in cells
	      do (plot-iv cell)))))


(defun i-v-characteristic (&optional (cell *cell*))
  (set-circuit-elements-parameters)	; Just to be sure (for non-simulated cells).
  (initialize-simulation)
  (let ((cell (element cell 'cell))
;
;        (current-plot-flags (loop for ch in (channels)
;                                  collect (channel-plot-conductance ch) into flags
;                                  do (setf (channel-plot-conductance ch) t)
;                                  finally (return flags)))

	)	
    (initialize-simulation)		; To register the plotted conductances
    (let* ((soma (cell-soma cell))
	   (tree-load
	    (if (cell-tree-p cell)
		(/ 1.0 (z-tree-cable-in-cell cell)) 0.0))
	   (iv
	    (loop for voltage from -150.0 to 50 by 1.0
		  do
		  ;; set the voltage
		  (loop for soma being the hash-value of (SOMA-HASH-TABLE) do
			(set-node-voltage (soma-node soma) voltage))
		  (INIT-ALL-NODES)			
		  ;; Now that node voltages are set, initialize the particle states and calculate the
		  ;; channel currents.
		  (when *active*
		    (eval-all-particles t)
		    (eval-all-conc-particles t)
		    (eval-all-channels))

		  ;; add the passive soma current.
		  collecting
		  (coerce
		   (+ (* (soma-g-leak soma) (- voltage (soma-v-leak soma)))
		      (* (if (soma-include-shunt soma) (soma-g-shunt soma) 0.0)
			 voltage)
		      (* tree-load (- voltage (cell-type-dendrite-v-leak (cell-type cell))))
		      (if *active*
			  (loop for elt in (node-elements (soma-node (cell-soma cell)))
				when (channel-active-p elt) sum (get-channel-current-not-inline elt))
			  0.0))
		   'single-float)
		  into nanoamps
		  collecting voltage into volts
		  finally (return (list nanoamps volts)))))
      ; Restore plot flags
;      (loop for ch in (channels)
;            for flag in current-plot-flags
;            do (setf (channel-plot-conductance ch) flag))
      iv)))


;;; MAX-MIN-PLOT-DATA prints out and returns the maxs and mins of each of the data sets of
;;; the plot-list, in the following format:
;;;
;;; (((NODE-NAME-1-SYMBOL (MAX maximum) (MIN minimum)) 
;;;                   .
;;;                   .
;;;                   .
;;;  ((NODE-NAME-N-SYMBOL (MAX maximum) (MIN minimum)))
;;;
;;; The NODE-NAME-N-SYMBOLs are made of the labels in LABEL-LIST, with NODE prepended.
(defun max-min-plot-DATA (plot-data-list label-list &optional
					 (x-min *x-integrate-min*)
					   (x-max *x-integrate-max*) (units "mV"))
  (without-floating-underflow-traps
   (loop for plot-data in plot-data-list
	 for label in label-list
	 collect
	 (let ((max-min (loop for val in plot-data
			      for time in *sim-reverse-plot-time-list*
			      when (and (if x-min (>= time x-min) t)
					(if x-max (<= time x-max) t))
			      maximize val into max
			      minimize val into min
			      finally (return (list max min)))))
	   (format t "Node ~a Max =  ~a, Min = ~a ~a ~%"
		   label (my-float-format (car max-min)) (my-float-format (cadr max-min)) (string units))
	   (list (create-output-symbol "node" t label) (cons 'Max (car max-min)) (cons 'min (cadr max-min)))))))


;;; INTEGRATE-PLOT-DATA prints out and returns the integrals of each of the data sets of
;;; the PLOT-DATA-LIST, in the following format when *AVERAGE-INTEGRALS* is NIL:
;;;
;;; (((NODE-NAME-1-SYMBOL (INTEGRAL integral) (BASE integral-base)) 
;;;                   .
;;;                   .
;;;                   .
;;;  ((NODE-NAME-N-SYMBOL (INTEGRAL integral) (BASE integral-base))) 
;;;
;;; And in the following format when *AVERAGE-INTEGRALS* is T:
;;;
;;; (((NODE-NAME-1-SYMBOL (AVERAGE average) (BASE average-base)) 
;;;                   .
;;;                   .
;;;                   .
;;;  ((NODE-NAME-N-SYMBOL (AVERAGE average) (BASE average-base))) 
;;;
;;; The NODE-NAME-N-SYMBOLs are made of the labels in LABEL-LIST, with NODE prepended.
;;; Note that the plot data lists must be in time-decreasing order.
(defun INTEGRATE-plot-DATA (plot-data-list label-list &optional (x-min *x-integrate-min*)
					   (x-max *x-integrate-max*) (units "mV"))
  (without-floating-underflow-traps
   (loop for plot-data in plot-data-list
	 for label in label-list
	 collect (let ((result
			(integrate-x-y
			 plot-data *sim-reverse-plot-time-list* :y-base *integral-base* :x-min x-min :x-max x-max
			 :average *average-integrals*)))
		   (format t "Node ~a ~a =  ~a ~a (ref ~a) ~%"
			   label (if *average-integrals* "average" "integral")
			   (my-float-format result)
			   (if *average-integrals* (string units) (format nil "~A-ms" units))
			   *integral-base*)
		   (list
		    (create-output-symbol "node" t label)
		    (cons (if *average-integrals* 'average 'integral) result)
		    (cons 'base *integral-base*))))))

(defun time-integral (data-list time-list &optional data-offset)
  (let ((integral 0.0))
    (do ((lt time-list (cdr lt))
	 (lv data-list (cdr lv)))
	((not (cdr lt))) 
      (setq integral (+ integral (* (- (car lv) data-offset) (- (cadr lt) (car lt))))))
    integral))



(defun fit-exp (data-list start stop &key (delta-t 1.0) plot-normalize-fit
			  (max-amp-proportion 0.5)
			  (min-amp-proportion 0.05)
			  negative-p return-fit)
  "Fit single exponential to evenly sampled DATA-LIST points between START and STOP, with grid
DELTA-T."
  (multiple-value-bind (min max)
      (loop for data in data-list
	    for time from 0.0 by delta-t
	    when (>= stop time start) maximize data into max and minimize data into min
	    finally (return (values min max)))
    (format t "min ~A max ~A~%" min max)
    (let* ((amp (abs (- max min)))
	   (pre-collect-data t)
	   post-collect-data
	   (max-amp (* max-amp-proportion amp))
	   (min-amp (* min-amp-proportion amp)))
      (multiple-value-bind (transformed-data-list transformed-time-list)

	  (loop for data in data-list	
		for time from 0.0 by delta-t
		when (and (>= time start) (<= time stop))
		do (format t "time ~A data ~A max ~A min-amp ~A max-amp ~A~%" time data max min-amp max-amp)
		(if (and (if negative-p (>= (- max data) min-amp) (>= (- data min) min-amp))
			 (if negative-p (<= (- max data) max-amp) (<= (- data min) max-amp))
			 (>= time start)
			 (not post-collect-data))
		    (if (> time stop)
			(setq post-collect-data t)
			(progn (print 'foo) (setq pre-collect-data nil)))
		    (if (not post-collect-data) (setq pre-collect-data t)))
		(format t "pre-collect ~A post-collect ~A~%" pre-collect-data post-collect-data)
		when (and (not post-collect-data) (not pre-collect-data))
		collect (/ (if negative-p (- max data) (- data min)) max-amp) into output-list
		and collect time into time-list
		finally (return (values output-list time-list)))

	
	(let ((transformed-time-start (car transformed-time-list)))
	  (loop for time in transformed-time-list
		for data in transformed-data-list
		sum (square (- time transformed-time-start)) into num
		sum (* (- time transformed-time-start) (log data)) into den
		finally
		(return
		  (let* ((tau (/ num den))
			 (fitted-normalized-data
			  (when (or plot-normalize-fit return-fit)
			    (loop for time in transformed-time-list
				  for data in data-list 
				  collect (exp (/ (- time transformed-time-start) tau)))))
			 (fitted-data
			  (when (or plot-normalize-fit return-fit)
			    (loop for data in fitted-normalized-data
				  collect (+ (* max-amp (if negative-p -1 1) data)
					     (if negative-p max min))))))
		    (when plot-normalize-fit
		      (plot-timed-data (list fitted-normalized-data transformed-data-list)
				       (list "Normalised-Fit" "Normalised-Data")
				       transformed-time-list :title "Normalized Data" :y-max 1.0 :y-min 0.0))
		    (if return-fit
			(progn
			  (format t "tau ~A~%" (/ num den))
			  (list transformed-time-list fitted-data))
			(/ num den))))))))))

(defun convert-data-dt-lists (data-list old-time-ref new-time-ref
					&optional (output-order-data-time t) include-time-list new-time-ref-is-length)
  "As CONVERT-DATA-TIME-LISTS, with that function's TIME-LIST arg set to a sequence whose length is
given by the length of DATA-LIST, starting with 0 and incremented by OLD-TIME-REF."
  (if (= old-time-ref new-time-ref)
      (sequence-to-list data-list)
      (convert-data-time-lists data-list
			       old-time-ref
			       new-time-ref
			       output-order-data-time include-time-list new-time-ref-is-length)))

#|
(defun interpolate-data (data-0 time-0 data-1 time-1 new-time)
  (+ data-1 (* (- new-time time-1) (/ (- data-0 data-1) (- time-0 time-1)))))
|#

(proclaim '(inline interpolate-data))
(defun interpolate-data (data-t0 t0 data-t-1 t-1 new-time)
  (declare (optimize (safety 0) (speed 3) (space 1))) 
  (if t-1
      (+ (the sf data-t-1) (* (- (the sf new-time) (the sf t-1))
			      (/ (- (the sf data-t0) (the sf data-t-1))
				 (- (the sf t0) (the sf t-1)))))
      (the sf data-t0)))

(proclaim '(inline interpolate-data-df))
(defun interpolate-data-df (data-t0 t0 data-t-1 t-1 new-time)
  (declare (optimize (safety 0) (speed 3) (space 1))) 
  (if t-1
      (+ (the df data-t-1) (* (- (the df new-time) (the df t-1))
			      (/ (- (the df data-t0) (the df data-t-1))
				 (- (the df t0) (the df t-1)))))
      (the df data-t0)))


(defun convert-data-time-lists (data-list time-ref new-time-ref &optional (output-order-data-time t) include-time-list new-time-ref-is-length)
  "Given a list of time points in TIME-REF, which may not be evenly spaced, and a sequence of data
points in DATA-LIST that refer to these time points, generate a data list that is sampled evenly
[linear interpolation] at intervals derived from NEW-TIME-REF. If TIME-REF is a single number, this
is taken as the fixed time step of the input data. If NEW-TIME-REF-IS-LENGTH is NIL [default NIL]
then NEW-TIME-REF is the new time step, whose units are the same assumed in TIME-REF. Otherwise, the
new time step is chosen so that the resampled data list has length given by NEW-TIME-REF. For making
evenly sampled versions of Surf-Hippo simulations, the current simulation time list is found with
CURRENT-SIM-PLOT-TIME-LIST. When the optional INCLUDE-TIME-LIST arg is T, depending on the optional
argument OUTPUT-ORDER-DATA-TIME [default T] the function returns:

 (list new-data-list new-time-list)  <=  OUTPUT-ORDER-DATA-TIME = T 
 (list new-time-list new-data-list)  <=  OUTPUT-ORDER-DATA-TIME = NIL

Otherwise the function returns just the new-data-list."
  (declare (optimize (safety 0) (speed 3) (space 1))) 
  (let* ((time-list (if (consp time-ref) time-ref (list-of-nums (length data-list) 0.0 time-ref)))
	 (data-list (sequence-to-list data-list))
	 (new-time-step (if new-time-ref-is-length
			  (* time-ref (/ (length data-list) new-time-ref))
			  (s-flt new-time-ref)))
	 (time-list-in-reverse-order-p (> (car time-list) (car (last time-list))))
	 (time-list (if time-list-in-reverse-order-p (reverse time-list) time-list))
	 (data-list (if time-list-in-reverse-order-p (reverse data-list) data-list))
	 (time-1 (car time-list))
	 (time-0 (cadr time-list))
	 (first-point (car time-list))
	 (data-1 (car data-list))
	 (data-0 (cadr data-list))
	 fudge-first-time)
    (declare (single-float time-0 time-1 data-0 data-1))
    (setq time-list (cdr time-list) data-list (cdr data-list))
    (loop for new-time single-float from first-point by new-time-step until (and (>= new-time time-0) (null (cdr data-list)))
	  unless (and fudge-first-time first-point)
	  do (loop until (or (null (cdr data-list)) ;; (format t "... inner loop...  ~%")
			     (and (<= time-1 new-time) (< new-time time-0)) )
		   do (setq time-1 time-0
			    time-list (cdr time-list)
			    time-0 (car time-list)
			    data-1 data-0
			    data-list (cdr data-list)
			    data-0 (car data-list)))

          ;; (format t "new-time ~A, time-1 ~A, time-0 ~A,  data-1 ~A,  data-0 ~A, new-time-step ~A ~%"
          ;;        new-time time-1 time-0 data-1 data-0 new-time-step)
	  
	  when (or (and fudge-first-time first-point) (not (= time-0 time-1)))
	  ;; do (format t "  pushing....~%") and
	  collect (if (and fudge-first-time first-point) first-point (interpolate-data data-0 time-0 data-1 time-1 new-time))
	  into new-data-list
	  and when include-time-list
	  collect new-time into new-time-list

	  do (setq first-point nil)
	  
	  finally
	   ;; (format t "Finishing.... length of data-list ~A ~%" (length data-list))
					;	  (break)
	   ;; (setq new-data-list (append new-data-list data-list)
	;;	new-time-list (append new-time-list (list new-time-step)))
	  (when new-time-ref-is-length
	    (cond ((< (length new-data-list) new-time-ref)
		   ;; (format t "(< (length new-data-list) new-time-ref)... ~A, ~A~%" (length new-data-list) new-time-ref) 
		   (setq new-data-list (wh::pad-end new-data-list new-time-ref))
		   (when include-time-list
		     (setq new-time-list (append new-time-list
					       (list-of-nums (- new-time-ref (length new-data-list))
							     (+ new-time-step (car (last new-time-list)))
							     new-time-step)))))
		  ((< new-time-ref (length new-data-list))
		   ;; (format t "(< new-time-ref (length new-data-list))...~%")
		   (setq new-data-list (list-head new-data-list new-time-ref))
		   (when include-time-list
		     (setq new-time-list (list-head new-time-list new-time-ref))))))
	  ;; (format t "   Returning....~%")
	  (return (if include-time-list
		    (let ((output (list new-data-list new-time-list)))
		      (if output-order-data-time output (reverse output)))
		    new-data-list)))))

	  

#|
(defun value-at-time (data-list time-list target-time)
  (let* ((first-t (car time-list))
	 (last-t (car (last time-list)))
	 (time-list (if (> first-t last-t) (reverse time-list) time-list))
	 (data-list (if (> first-t last-t) (reverse data-list) data-list))
	 time	 time-1	 data	 data-1)
    (setq time (car time-list))
    (setq time-1 time)
    (setq time-list (cdr time-list))
    (setq time (car time-list))
    (setq data (car data-list))
    (setq data-1 data)
    (setq data-list (cdr data-list))
    (setq data (car data-list))
    (loop for new-t from 0.0 by new-time-step
	  until (null data-list)
	  collect
	  (+ data-1
	     (if (= 0 (- time time-1)) 0 (* (- new-t time-1) (/ (- data data-1) (- time time-1)))))
	  into new-data-list
	  collect new-t into new-time-list
	  do
	  (loop until (or (null data-list) (and (<= time-1 new-t)(< new-t time)) )
		do
		(setq time-1 time)
		(setq time-list (cdr time-list))
		(setq time (car time-list))
		(setq data-1 data)
		(setq data-list (cdr data-list))
		(setq data (car data-list)))
	  finally 
	  (return (list new-data-list new-time-list)))))
|#

