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


;;; SYS Source file: calc-lte-ratio.lisp

(in-package "SURF-HIPPO")

;; CALC-VOLTAGE-LTE-RATIO calculates the linear truncation error (LTE) over all the nodes in
;; *NODE-W/ELEMENTS-ARRAY* and returns the ratio of the estimate of max error divided by the allowed
;; error. This function also estimates the voltage derivatives, and thus is used even when
;; *USE-FIXED-STEP* is T.

;; old note - Estimate O(n^2) error using V''(t + dt/2) = [V'(t + dt) - V'(t)] / dt

(defun set-dvdts ()
  (declare (optimize (safety 0) (debug 0) (speed 3) (space 0)))
  (let ((max-dv-diff 0.0d0))
    (declare (double-float max-dv-diff)
	     (values double-float))
    (dotimes (index (the fn *NODE-W/ELEMENTS-ARRAY-LENGTH*))
      (declare (fixnum index))
      (let ((nd (aref (the (simple-array node) *NODE-W/ELEMENTS-ARRAY*) index)))
	;; GET-NODE-DVDT returns
	;;        v(t[n+1]) - v(t[n])
	;;        -------------------
	;;            *delta-t[n]*
	(setf (node-dvdt-n nd) (get-node-dvdt nd))
	(let ((dv-diff (abs (- (node-dvdt-n nd) (node-dvdt-n-1 nd)))))
	  (when (> dv-diff max-dv-diff) (setf max-dv-diff dv-diff)))))
    (if *use-fixed-step*
	0.0d0				; Dummy double float value to return.
	max-dv-diff)))
	
;; Sets *RELATIVE-VOLTAGE-LTE*, the maximum relative voltage error. Also calculates all node-dvdts for nodes with elements.
(defun calc-voltage-lte-ratio ()
  (declare (optimize (safety 0) (debug 0) (speed 3) (space 0)))
  (let ((max-dv-diff (set-dvdts)))
    (declare (double-float max-dv-diff))
    (unless *use-fixed-step*
      (setq *relative-voltage-lte* 
	    (if (= 0.0d0 max-dv-diff)
		0.0			; If error is zero, return immediately.
		;; Otherwise, return ratio of estimated LTE (linear truncation error) / *ABSOLUTE-VOLTAGE-ERROR*.
		(let ((lte-voltage (* (*delta-t[n]-squared*)
				      (/ max-dv-diff (* 2.0 (*delta-t-prime[n]*)
							;; need double float stored in (*delta-t-prime[n]*)
							;; (- (*t-prime[n+1]*) (*t-prime[n]*))
							)))))
		  (s-flt (the df (/ lte-voltage *ABSOLUTE-VOLTAGE-ERROR*))))))))
  nil)

;; Sets *relative-particle-lte*. Returns T if error OK.
(defun particle-error-ok ()
  (declare (optimize			; (safety 1)
	    (speed 3) (space 0) (compilation-speed 0)))
  (or (or *use-fixed-step* (not *calculate-particle-error*)) ; Return "ok" if we don't care about particle error.
      ;; *maximum-particle-error-numerator* = (x-dot-n - x-dot-n-1)
      (let ((max-particle-lte
	     (s-flt (if (<= (*maximum-particle-error-numerator*) 1.0d-10) ; Getting problems with values around 1.0e-320.
			0.0
			(* (*delta-t-prime[n]-squared*)
			   (/ (*maximum-particle-error-numerator*)
			      (* 2 (- (*t-prime[n-prime]*) (*t-prime[n-prime-1]*)))))))))
	(declare (single-float max-particle-lte))
	(setq *relative-particle-lte* (/ max-particle-lte *absolute-particle-error*))
	(when *debug-particle-error*
	  (format t "time ~f part ~A has Max error ~f~%" *real-time* *particle-w-max-error* max-particle-lte))
	(<= *relative-particle-lte* 1))))

;; Sets *relative-conc-int-lte*. Returns T if error OK.
(defun conc-int-error-ok ()
  (declare (optimize			; (safety 1)
	    (speed 3) (space 0) (compilation-speed 0)))
  (or (or *use-fixed-step* (not *calculate-conc-int-error*)) ; Return "ok" if we don't care about conc-int error.
      ;; *maximum-conc-int-error-numerator* = (x-dot-n - x-dot-n-1)
      (let ((max-conc-int-lte
	     (s-flt
	      (if (<= (*maximum-conc-int-error-numerator*) 1.0d-10) ; Getting problems with values around 1.0e-320.
		  0.0
		  (* (*delta-t[n]-squared*)
		     (/ (*maximum-conc-int-error-numerator*)
			(* 2 (- (*t-prime[n+1]*) (*t-prime[n]*)))))))))
	(declare (single-float max-conc-int-lte))
	(setq *relative-conc-int-lte* (/ max-conc-int-lte *absolute-conc-int-error*))
	(when *debug-conc-int-error*
	  (format t "time ~f part ~A has Max error ~f~%" *real-time* *conc-int-w-max-error* max-conc-int-lte))
	(<= *relative-conc-int-lte* 1))))



; (pick-time-step lte-ratio (if lte-ok *sim-time-n+1* *sim-time-n*)))))


(defun check-element-time-step-maximum (element-mrt-step)
  (declare (optimize (safety 0) (speed 3) (space 0))
	   (fixnum element-mrt-step))
  (when (or (not *element-time-step-maximum*)
	    (> (the fn *element-time-step-maximum*) element-mrt-step))
    (setf *element-time-step-maximum* element-mrt-step)
    nil))

      
(proclaim '(inline get-max-allowed-particle-step))
(defun get-max-allowed-particle-step ()
;  (declare (optimize (safety 0) (speed 3) (space 0)))
  (if (and *calculate-particle-error* (> *relative-particle-lte* 0.0))
      (let* ((error-ok (<= *relative-particle-lte* 1.0))
	     (delta-t-previous (if error-ok (*delta-t[n]*) (*delta-t[n-1]*))))
	(declare (double-float delta-t-previous))
	;; Max delta-t-particle
	(let ((Max-delta-t-particle
	       (- (/ (* 2 (*delta-t-prime[n]*)) (the sf (sqrt *relative-particle-lte*)))
		  delta-t-previous)))
	  (when (> (the sf *user-min-step*) Max-delta-t-particle)
	    (push *real-time* *particle-ERROR-STEP-less-than-min-step*))
	  (max *user-min-step-double* Max-delta-t-particle)))
      -1.0d0))

(proclaim '(inline get-max-allowed-voltage-step))
(defun get-max-allowed-voltage-step ()
  (declare (optimize (safety 0) (speed 3) (space 0)))
  (when  (< *relative-voltage-lte* 0.0) (break))
  (if (> *relative-voltage-lte* 0.0)
      (/ (*delta-t[n]*) (the sf (sqrt *relative-voltage-lte*)))
      -1.0d0))

(proclaim '(inline get-max-allowed-conc-int-step))
(defun get-max-allowed-conc-int-step ()
  (declare (optimize (safety 0) (speed 3) (space 0)))
  (if (and *calculate-conc-int-error* (> *relative-conc-int-lte* 0.0))
      (/ (*delta-t[n]*) (the sf (sqrt *relative-conc-int-lte*)))
      -1.0d0))

(defun get-max-allowed-mrt-step ()
  (declare (optimize (safety 0) (speed 3) (space 0)))
  (let* (;; Each of these return -1.0 is error is 0 or error is not considered.
	 (max-allowed-particle-step (the df (get-max-allowed-particle-step)))
	 (max-allowed-conc-int-step (the df (get-max-allowed-conc-int-step)))
	 (max-allowed-voltage-step (the df (get-max-allowed-voltage-step)))

	 (particle-is-min
	  (unless (= max-allowed-particle-step -1)
	    (< max-allowed-particle-step (if (= max-allowed-conc-int-step -1)
					     max-allowed-voltage-step
					     (min max-allowed-voltage-step max-allowed-conc-int-step)))))
	 (conc-int-is-min
	  (unless (or particle-is-min
		      (= max-allowed-conc-int-step -1))
	    (< max-allowed-conc-int-step max-allowed-voltage-step)))
	 (max-allowed-step
	  (the df
	       (cond (particle-is-min
		      (when *count-error-step-change* (push *real-time* *particle-ERROR-STEP-CHANGES*))
		      (when *debug-particle-error-step-change*
			(format t " real-time: ~a PICK-TIME-STEP limited by particle error~%" *real-time*))
		      max-allowed-particle-step)
		     (conc-int-is-min
		      (when *count-error-step-change* (push *real-time* *conc-int-ERROR-STEP-CHANGES*))
		      (when *debug-conc-int-error-step-change*
			(format t " real-time: ~a PICK-TIME-STEP limited by conc-int error~%" *real-time*))
		      max-allowed-conc-int-step)
		     (t (when *count-error-step-change* (push *real-time* *VOLTAGE-ERROR-STEP-CHANGES*))
			(when *debug-voltage-error-step-change*
			  (format t " real-time: ~a PICK-TIME-STEP limited by voltage error~%" *real-time*))
			max-allowed-voltage-step))))
	 (overall-max-allowed-mrt-step
	  (the fn (if *element-time-step-maximum* (min (the fn *element-time-step-maximum*) *max-step*) *max-step*))))
    ;; (format t "max-allowed-step ~A, ~A~%" max-allowed-step (* *pick-time-step-fudge* max-allowed-step))
	    
    (if (= -1 max-allowed-step)
	overall-max-allowed-mrt-step
	(let ((max-allowed-mrt-step (truncate (/ (* (the sf *pick-time-step-fudge*) max-allowed-step)
						 *mrt*))))
	  ;; (format t "max-allowed-mrt-step ~A, trun arg ~A~%" max-allowed-mrt-step
	  ;; (/ (* *pick-time-step-fudge* max-allowed-step) *mrt*))
	  (min overall-max-allowed-mrt-step max-allowed-mrt-step)))))


(defmacro next-breakpoint ()
  `(the fn (car *MRT-BREAKPOINT-LIST*)))

;; If *USE-TIME-LIST* is T, PICK-TIME-STEP is *not* used, and the time step is taken from a list of
;; the last simulation's time steps.
; (proclaim '(notinline pick-time-step))
(defun pick-time-step (current-time)
  (declare (optimize (safety 0) (speed 3) (space 0))
	   (fixnum current-time))
  (let ((temp-step
	 ;; In units of *mrt*. If we are here due to a break point, time step is given by *min-step*. 
	 (if (= current-time (next-breakpoint)) *min-step* (get-max-allowed-mrt-step))))
    (declare (fixnum temp-step))

    ;; Advance *MRT-BREAKPOINT-LIST* over current time if necessary.
    (loop until (or (null *MRT-BREAKPOINT-LIST*) (< current-time (next-breakpoint)))
	  do (setf *MRT-BREAKPOINT-LIST* (cdr *MRT-BREAKPOINT-LIST*)))
    
    ;; Make sure that we're not about to pass a break point.
    (if (> (+ current-time temp-step) (next-breakpoint))

	(setf temp-step (- (next-breakpoint) current-time)) ; We did, adjust time step.

	;; If there's another break point coming within temp-step + *min-step*, reduce temp-step
	;; appropriatly so that we hit it next time.
	(when (> (+ current-time temp-step *min-step*) (next-breakpoint))
	  (setf temp-step (- (next-breakpoint) (+ current-time *min-step*)))))

    (when *debug-time-trace* (format t "LTE puts next step: ~6f~%" temp-step))
    ;; Just to be sure, bound temp-step again (perhaps this could be useful if there are two
    ;; breakpoints that are less than *min-step* apart?).
    (setf *time-step* (the fn (max (the fn (min temp-step *max-step*)) *min-step*)))
    nil))


(defun plot-error-step-rasters (&key (include-time-steps t) include-only-used-time-steps
				     font event-height
				     include-particle-steps-<-min-step
				      title)
  "When LTE adaptive time step used, generates a raster plot of simulation time points [when
INCLUDE-TIME-STEPS, default NIL] and those times in which various LTE criteria set the time step,
depending on the values of *CALCULATE-PARTICLE-ERROR* and *CALCULATE-CONC-INT-ERROR*. When
INCLUDE-ONLY-USED-TIME-STEPS [default NIL] only those LTE time steps that are finally used will be
plottted."
  (unless *use-fixed-step*
    (raster-plots :event-data-lists
		  (no-nils (list
			    (and include-time-steps *sim-reverse-time-list*)
			    (if include-only-used-time-steps
				(intersection *sim-reverse-time-list* *VOLTAGE-ERROR-STEP-CHANGES*)
				*VOLTAGE-ERROR-STEP-CHANGES*) 
			    (and *calculate-particle-error*
				 (if include-only-used-time-steps
				     (intersection *sim-reverse-time-list* *particle-ERROR-STEP-CHANGES*)
				     *particle-ERROR-STEP-CHANGES*))
			    (and *calculate-particle-error*
				 include-particle-steps-<-min-step
				 (> (length *PARTICLE-ERROR-STEP-LESS-THAN-MIN-STEP*) 0)
				 *PARTICLE-ERROR-STEP-LESS-THAN-MIN-STEP*)
			    
			    (and *calculate-conc-int-error*
				 (if include-only-used-time-steps
				     (intersection *sim-reverse-time-list* *conc-int-ERROR-STEP-CHANGES*)
				     *conc-int-ERROR-STEP-CHANGES*))))
		  :font font
		  :event-height event-height
		  :event-element-labels
		  (no-nils (list (and include-time-steps  "Simulation Time Points")
				 "Voltage Error Step Changes"
				 (and *calculate-particle-error* "Particle Error Step Changes")
				 (and *calculate-particle-error*
				      include-particle-steps-<-min-step
				      (> (length *PARTICLE-ERROR-STEP-LESS-THAN-MIN-STEP*) 0)
				      "Particle Error Step < *MIN-STEP*")
				 (and *calculate-conc-int-error* 
				      "Conc-Int Error Step Changes")))
		  :title (or title (format nil "~A: LTE Error Step Changes" *simulation-name*)))))

;;;;;;;;;;; old

#|
(defun pick-time-step (lte-ratio current-time)
  (declare (optimize (safety 0) (speed 3) (space 0))
	   (fixnum current-time)
	   (single-float lte-ratio))
  (let ((temp-step (max
		    *min-step*
		    (if (= lte-ratio 0.0) ; This really means 0 error. 
			*max-step*
			;; Limit according to *ELEMENT-TIME-STEP-MAXIMUM*.
			(min
			 (the fn (if *element-time-step-maximum*
				     (min (the fn *element-time-step-maximum*) *max-step*)
				     *max-step*))
			 (the integer (truncate
				       (* *time-step* (the sf *pick-time-step-fudge*) (the sf (sqrt lte-ratio))))))))))
    (declare (fixnum temp-step))
    
    ;; If we are here due to a break point, reduce the time step.
    (when (= current-time (the fn (car *MRT-BREAKPOINT-LIST*))) (setf temp-step *min-step*))
    
    ;; Advance *MRT-BREAKPOINT-LIST* over current time if necessary.
    (do ()
	((or (null *MRT-BREAKPOINT-LIST*)
	     (< current-time (the fn (car *MRT-BREAKPOINT-LIST*)))))
      (setf *MRT-BREAKPOINT-LIST* (cdr *MRT-BREAKPOINT-LIST*)))
    
    ;; Make sure that we're not about to pass a break point.
    (if (> (+ current-time temp-step) (the fn (car *MRT-BREAKPOINT-LIST*)))
	;; We did, adjust time step.
	(setf temp-step (- (the fn (car *MRT-BREAKPOINT-LIST*)) current-time))

	;; If there's another break point coming within temp-step + *min-step*, reduce temp-step
	;; appropriatly so that we hit it next time.
	(when (> (the fn (+ current-time (the fn (+ temp-step *min-step*))))
		 (the fn (car *MRT-BREAKPOINT-LIST*)))

	  (setf temp-step (the fn (+ (the fn (car *MRT-BREAKPOINT-LIST*))
				     (- current-time)
				     (- *min-step*))))))

    (if *debug-time-trace* (format t "voltage lte puts next step: ~6f~%" temp-step))
    
    ;; If we care about particles, then PARTICLE-ERROR-OK will have adjusted
    ;; *PARTICLE-ERROR-MAX-TIME-STEP* to the maximum allowed by the particle error.
    (when *debug-particle-error-step-change*
      (format t " going into picktimestep *PARTICLE-ERROR-MAX-TIME-STEP*: ~a ~%" *PARTICLE-ERROR-MAX-TIME-STEP*))

    (when (and *calculate-particle-error* (> *particle-error-max-time-step* 0))
      (when (< *particle-error-max-time-step* *min-step*) (setq *particle-error-max-time-step* *min-step*))
      (when (> temp-step *particle-error-max-time-step*)
	(when (and *debug-particle-error-step-change* (> *particle-error-max-time-step* *min-step*))
	  (format t " real-time: ~a PICK-TIME-STEP limited by *PARTICLE-ERROR-MAX-TIME-STEP*: " (*t[n+1]*))
	  (format t "time-step going from ~a to ~a~%" *time-step* *particle-error-max-time-step*))
	(setf temp-step *particle-error-max-time-step*)))

    ;; If we care about conc-ints, then CONC-INT-ERROR-OK will have adjusted
    ;; *CONC-INT-ERROR-MAX-TIME-STEP* to the maximum allowed by the conc-int error.

    (when (and *calculate-conc-int-error* (> *conc-int-error-max-time-step* 0))
      (when (< *conc-int-error-max-time-step* *min-step*) (setq *conc-int-error-max-time-step* *min-step*))
      (when (> temp-step *conc-int-error-max-time-step*)
	(when (and *debug-conc-int-error-step-change* (> *conc-int-error-max-time-step* *min-step*))
	  (format t " real-time: ~a PICK-TIME-STEP limited by *CONC-INT-ERROR-MAX-TIME-STEP*: " (*t[n+1]*))
	  (format t "time-step going from ~a to ~a~%" *time-step* *conc-int-error-max-time-step*))
	(setf temp-step *conc-int-error-max-time-step*)))

    (when (and (= current-time *sim-time-n*)
	       (= *time-step* temp-step))
      (setq temp-step (round (/ temp-step 2)))
					; (sim-error (format nil "Stuck backing up at ~Ams!" *real-time*))
      )

        
    ;; Just to be sure, bound temp-step again (perhaps this could be useful if there are two
    ;; breakpoints that are less than *min-step* apart?).
    (setf *time-step* (the fn (max (the fn (min temp-step *max-step*)) *min-step*)))

    ;; Some debugging and bookeeping.
    (when *debug-time-trace* (format t "picktimestep puts next step: ~6f~%" temp-step))
    (when *count-error-step-change*
      (cond ((and *element-time-step-maximum* (= *time-step* *element-time-step-maximum*)))
	    ((and *calculate-particle-error* (= *time-step* *particle-error-max-time-step*))
	     (push *real-time* *particle-ERROR-STEP-CHANGES*))
	    ((and *calculate-conc-int-error* (= *time-step* *conc-int-error-max-time-step*))
	     (push *real-time* *conc-int-ERROR-STEP-CHANGES*))
	    (t (push *real-time* *VOLTAGE-ERROR-STEP-CHANGES*))))
    nil))
|#






#|
(defun calc-lte-ratio ()
  (declare (optimize (safety 0) (debug 0) (speed 3) (space 0)))
  (if *use-fixed-step*
      0.0				; Dummy float value to return.
      (let ((max-dv-diff-neg 0.0d0)
	    (max-dv-diff-pos 0.0d0))
	(declare (double-float max-dv-diff-neg max-dv-diff-pos)
		 (values single-float))
	(dotimes (index (the fn *NODE-W/ELEMENTS-ARRAY-LENGTH*))
	  (declare (fixnum index))
	  (let ((nd (aref (the (simple-array node) *NODE-W/ELEMENTS-ARRAY*) index)))
	    ;; GET-NODE-DVDT returns
	    ;;        v(t[n+1]) - v(t[n])
	    ;;        -------------------
	    ;;            *delta-t[n]*
	    (setf (node-dvdt-n nd) (get-node-dvdt nd))
	    (let ((dv-diff (- (node-dvdt-n nd) (node-dvdt-n-1 nd))))
	      (cond ((< dv-diff max-dv-diff-neg) (setf max-dv-diff-neg dv-diff))
		    ((> dv-diff max-dv-diff-pos) (setf max-dv-diff-pos dv-diff))))))
	
	(let* ((neg-max-dv-diff-neg (- max-dv-diff-neg))
	       (max-dv-diff (if (> neg-max-dv-diff-neg max-dv-diff-pos)	neg-max-dv-diff-neg max-dv-diff-pos)))
	  (declare (double-float neg-max-dv-diff-neg max-dv-diff))
	  (if (= 0.0d0 max-dv-diff)
	      ;; Here, the zero means that the error is zero, although that would actually
	      ;; cause the returned ratio to be infinite. 
	      0.0			
	       
	      ;; Return ratio of *ABSOLUTE-VOLTAGE-ERROR* / estimated LTE (linear truncation error).
	      (let ((lte-voltage (* (*delta-t[n]-squared*)
				    (/ max-dv-diff
				       ;; 2 * [(t[n+1] - t[n]) + (t[n] - t[n-1])]
				       ;; = 2 * (t[n+1] - t[n-1])
				       (* 2.0 (+ (*delta-t[n]*) (*delta-t[n-1]*)))))))
		(s-flt (the df (/ *ABSOLUTE-VOLTAGE-ERROR* lte-voltage)))))))))
|#

