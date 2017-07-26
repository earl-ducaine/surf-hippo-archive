;; -*- mode: lisp; Syntax: Common-lisp; package: #-parallel surf #+parallel *surf; base: 10;  -*-
;;; (c) Copyright 1985, Don Webber, Thinking Machines Corporation, Inc.
;;; alterations (c) Copyright 1990, Lyle Borg-Graham, MIT Center for Biological Information Processing
;;; File creation date: 6/11/85 17:15:58
;
; The main iteration control
;


#-parallel
(in-package "SURF-HIPPO"
	    :use '("COMMON-LISP-USER" "COMMON-LISP")
	    :nicknames '("SURF"))
#+parallel 
(in-package "*SURF")



(defun do-time-control ()
  "Controls the main time loop. Returns t if not successful."
  (format t "~%Starting transient solution~%")
  (initialize-integration)
  (save-data)				;save the initial time point
  (do ()
      ((>= sim-time-n+1 stop-time))
    (advance-all-nodes)
    (do ((lte-ok nil) 
	 (converged *use-hines*)	;Hines method always converges.
	 (lte-ratio 1.0))
	(lte-ok)
      (declare (single-float lte-ratio))
      (if *debug-time-trace*
	  (format t "Time ~4f step ~4f" (* sim-time-n mrt) (* time-step mrt)))
      (setf
       sim-time-n+1 (+ sim-time-n time-step) 	  
       alpha (/ 2.0 (* time-step mrt)))	;
      (if *use-hines*
	  (hines-step)
	  (progn
	    (initialize-relax)		;Set the sources, set node old rhs, predict new voltage
	    (setf converged (relax-until-converged)))) ;Relax until currents and voltages converged, or iter-count exceeded.
      (if converged
	  (progn			; converged
	    (setf
	     lte-ratio (calc-lte))
	    (if *debug-time-trace* (format t " lte-ratio ~4f" lte-ratio))
	    (if (>= lte-ratio 1.0)
		(setf			; converged and OK
		 lte-ok t
		 time-step (pick-time-step lte-ratio time-step sim-time-n+1))
		(progn			; converged but not OK
					; lte-ok nil
		  (if *debug-time-trace* (format t " ... backing up"))
		  (setf time-step (pick-time-step lte-ratio time-step sim-time-n))
		  ))
	    (if *debug-time-trace* (format t "~%")))
	  (progn			; not converged
	    (if *debug-time-trace* (format t " no convergence, backing up~%"))
	    (setf
	     time-step (truncate (/ time-step 4)))
	    (check-time-step)
	    )))
    (save-data)				; This time step is successful, save 
    (incf *total-num-time-points*)	;  out the data
    ;;;;;(if (= (remainder *total-num-time-points* 10) 0) (process-allow-schedule))
    ))


;(defvar *hines-step-min-max-dv .01)
;(defvar *hines-max-step .2)
;(defvar *hines-step-max-max-dv 1)
;(defvar *hines-min-step .01)
;(defvar *hines-step-coefficient 1)
;
;(defun pick-hines-step ()
;  (truncate (/
;	      (let ((max-delta-v 0) temp-dv)
;		(dolist (core-node *core-node-list*)
;		  (setq temp-dv (abs (- (core-node-voltage-n+1 core-node)(core-node-voltage-n core-node))))
;		  (if (< max-delta-v temp-dv) (setq  max-delta-v temp-dv)))
;		(cond ((< max-delta-v *hines-step-min-max-dv) (* time-step mrt))
;		      ((> max-delta-v *hines-step-max-max-dv) *hines-min-step)
;		    (t (* 1.5 time-step))));(/ *hines-step-coefficient max-delta-v))))
;	      mrt)))



(defun hines-step ()
;;Note that for the Hines method, "delta-v" structure slot values are actually V(t + dt/2).
  (setq delta-back (* mrt 0.5 last-time-step)
	delta-forward (* mrt 0.5 time-step))
  (setq half-delta-for-back (* 0.5 (- delta-forward delta-back))
      sum-delta-for-back (+ delta-forward delta-back))
  ;;Set sources.
  (initialize-hines-step)			
  ;;Clear tri-diag matrix, and initialize all the accumulator fields, including (core-node-jacobian nd),
  ;;(core-node-charge nd), and (core-node-current nd).
  (init-all-nodes)
; (print-node-voltages)
  ;;Run all the eval routines for the circuit elements, each of which accumulates the jacobian, charge, and
  ;;current entries of the approriate node, and accumulates the near off-diagonal entries of the tri-diag matrix, as
  ;;appropriate.
  (eval-all-elements)				;** so far only eval-seg modified **
  ;;Solve both implicit and explicit Hines step:
  ;;Evaluate all nodes: Set the RHS for the core-nodes, solve the matrix, update the voltage estimate with
  ;;the new delta-V's.
  (eval-all-nodes 0)
  (if *debug-all-iterations* (print-node-voltages))
  )

(defun check-time-step ()
  "Checks the time step to make sure that it has not gotten too small."
  (if (<= time-step min-step)
      (sim-error "Time step too small in transient analysis at time ~a~%"
			 (* sim-time-n mrt))))

#-parallel
;;; Calculates the LTE and returns the LTE ratio. For Hines method, returns ratio of allowed error divided by
;;; estimate of max error. 

(defun calc-lte ()
  (let ((max-voltage 0.0) (max-error FUZZ) (temp 0.0)
	(delta-t-n+1 (- sim-time-n+1 sim-time-n))
	(delta-t-n (- sim-time-n sim-time-n-1)))
    (declare (single-float temp max-voltage max-error))
    (declare (fixnum delta-t-n+1 delta-t-n))
    (dolist (nd *core-node-list*)		; find the max voltage and error
      (if *use-hines*
 	  (progn
	    ;; Estimate O(n^2) error using V''(t + dt/2) = 0.5 [V'(t + dt) - V'(t)]
	    ;; Note: 0.5 factor handled at end of function.
	    (setf temp (abs (- (/ (- (core-node-voltage-n+1 nd) (core-node-voltage-n nd))
				  delta-t-n+1)
			       (/ (- (core-node-voltage-n nd) (core-node-voltage-n-1 nd))
				  delta-t-n))))
	    ;;Amplify error for particle nodes
;	    (if (not (core-node-hines-circuit-node nd)) (setq temp (* *part-error-factor temp)))	
	    (if (> temp max-error) (setf max-error temp)))
	  (progn
	    (setf temp (*  0.333 (+ (abs (core-node-voltage-n+1 nd))
				    (abs (core-node-voltage-n   nd))
				    (abs (core-node-voltage-n-1 nd)))))
	    (if (> temp max-voltage) (setf max-voltage temp))
	    (setf temp (abs (- (core-node-voltage-n+1 nd) (core-node-predictor nd))))
	    (if (> temp max-error) (setf max-error temp)))))
    (if (not *use-hines*)
	(if (> max-error FUZZ)
	    (progn
	      (setf
		temp (/ (* (+ (* lterel max-voltage) lteabs)
			   (- sim-time-n+1 sim-time-n-2)
			   2.0)
			(* (- sim-time-n+1 sim-time-n) max-error)))
	      (if (> temp (float step-3-ratio))
		  (setf temp (float step-3-ratio)))
	      (if (< temp (/ 1.0 down-step-ratio)) (setf temp (/ 1.0 down-step-ratio))))
	    (setf temp (float step-3-ratio))))
;    (format t "max-error= ~a, max-dv= ~a ~%" max-error max-voltage)
    (if *use-hines*
	(/ *hines-max-error
	   ;;0.25 factor from 0.5 terms in V'' estimate and Taylor expansion.
	   (* 0.25 max-error (/ (* delta-t-n+1 delta-t-n+1)	
				(- sim-time-n+1 sim-time-n-1))))
	temp)))


#+parallel
(defun calc-lte ()
  "Calculates the LTE and returns the LTE ratio."
  (let ((max-voltage 0.0)
	(max-error 0.0)
	(temp))
    (*select-type (core-node)			; find the max voltage and error
      (*when (not!! core-node-is-source)
	(setf
	  max-voltage (* 0.333
			 (*max (+!! (float-abs!! core-node-voltage-n+1)
				    (float-abs!! core-node-voltage-n)
				    (float-abs!! core-node-voltage-n-1))))
	  max-error (*max (float-abs!! (-!! core-node-voltage-n+1 core-node-predictor))))))
    (if (> max-error FUZZ)
	(progn
	  (setf
	    temp (/ (* (+ (* lterel max-voltage)
			  lteabs)
		       (- sim-time-n+1 sim-time-n-2)
		       2.0)
		    (* (- sim-time-n+1 sim-time-n)
		       max-error)))
	  (if (> temp step-3-ratio)
	      (setf temp step-3-ratio))
	  (if (< temp (/ 1.0 down-step-ratio))
	      (setf temp (/ 1.0 down-step-ratio))))
	(setf temp step-3-ratio))
    temp))

(defun pick-time-step (lte-ratio prev-step current-time)
  (declare (fixnum current-time prev-step))
  (declare (single-float lte-ratio))
  "Picks a new time step based on lte-ratio."
  (let ((new-ratio (expt lte-ratio 0.3333))
	(temp-step 0.0))
    (declare (single-float new-ratio))
    (if *use-hines*
	(setf temp-step (* 0.7 (expt (* prev-step prev-step lte-ratio) 0.5)))
	(if (< new-ratio 1.0)
	    (let ((temp-temp-step
		   (/ (float prev-step) down-step-ratio)))
	      (declare (single-float temp-temp-step))
	      (check-time-step)		; if time step is already min-time-step, error
	      (setf temp-step (min (* 0.7 new-ratio prev-step) temp-temp-step)))
	    (if (> new-ratio 1.2)
		(setf
		 temp-step (+ prev-step
			      (* 0.9
				 prev-step
				 (- new-ratio 1.2))))
		(setf temp-step (float prev-step)))))
    ;; Bound the step because otherwise the next part of the algorithm can get confused.
    ;; Specifically, if temp-step is greater then max-step and also greater then
    ;; a breakpoint, while max-step is less then the breakpoint, we could be left
    ;; just before the breakpoint, which will force us to take a very small step.
    (if (> temp-step max-step) (setf temp-step (float max-step)))
    (if (< temp-step min-step) (setf temp-step (float min-step)))
    ;;    (format t "~%First try step = ~a at time ~a~%" temp-step current-time)
    ;;if we are here due to a break point, reduce the time step
    (if (>= current-time (car *break-point-list*))
	(progn
	  (setf temp-step (/ temp-step down-step-ratio)) 
	  ;;	  (format t "Reduced after breakpoint to ~a~%"  temp-step)
	  ))
    ;; advance *break-point-list* over the current time. if we've already passed a break point, too bad
    (do ()
	((< current-time (car *break-point-list*)))
      (setf *break-point-list* (cdr *break-point-list*)))
    ;; make sure that we're not about to pass a break point
    (if (> (+ current-time temp-step) (car *break-point-list*))	
	(progn				; we did, adjust time step
	  (setf temp-step (- (car *break-point-list*) current-time))
					;	  (format t "Reduced to step on breakpoint to ~a~%"  temp-step)
	  )
	(if (> (+ current-time (* temp-step 2)) (car *break-point-list*))
					; If there's another break point coming within 2 time steps, go only
					; half way there.
	    (progn
	      (setf temp-step (/ (- (car *break-point-list*) current-time) 2))
	      ;;	      (format t "Reduced to step half way to break to ~a~%" temp-step)
	      )))
    ;; check the bounds again.
    (cond
      ((> temp-step (float max-step))
       max-step)
      ((< temp-step (float min-step))
       min-step)
      (t
       (truncate temp-step)))))


(defun initialize-integration ()
  "Set up all the variables for the integration algorithm."
  (if (not *dc-solution-computed*)		; since we don't do DC solution, zero voltage-n+1 for all nodes	
      #-parallel (dolist (nd *core-node-list*) (zero-v-n+1 nd))
      #+parallel (*select-type (core-node) (zero-v-n+1)))
  (setf time-step (min max-step (max min-step (* min-step 111))))	; initial step, should be automatic
  (setf sim-time-n+1 start-time			
	sim-time-n   (- sim-time-n+1 time-step)
	sim-time-n-1 (- sim-time-n   time-step)
	sim-time-n-2 (- sim-time-n-1 time-step))
  (setq *real-time (float start-time)
	delta-back (* mrt 0.5 last-time-step)
	delta-forward (* mrt 0.5 time-step))
  (setq half-delta-for-back (* 0.5 (- delta-forward delta-back))
	sum-delta-for-back (+ delta-forward delta-back))
  (if (not *dc-solution-computed*)
      (dolist (pair *init-value-list*)		; set the voltages that we do know
	(if (typep (car pair) 'node)
	    (set-node-voltage (car pair) (cdr pair))
	    (set-node-voltage (gethash (car pair) node-hash-table) (cdr pair)))))
  (set-pwl-sources)
  (set-pwl-isources)
  #-parallel (dolist (nd *core-node-list*) (start-node nd))	; set appropriatly the "past" times and voltages
  #+parallel (*select-type (core-node) (start-node))
  #-parallel
  (progn (init-all-nodes)
	 (eval-all-elements))
  #+parallel
  (progn
;   !!! I don't know why this was here. DMW !!!
;    (setf sim-time-n+1 (+ sim-time-n time-step)
;          alpha (/ 2.0 (* time-step mrt)))
;    (initialize-relax)
    (init-all-nodes)				; does a send-all-voltages
    (send-voltages)
    (eval-all-elements)
    (return-current)
    (return-charge)
    (return-conductance)))

(defun initialize-hines-step ()
  (set-pwl-sources)
  (set-pwl-isources))

(defun initialize-relax ()
  "To begin a new relaxation iteration. To be called once before each
   relaxation iteration starts."
  (set-pwl-sources)
  (set-pwl-isources)
  #-parallel (dolist (nd *core-node-list*) (node-initialize-relax nd))
  #+parallel (*select-type (core-node) (*when (not!! core-node-is-source) (node-initialize-relax))))

(defun relax-until-converged ()
  "Do relaxation iterations until the voltages and currents converge.
   Returns t if converged."
					; loop until converged
  (do ((iter-count 1 (1+ iter-count))
       (converged nil))
      ((or converged (> iter-count *max-num-relax-iterations*))
       (if *debug-time-trace* (format t " iter ~a" (1- iter-count)))
       (setf *total-num-iterations* (+ *total-num-iterations* (1- iter-count)))
       converged)
					; returns t if iteration converged, nil if iteration count exceeded.
    (declare (fixnum iter-count))
    (relax-iteration iter-count)
					; check convergence
    (setf converged (check-all-convergence))
    ))



#-parallel
(defun relax-iteration (iter-count)
  (declare (fixnum iter-count))
  "The inner relaxation loop. Implicitly does only one NR loop."
  ;;Clear tri-diag matrix, and initialize all the accumulator fields, including (core-node-jacobian nd),
  ;;(core-node-charge nd), and (core-node-current nd).
  (init-all-nodes)
  ;;Run all the eval routines for the circuit elements, each of which accumulates the jacobian, charge, and
  ;;current entries of the approriate node, and accumulates the near off-diagonal entries of the tri-diag matrix, as
  ;;appropriate.
  (eval-all-elements)
;;Evaluate all nodes: Set the RHS for the core-nodes, solve the matrix, update the voltage estimate with
;;the new delta-V's, using over/under-relaxation if appropriate.
  (eval-all-nodes iter-count)
  (if *debug-all-iterations* (print-node-voltages))
  )

#+parallel
(defun relax-iteration (iter-count)
  "The inner relaxation loop. Implicitly does only one NR loop."
  (init-all-nodes)
  (send-voltages)
  (eval-all-elements)
  (return-current)
  (return-charge)
  (return-conductance)
  (eval-all-nodes iter-count)
  (if *debug-all-iterations*  
      (print-node-voltages))
  )

#-parallel
(defun eval-all-elements ()
  "Evaluates all the elements."
  (dolist (mod *models*)
    (dolist (inst (model-template-instances mod))
      (dolist (elt (model-instance-elements inst))
	(funcall (model-template-eval-routine mod) elt))))
)

#+parallel
(defun eval-all-elements ()
  "Evaluates all the elements."
  (dolist (mod *models*)
    (funcall (model-template-eval-routine mod))))

#-parallel
(defun init-all-nodes ()
  "Initializes all nodes."
  ;;clear the matrix
  (if (or *use-hines* *use-tridiagonal*)
      (dotimes (i *num-unknowns*)
	(setf (aref *diag* i) zero)
	(if (or (not *use-hines*) (and *use-hines* (= *real-time 0)))
	    (setf (aref *lower-diag* i) zero
		  (aref *upper-diag* i) zero))))
  (dolist (nd *core-node-list*)
    (init-node nd))
  )

#+parallel
(defun init-all-nodes ()
  "Initializes all nodes."
  (progn
    (*set fanout-value (!! zero))
    (*select-type (core-node)
		  (init-node)))
  )

(defun advance-all-nodes ()
  "Advances the system one step in time."
  (setf
   sim-time-n-2 sim-time-n-1
   sim-time-n-1 sim-time-n
   sim-time-n   sim-time-n+1)
  (setq last-time-step time-step)
  (let ((real-time (* mrt sim-time-n+1)))
    (if (and (/= (floor *real-time) (floor real-time))
	     (= 0 (mod (floor real-time)  *display-time-increment)))
	(display-time))
    (setq  *real-time real-time))
  (setq *integer-time (floor *real-time)
	*fractional-time (rem *real-time 1))
  #-parallel (dolist (nd *core-node-list*)
	       (node-advance-time nd))
  #+parallel (*select-type (core-node) (node-advance-time))
  )


#-parallel
(defun eval-all-nodes (iter-count)
  (declare (fixnum iter-count))
  "Evaluates all nodes: Sets the RHS for the core-nodes, solves the matrix, updates the voltage estimate with
  the new delta-V's, using over/under-relaxation if appropriate."
;;Note that for the Hines method, "delta-v" is actually V(t + dt/2).
   (dolist (nd *core-node-list*)
    (set-rhs nd))
  (if (and *print-matrix* (or *use-hines* *use-tridiagonal*))
      (progn
	(format t "~%The matrix is:~%")
	(format t "index :  lower       diag        upper       rhs~%")
	(dotimes (i *num-unknowns*)
	  (format t "~5d : ~6f~19t~6f~32t~6f~42t~6f~%" i (aref *lower-diag* i)
		  (aref *diag* i) (aref *upper-diag* i) (aref *rhs* i)))
	(format t "~%")))
  (solve-matrix)
  (if (and *print-matrix* (or *use-hines* *use-tridiagonal*))
      (progn
	(format t "~%The solution is:~%")
	(format t "index       delta-v~%")
	(dotimes (i *num-unknowns*)
	  (format t "~5d ~6f~%" i (aref *delta-v* i)))
	(format t "~%")))
  (dolist (nd *core-node-list*)			;For Hines, transfers *delta-v*->core-node-delta-v, and does
    (eval-node nd iter-count))			;explicit half step. 
)

#+parallel
(defun eval-all-nodes (iter-count)
  "Evaluates all nodes."
  (*select-type (core-node)
		(eval-node iter-count))
  )

#-parallel
(defun check-all-convergence ()
  "Checks the convergence of all the nodes."
  (catch 'not-converged
    (dolist (nd *core-node-list*)
      (if (not (node-check-convergence nd))
	  (throw 'not-converged nil)))
    t))

#+parallel
(defun check-all-convergence ()
  "Checks the convergence of all the nodes."
  (*select-type (core-node)
    (*when (not!! core-node-is-source)
      (node-check-convergence)
      (*and core-node-converged))))

(defun queue-time (time)
  "Puts 'time' on the queue of break points so that the simulation can
   be sure to step there."
  (push time *break-point-list*))

(defun fix-break-point-list ()
  "Sorts and removes extra information from '*break-point-list*'"
;  (push user-start-time *break-point-list*)
  (push user-stop-time *break-point-list*)
;  (if *include-synapses
;      (dotimes (i (round user-stop-time))
;	(push (+ i user-start-time ) *break-point-list*)))
  (push (+ user-stop-time user-stop-time) *break-point-list*)	; just so something is on the
						;  list after stop-time
  (setf
    *break-point-list* (mapcar #'(lambda (x) (truncate (/ x mrt))) *break-point-list*)
    *break-point-list* #+lambda *break-point-list* #-lambda (delete-duplicates *break-point-list*)
    *break-point-list* (sort *break-point-list* #'<)
    *break-point-list* (delete start-time *break-point-list*)
    ))

(defun dc-solution ()
  "Does a DC solution by iteration."
  (format t "~%Starting DC solution~%")

  (setf sim-time-n+1 start-time)
  (setq *real-time (float start-time))
  
  ; zero voltage-n+1 for all nodes
  #-parallel (dolist (nd *core-node-list*)
	       (zero-v-n+1 nd))
  #+parallel (*select-type (core-node)
	       (zero-v-n+1))
  
  ; set the voltages that we do know
  (dolist (pair *init-value-list*)		;sets voltage-n+1, in Hines then voltage-n as well
    (if (typep (car pair) 'node)
	(set-node-voltage (car pair) (cdr pair))
	(set-node-voltage (gethash (car pair) node-hash-table) (cdr pair))))
  (let ((converged nil)
	(save-num-nr *max-num-relax-iterations*))
    (setf *max-num-relax-iterations* 100)
    (setf alpha 0.0)
    #-parallel (dolist (nd *core-node-list*)
		 (setf (core-node-old-rhs nd) zero))
    #+parallel (*select-type (core-node) (*set core-node-old-rhs (!! zero)))
    (set-pwl-sources)
    (set-pwl-isources)
    (if (not *use-hines*)    (setf converged (relax-until-converged)))	;have to fix this for Hines
    (setf *max-num-relax-iterations* save-num-nr)
    (if converged
	(format t "converged~%~%")
	(format t "No DC convergence~%~%"))
    )
  (setf *dc-solution-computed* t)
)

(defun pseudo-transient ()
  "Do a DC solution by Pseudo transient analysis."
  (format t "~%Starting pseudo transient solution~%")
  (initialize-integration)

  (do ((num-pseudo-steps 0)
       (pseudo-time 0))
      ((or (and (reached-steady-state) (> num-pseudo-steps 0))
	   (>= num-pseudo-steps 1000)))
    (advance-all-nodes)
    (do ((converged nil))
	(converged)
      (if *debug-dc*
	  (format t "Time ~4f step ~4f" (* sim-time-n mrt) (* time-step mrt)))
;	  (format t "Time ~4f >> ~a << step ~4f >> ~a <<" (* sim-time-n mrt) sim-time-n
;		     (* time-step mrt) time-step)
      (setf
	pseudo-time (+ pseudo-time time-step)
	alpha (/ 2.0 (* time-step mrt)))
      #-parallel (dolist (nd *core-node-list*)
		   (setf (core-node-old-rhs nd) (- (core-node-prev-current nd)
						   (* alpha (core-node-prev-charge nd)))))
      #+parallel (*select-type (core-node)
		   (*when (not!! core-node-is-source)
			  (*set core-node-converged nil!!)
			  (*set core-node-old-rhs (-!! core-node-prev-current
						       (*!! (!! alpha)
							    core-node-prev-charge)))))
      (setf
	converged (relax-until-converged))
      (if converged
	  (progn				; converged
	    (setf time-step (* time-step 2))
	    (if *debug-dc* (format t "~%")))
	  (progn
	    (setf time-step (/ time-step 4))
	    (if *debug-dc* (format t " no convergence, backing up~%"))))
      )
    (incf num-pseudo-steps)
    )
  (setf *dc-solution-computed* t))

#-parallel
(defun reached-steady-state ()
  "A test to see if the pseudo-transient solution has reached a steady state."
  (catch 'no-steady-state
    (dolist (nd *core-node-list*)
      (if (not (node-check-steady-state nd))
	  (throw 'no-steady-state nil)))
    t))

#+parallel
(defun reached-steady-state ()
  "A test to see if the pseudo-transient solution has reached a steady state."
  (*select-type (core-node)
    (*when (not!! core-node-is-source)
      (node-check-steady-state)
      ;; use the "core-node-converged" field of the node, it's state is not needed right now
      (*and core-node-converged))))

