;;; -*- Mode: Lisp; Syntax: Common-lisp; Package: #-PARALLEL SURF #+PARALLEL *SURF; Base: 10; -*-
;;;  (c) Copyright 1990, Lyle Borg-Graham, MIT Center for Biological Information Processing

;;; This file requires the Symbolics Window Environment and PAO's PLOT package.
;;;
;;; 7/6/92 LBG Now can use Garnet-based plotting under CMUCL. 


#-parallel
(in-package "SURF-HIPPO"
	    :use '("COMMON-LISP-USER" "COMMON-LISP")
	    :nicknames '("SURF"))
#+parallel 
(in-package "*SURF")

#+cmu
(defvars-w-value
    (voltage-pane1 'voltage-pane1)(voltage-pane2 'voltage-pane2)
  (current-pane 'current-pane)
  (particle-pane 'particle-pane)  					      
  )

;;; RETRIEVE-PLOT-DATA This function returns a list of lists, each of
;;; which is of the type referenced in the structure-data-types list,
;;; and comes from the structure name in name-list. This function
;;; allows the order of multiple data plots to be determined by the
;;; order in the original name-lists (e.g. *plot-nodes* or
;;; *plot-channel-currents*).
#+cmu
(defun retrieve-plot-data (name-lists structure-data-types)
  (loop
   for structure-data-type in structure-data-types nconcing
   (loop
    for name-list in name-lists nconcing
    (loop
     for name in name-list nconcing
     (case structure-data-type
       (node-voltage
	(if (gethash name node-hash-table)
	    (list (reverse (node-voltage-data (gethash name node-hash-table))))))
       (isource-current
	(if (gethash name isource-hash-table)
	    (list (reverse (isource-current-data (gethash name isource-hash-table))))))
       (synapse-current
	(if (gethash name synapse-hash-table)
	    (list (reverse (synapse-current-data (gethash name synapse-hash-table))))))
       (synapse-conductance
	(if (gethash name synapse-hash-table)
	    (list (reverse (synapse-conductance-data (gethash name synapse-hash-table))))))
       (channel-current
	(if (gethash name channel-hash-table)
	    (list (reverse (channel-current-data (gethash name channel-hash-table))))))
       (channel-conductance
	(if (gethash name channel-hash-table)
	    (list (reverse (channel-conductance-data (gethash name channel-hash-table))))))
       (particle-state
	(if (gethash name particle-hash-table)
	    (list (reverse (particle-state-data (gethash name particle-hash-table))))))
       (conc-int-concentration-1
	(if (gethash name conc-int-hash-table)
	    (list (reverse (conc-int-concentration-1-data (gethash name conc-int-hash-table))))))
       (conc-int-concentration-2
	(if (gethash name conc-int-hash-table)
	    (list (reverse (conc-int-concentration-2-data (gethash name conc-int-hash-table))))))
       )))))

;;; SURF-PLOTTER 
(defun surf-plotter ()
  (if *plot-soma-nodes*
      (surf-plot
       #+cmu (retrieve-plot-data (list *plot-soma-nodes*) `(node-voltage))
       #-cmu *pane1-data-list
       *plot-soma-nodes* voltage-pane1
       :y-label "mV"
       :y-min *soma-voltage-plot-min :y-max *soma-voltage-plot-max
       :x-min user-start-time :x-max user-stop-time
       :overlay *overlay-simulations)
      #-sun (send voltage-pane1 :clear-window)
      )
  (if *plot-nodes*
      (surf-plot
       #+cmu (retrieve-plot-data (list *plot-nodes*) `(node-voltage))
       #-cmu *pane2-data-list
       *plot-nodes* voltage-pane2
       :y-label "mV"
       :y-min *dendrite-voltage-plot-min :y-max *dendrite-voltage-plot-max
       :x-min user-start-time :x-max user-stop-time
       :overlay *overlay-simulations)
      #-sun (send voltage-pane2 :clear-window)
      )
  (if (or *plot-channel-currents* *plot-synapse-currents*  *plot-isource-currents*)
      (surf-plot
       #+cmu (retrieve-plot-data (list *plot-channel-currents* 
				       *plot-synapse-currents* *plot-isource-currents*)
				 `(channel-current synapse-current isource-current))
       #-cmu *current-pane-data-list
       (concatenate 'list *plot-channel-currents* *plot-synapse-currents*  *plot-isource-currents*)
       current-pane
       :y-label "nA"
       :x-min user-start-time :x-max user-stop-time
       :overlay *overlay-simulations)
      #-sun (send current-pane :clear-window)
      )
  (if *particle-pane-data-list
      (surf-plot
       #+cmu (retrieve-plot-data (list *plot-particles*) `(particle-state))
       #-cmu *particle-pane-data-list
       *plot-particles* particle-pane
       :y-label "Gating Particle State"
       :x-min user-start-time :x-max user-stop-time
       :y-min 0 :y-max 1 :overlay *overlay-simulations)
      (if *conductance-pane-data-list
	  (surf-plot
	   #+cmu (retrieve-plot-data (list *plot-channel-conductances* *plot-synapse-conductances*)
				     `(channel-conductance synapse-conductance))
	   #-cmu *conductance-pane-data-list
	   (concatenate 'list *plot-channel-conductances* *plot-synapse-conductances*)
	   particle-pane
	   :x-min user-start-time :x-max user-stop-time
	   :y-label "uS" :y-min 0 :overlay *overlay-simulations)
	  (if *conc-int-pane-data-list
	      (surf-plot
	       #+cmu (retrieve-plot-data (list *plot-conc-ints*) `(conc-int-concentration-1))
	       #-cmu *conc-int-pane-data-list
	       *plot-conc-ints*
	       particle-pane
	       :x-min user-start-time :x-max user-stop-time
	       :y-label "Concentration" :y-min 0 :overlay *overlay-simulations)
	      #-sun 	      (send particle-pane :clear-window)))
      )
  (if *transformed-plot
      (progn (surf-plot (transform-node-output *pane2-data-list) *pane2-label-list current-pane
			:x-min user-start-time :x-max user-stop-time
			:y-label "mV" :overlay *overlay-simulations) 
	     #-sun	     (send current-pane :set-label "Transformed Voltages")
	     ))
  ;;  (if *use-synapse-events (PLOT-SYNAPSE-BRANCH-EVENTS
  ;;  (get-synapse-types)))
  )



; The following version of transformed-node-output transforms the plot-list into the synaptic innactivation
; of the input.  The parameters are: the time constant of the innactivation
; and the time constant of the reactivation.

(defun SYNAPTIC-INNACTIVATION (plot-list tau1 tau2)
  (without-floating-underflow-traps		
    (let ((transformed-plot-list nil)
	  (times (cadar plot-list))       
	  dt)                             
      (dolist (nodes plot-list)           
	(let* ((concentrations  (car nodes))
	       (input          (car concentrations))      
	       (close          (/ tau1 (+ tau1 (* tau2 input))))      
	       (output         (list (* input close)))
	       (t1             (car times))) 
	      (do ((tt (cdr times) (cdr tt)) (cc (cdr concentrations) (cdr cc))) 
		  ((not tt))
		(setq dt (- (car tt) t1))
		(setq t1 (car tt))
		(setq close (+ close (- (* (- 1 close) (/ dt tau2)) (* close input (/ dt tau1)))))
		(setq input (car cc))
		(setq output (nconc output (list (* input close)))))
	      (setq transformed-plot-list (nconc transformed-plot-list (list (list output times))))))
      transformed-plot-list)))





; The following version of transformed-node-output transforms the plot-list into the synaptic innactivation
;  of the input via second messanger feedforward.  The parameters are: the time constants of activation and innactivation,
; the messenger's degree of cooperativity, and the threshold of innactivation.

(defun SECOND-MESSENGER (plot-list tau1 tau2 power threshold)
  (without-floating-underflow-traps		
    (let ((transformed-plot-list nil)
	  (times (cadar plot-list))       
	  dt)                             
      (dolist (nodes plot-list)           
	(let* ((concentrations  (car nodes))
	       (input          (car concentrations))
	       (inhibitor      (/ (* input tau2) tau1)) 
	       (output         (list (/ input (+ threshold (expt inhibitor power)))))
	       (t1             (car times))) 
	      (do ((tt (cdr times) (cdr tt)) (cc (cdr concentrations) (cdr cc))) 
		  ((not tt))
		(setq dt (- (car tt) t1))
		(setq t1 (car tt))
		(setq inhibitor (+ inhibitor (* dt (- (/ input tau1) (/ inhibitor tau2)))))
		(setq input (car cc))
		(setq output (nconc output (list (/ input (+ threshold (expt inhibitor power)))))))
	      (setq transformed-plot-list (nconc transformed-plot-list (list (list output times))))))
      transformed-plot-list)))





; The following output function transforms the output to an exponential release function of the
; difference from resting potential. The parameters are the voltage-constant and the constant that
; multiplies the output.

(defun EXPONENTIAL-RELEASE (plot-list voltage-constant output-constant)
  (without-floating-underflow-traps                       
    (static-release
      `(function (lambda (x) (* ,output-constant (exp (/ (- x *e-l) (float ,voltage-constant))))))
      plot-list)))




; The following output function transforms the output to a linear release function of the
; difference from resting potential.  

(defun LINEAR-RELEASE (plot-list)
  (without-floating-underflow-traps                       
    (static-release
      `(function (lambda (x) (- x *e-l)))
      plot-list)))



; If the input is above resting potential, then the following function transforms the output to a
; linear release function of the difference from resting potential.  Otherwise, the output is zero.

(defun LINEAR-THRESHOLD-RELEASE (plot-list)
  (without-floating-underflow-traps                       
    (static-release
      `(function (lambda (x) (if (> x *e-l) (- x *e-l) 0))) 
      plot-list)))


;;;This function is a subroutine to be used with various synaptic release transforms.  The parameter transform
;;;is the transform to be performed on the plot-list. 

(defun STATIC-RELEASE (transform plot-list)
  (let ((transformed-plot-list '()))
    (dolist (values plot-list)
      (setq transformed-plot-list
	    (nconc transformed-plot-list (list (list (mapcar (eval transform) (car values)) (cadr values))))))
    transformed-plot-list))



;;; This function chooses the sequences of operations to be applied to the plot-list and set its parameters. 

(defun TRANSFORM-NODE-OUTPUT (plot-list)
  (synaptic-innactivation (exponential-release plot-list 5.0 .1) 1000.0 1000.0))  ;;; voltage-constant, release-constant
                                                                       ;;; innactivation, and reactivation.


;;; The following function extracts the portion of a data-list that lies between time beginning and end.

(defun PARTIAL-DATA  (plot-list beginning end) 
  (let ((transformed-plot-list nil)
	(times (cadar plot-list))
	head-cuts
	tail-cuts)
    (do ((i 0 (+ i 1)) (list times (cdr list)))
	((>= (car list) beginning) (setq head-cuts i) (setq times list))) 
    (do ((i 0 (+ i 1)) (list times (cdr list)))
	((or (not list) (>  (car list) end)) (setq tail-cuts (- (length times) i)) (setq times (butlast times tail-cuts))))
    (dolist (nodes plot-list)
      (let ((voltages  (butlast (nthcdr head-cuts (car nodes)) tail-cuts)))
	(setq transformed-plot-list (nconc transformed-plot-list (list (list voltages times))))))
    transformed-plot-list))


;;; The following function returns the integrals of each of the data sets of the plot-list.

(defun INTEGRATE-DATA (plot-list label-list)
  (without-floating-underflow-traps
   (let ((times (cadar plot-list))
	 integral)
     (do ((plot-list plot-list (cdr plot-list))
	  (label-list label-list (cdr label-list)))
	 ((not plot-list))
       (setq integral 0)
       (do ((lt times (cdr lt)) (lv (caar plot-list) (cdr lv)))
	   ((not (cdr lt))) 
	 (setq integral (+ integral (* (- (car lv) -70) (- (cadr lt) (car lt)))))) 
       (format *output-stream "Node ~a integral =  ~a~%" (car label-list) integral))
     integral)))

					;This is the list of lists format required by PAO's PLOT



;;;; SURF-PLOT
(defun surf-plot (data-lists label-list plot-pane 
			     &key y-min y-max x-min x-max y-label
			     (overlay *overlay-simulations)
			     (plot-symbols t))
;;; For the Garnet implementation, data-lists is simply a list of the individual data lists.
  #+garnet (declare (ignore plot-symbols))
  #+garnet (plot-timed-data data-lists label-list *ordered-time-list*
			    :win plot-pane :overlay overlay
			    :y-min y-min :y-max y-max :y-label (string y-label)
			    :x-max x-max :x-min x-min)
  #-sun (send plot-pane
	:plot "Node Values "	
	data-lists label-list
	:x-label 	"mS"
	:y-label 	y-label
	:y-min y-min :y-max y-max
	:all-solid-lines *plot-voltages-solid :overlay overlay-simulations :leave-window overlay-simulations
	:symbols-p plot-symbols :symbol-size 3)
  )

;	 (t					;This is to plot subsequent traces as distinct ones.
;	 (send plot-pane
;		 :replot			
;		 :new-curves plot-list :new-curve-labels label-list
;		 :all-solid-lines *plot-voltages-solid
;		 :symbols-p *plot-symbols :symbol-size 3)))))

;; ****************************************************

;(defun printoutnodes (name nd)
; (print "")(print name)
;;	(print " This node has the following elements -")
;;	(let ((element-list (node-elements nd)))
;;	 (dolist (element element-list)
;	(let ((core (node-core-nd nd)))
;	 (if core (print (core-node-rhs core))
;	 (print "no core ........")))
;;	(print "")
;)


;;; FIND-TIME-INDEX Returns the array index for the output arrays that corresponds to "target-time".
(defun find-time-index (target-time)
 (let ((time (reverse *time-list*))(t-1 0.0)(t+1 0.0))
 (do ((index 0 (1+ index)))
	(())
 (setq t-1 (nth index time)
	 t+1 (nth (+ 1 index) time))
 (if (and (< target-time t+1)(>= target-time t-1))
	 (return index)))))


;; make-even-time-list
(defun make-even-time-list (list delta-t duration)
  (let ((even-list '())(time-list '()))
    (dotimes (i (round (/ duration delta-t)))
      (setq even-list (nconc even-list
			     (list (nth (find-time-index (* i delta-t)) list)))
	    time-list (nconc time-list (list  (* i delta-t)))))
    (list even-list time-list)))

;;; MAKE-SNAPSHOT Returns a list of two lists, the first corresponding to the elements from each array in the
;;; array-list which correspond to "time", and the second given by the argument "x-list". 
(defun make-snapshot (time array-list x-list)
 (let ((time-index (find-time-index time))(y-list '()))
 (dolist (array array-list)
 (setq y-list (nconc y-list (list (aref array time-index 1)))))
 (list (list y-list x-list))))


;;; MAKE-ALBUM
(defun make-album (time-list array-list x-list)
 (let ((output-list '()))
 (dolist (time time-list)
 (let ((plot-list (make-snapshot time array-list x-list)))
	(setq output-list (nconc output-list plot-list))))
 output-list))



;1067 748
; (progn (send poster-window :expose)
; (let ((*standard-output* poster-window)
;       (data-list  (low-pass-this-list (car (car *pane2-data-list)) 4)))
; (graphics:with-graphics-translation (t  100 200)
;  (graphics:draw-line 0 0 400 0 :thickness 2)	;x-axis, 0 to 500 ms
;  (graphics:draw-line 0 0 0 -100 :thickness 2)	;y-axis -80 to 20 mv
;  (dotimes (x-inc 10)
;    (graphics:draw-line (* x-inc 40) -5 (* x-inc 40) 5 :thickness 2))	;x tics
;  (dotimes (y-inc 5)
;    (graphics:draw-line -5  (* y-inc -20) 5  (* y-inc -20) :thickness 2))
;
;  (loop for voltage in data-list
;	for time in (cadr (car *pane2-data-list)) do
;    
;    (graphics:draw-point (* time (/ 400.0 500)) (- -80 voltage))))))



;(progn (send plot:|Plot Hack 7| :expose)
;   (send plot:|Plot Hack 7|
;   :plot nil (list
;   (cdr (nth 2 (nth 0 (cdar (symbol-value-in-instance voltage-pane2 'plot:zoom-list))))) ;a
;   (cdr (nth 1 (nth 0 (cdar (symbol-value-in-instance voltage-pane2 'plot:zoom-list))))) ;b
;   (cdr (nth 0 (nth 0 (cdar (symbol-value-in-instance voltage-pane2 'plot:zoom-list))))) ;p
;
;   (cdr (nth 3 (nth 0 (cdar (symbol-value-in-instance voltage-pane2 'plot:zoom-list))))) ;n
;  )
;  (list "A" "B" "A->B" "B->A") ))


;(progn (send plot:|Plot Hack 12| :expose) (send plot:|Plot Hack 13| :expose) (send plot:|Plot Hack 14| :expose)
; (send plot:|Plot Hack 15| :expose)
; (send plot:|Plot Hack 12| :select)
; (send plot:|Plot Hack 12|
;   :plot "" (list
;   (cdr (nth 2 (nth 0 (cdar (symbol-value-in-instance voltage-pane2 'plot:zoom-list))))) ;a
;  )
;  (list "A") :x-interval 100 :y-min -80 :y-max -45 :grid-p t :x-label "mS" :y-label "mV")
; (send plot:|Plot Hack 13|
;   :plot "" (list
;   (cdr (nth 1 (nth 0 (cdar (symbol-value-in-instance voltage-pane2 'plot:zoom-list))))) ;b
;  )
;  (list   "B"  ) :x-interval 100  :y-min -80 :y-max -45 :grid-p t :x-label "mS" :y-label "mV")
; (send plot:|Plot Hack 14|
;   :plot "" (list
;   (cdr (nth 0 (nth 0 (cdar (symbol-value-in-instance voltage-pane2 'plot:zoom-list))))) ;p 
;   (cdr (nth 2 (nth 0 (cdar (symbol-value-in-instance voltage-pane2 'plot:zoom-list))))) ;a
;   (cdr (nth 1 (nth 0 (cdar (symbol-value-in-instance voltage-pane2 'plot:zoom-list))))) ;b
;  )
; (list "A->B"  "A" "B" ) :x-interval 100  :y-min -80 :y-max -45 :grid-p t :x-label "mS" :y-label "mV")
; (send plot:|Plot Hack 15|
;   :plot "" (list
;   (cdr (nth 3 (nth 0 (cdar (symbol-value-in-instance voltage-pane2 'plot:zoom-list))))) ;n
;   (cdr (nth 2 (nth 0 (cdar (symbol-value-in-instance voltage-pane2 'plot:zoom-list))))) ;a
;   (cdr (nth 1 (nth 0 (cdar (symbol-value-in-instance voltage-pane2 'plot:zoom-list))))) ;b
;  )
;  (list "B->A" "A" "B") :x-interval 100  :y-min -80 :y-max -45 :grid-p t :x-label "mS" :y-label "mV")) 