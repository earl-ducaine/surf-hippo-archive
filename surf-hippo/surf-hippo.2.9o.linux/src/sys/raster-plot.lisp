;;; -*- Mode: Lisp; Syntax: Common-lisp; Package: SURF ; Base: 10; -*-
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


;;; SYS Source file: raster-plot.lisp
(IN-PACKAGE "SURF-HIPPO")


;;; RASTER-PLOTS Plots of event times of each of the EVENT-ELEMENTS (ONLY-EVENT-GENERATORS nil) or
;;; the original times of the event generators for the EVENT-ELEMENTS (ONLY-EVENT-GENERATORS T,
;;; default). If not supplied, EVENT-ELEMENTS is taken as all the axons, voltage-controlled and
;;; autonomous synapses in the circuit. EVENT-WIDTH is the width of the mark for each event on the
;;; plot, in pixels. START-TIME and STP-TIME are in milliseconds. RASTER-SPACING is the vertical
;;; spacing between rasters in pixels.

;;; Events may also be supplied directly with the keyword argument :EVENT-DATA-LISTS (a list of
;;; lists). In this case, you must also supply a list of names for :EVENT-ELEMENT-LABELS.


(defvar *plot-only-elements-with-activity* nil)



(defun setup-raster-plot-win (win data-agg font start stop plot-left-border plot-right-border plot-top-border plot-bottom-border plot-width width height
				  raster-source-label)
  (s-value win :raster-font font)
  (s-value win :font font)
  (s-value win :event-plotter t)
  (s-value win :minimum-event-time start)
  (s-value win :maximum-event-time stop)

  (s-value win :event-plot-left-border plot-left-border)
  (s-value win :event-plot-width plot-width)

  (resurrect-opal-win win :visible t :update t)
  (s-value win :width width)
  (s-value win :height height)
  ;; Left (START) time line
  (opal:add-component data-agg
		      (create-instance nil opal:line (:constant t) (:line-style (pick-thickness 1))
				       (:x1 plot-left-border) (:x2 plot-left-border)
				       (:y1 (- plot-top-border *default-raster-spacing*))
				       (:y2 (+ *default-raster-spacing* (- height plot-bottom-border)))))
  ;; Start label
  (opal:add-component data-agg
		      (create-instance nil opal:text (:constant nil) (:line-style (pick-thickness 1))
				       (:left (o-formula (round (- plot-left-border (* 0.5 (gvl :width))))))
				       (:top (o-formula (round (+ (gvl :parent :window :raster-font :font-height)
								  (/ *default-raster-spacing* 2)
								  (- height plot-bottom-border)))))
				       (:string (format nil "~ams" start))
				       (:font (o-formula (gvl :parent :window :raster-font)))))
  ;; Raster sources label
  (when (> (length raster-source-label) 0)
    (opal:add-component data-agg
			(create-instance nil opal:text (:constant nil) (:line-style (pick-thickness 1))
					 (:left (o-formula (round (- (/ plot-left-border 2) (* 0.5 (gvl :width))))))
					 (:top (round (- plot-top-border *default-raster-spacing*))) 
					 (:string raster-source-label)
					 (:font (o-formula (gvl :parent :window :raster-font))))))
  
  ;; Right (STOP) time line
  (opal:add-component data-agg
		      (create-instance nil opal:line (:constant nil) (:line-style (pick-thickness 1))
				       (:x1 plot-right-border) (:x2 plot-right-border)
				       (:y1 (- plot-top-border *default-raster-spacing*))
				       (:y2 (+ *default-raster-spacing* (- height plot-bottom-border)))))
  ;; Stop label
  (opal:add-component data-agg
		      (create-instance nil opal:text (:constant nil) (:line-style (pick-thickness 1))
				       (:left (o-formula (round (- plot-right-border (* 0.5 (gvl :width))))))
                                       (:top (o-formula (round (+ (gvl :parent :window :raster-font :font-height)
								  (/ *default-raster-spacing* 2)
								  (- height plot-bottom-border)))))
				       (:string (format nil "~ams" stop))
				       (:font (o-formula (gvl :parent :window :raster-font))))))


(defvar *default-raster-spacing* 20 "Default raster spacing in pixels as used by RASTER-PLOTS.")
(defvar *default-raster-event-width* 1 "Default raster event width in pixels as used by RASTER-PLOTS.")
(defvar *default-raster-event-height* 4 "Default raster event height in pixels as used by RASTER-PLOTS.")

(defun opal-text-list (labels font left)
  (loop for label in (coerce-to-list labels)
	collect (create-instance nil opal:text (:constant nil)
				 (:left left) (:string (princ-to-string label)) (:font font))))

(defun all-event-elements ()
  (flatten-list (axons) (synapses-of-control 'voltage) (synapses-of-control 'auto)))

(defun raster-plot (&key (start 0) stop width height title-postfix win title font
			 (event-width *default-raster-event-width*) (event-height *default-raster-event-height*) ; pixels
			 (times-in-fns t) (max-traces-per-plot 20) (raster-spacing *default-raster-spacing*)
			 event-data-lists ; Explicit data lists which override other sources of events.
			 event-data-list-labels ; Instead of actual names of event elements.
			 fixed-top-gap fixed-bottom-gap fixed-right-gap fixed-left-gap
			 (include-labels t) default-label (raster-source-label ""))
  (let* ((single-data-list (not (consp (car event-data-lists))))
	 (event-data-lists (if single-data-list (list event-data-lists) event-data-lists))
	 (max-event (loop for list in event-data-lists maximize (max-of-list list))))
    (raster-plots :start start :stop (or stop max-event)
		  :width width :height height :title-postfix title-postfix :win win :title title :font font
		  :event-data-lists event-data-lists :event-width event-width :event-height event-height
		  :times-in-fns times-in-fns :max-traces-per-plot max-traces-per-plot
		  :raster-spacing raster-spacing
		  :only-event-generators nil
		  :event-element-labels event-data-list-labels 
		  :fixed-right-gap fixed-right-gap :fixed-left-gap fixed-left-gap :fixed-top-gap fixed-top-gap :fixed-bottom-gap fixed-bottom-gap
		  :include-labels include-labels :default-label default-label :raster-source-label raster-source-label)))
		  


(defun raster-plots (&key (start *user-start-time*) (stop *user-stop-time*) width height title-postfix win title font
			  (event-width *default-raster-event-width*) (event-height *default-raster-event-height*) ; pixels
			  (times-in-fns t) (max-traces-per-plot 20) (raster-spacing *default-raster-spacing*)
			  event-data-lists ; Explicit data lists which override other sources of events.
			  spiking-elements ; Elements for which spikes must be extracted with ELEMENT-SPIKE-TIMES.
			  event-elements ; Elements that have associated events.
			  (only-event-generators t) (only-elements-with-activity *plot-only-elements-with-activity*)
			  event-element-labels ; Instead of actual names of event elements.
			  fixed-top-gap fixed-bottom-gap fixed-right-gap fixed-left-gap
			  (include-labels t) default-label (raster-source-label ""))
  (let* ((*create-new-plot-windows* (or *create-new-plot-windows* *CREATE-NEW-SIMULATION-PLOTS*))
	 (plot-top-border (or fixed-top-gap 40))
	 (plot-bottom-border (or fixed-bottom-gap 50))
	 (plot-h-border 40)
	 (event-width (abs (round event-width))) (event-height (abs (round event-height)))
	 (stop (max start stop))	; (start (max 0.0 start))
	 (duration (- stop start))
	 (elements (or (coerce-to-list spiking-elements)
		       (loop for event-element in (or (coerce-to-list (element event-elements)) (all-event-elements))
			     when (and (event-generator event-element)
				       (event-driven-element-p event-element)
				       (or (not only-event-generators) (event-generator-p event-element)))
			     collect event-element)))
	 (all-event-data-lists (or event-data-lists
				   (loop for element in elements collect 
					 (if spiking-elements (element-spike-times element) (get-events element only-event-generators)))))
	 (all-event-element-labels
	  (coerce-to-list
	   (cond (spiking-elements (massage-element-plot-labels (element-name spiking-elements)))
		 (event-element-labels event-element-labels)
		 (elements (massage-element-plot-labels (element-name elements)))
		 (t (loop for event-list in event-data-lists for count from 1
			  collect (format nil "~A ~A" (or default-label "Data") count)))))))
    (when (and only-elements-with-activity (not event-data-lists))
      (loop for data in all-event-data-lists
	    for label in all-event-element-labels
	    when (> (length data) 0) collect data into data-lists and collect label into label-lists
	    finally (setq all-event-data-lists data-lists
			  all-event-element-labels label-lists)))
    (let ((number-of-windows (ceiling (/ (length all-event-data-lists) max-traces-per-plot)))
	  (temp-type (cond ((and title-postfix only-event-generators)
			    (format nil "~A Event Generator Timing" title-postfix))
			   (title-postfix (format nil "~A Events" title-postfix))
			   (only-event-generators "Event Generator Timing")
			   (t "Events"))))
      (loop for event-data-lists in (split-up all-event-data-lists max-traces-per-plot)
	    for event-element-labels in (split-up all-event-element-labels max-traces-per-plot)
	    for win-count from 0 collect
	    (let* ((plot-pane-type (if (> number-of-windows 1) (format nil "~A ~A" temp-type win-count) temp-type))
		   (title (or title
			      (if (> (length *simulation-name*) 0)
				(concatenate-strings *simulation-name* ": " plot-pane-type)
				plot-pane-type)))
		   (win (or win (get-plot-window :raster plot-pane-type nil :mode :2dplot ; :name title
						 :session-name *session-name*)))
		   (font (or font (plot-window-font win))) ; *plot-axis-font*
		   (half-font-height (/ (g-value font :font-height) 2))
		   (event-element-labels (opal-text-list event-element-labels font 10))
		   (raster-spacing (max raster-spacing (+ 4 (max event-height (g-value font :font-height))))))
	      (when (or fixed-top-gap fixed-bottom-gap fixed-right-gap fixed-left-gap)
		(UPDATE-FIXED-GAP-PARAMETERS win fixed-top-gap fixed-top-gap fixed-bottom-gap fixed-bottom-gap
					     fixed-right-gap fixed-right-gap fixed-left-gap fixed-left-gap))
	      (let* ((max-name-width (if include-labels
				       (loop for label in event-element-labels maximizing (g-value label :width))
				       0))
		     (width (or width (max (g-value win :width) (+ max-name-width 500))))
		     (height (or height
				 (max 0	; (g-value win :height)
				      (+ plot-top-border plot-bottom-border
					 (* (1- (length event-element-labels)) raster-spacing)))))
		     (plot-width (- width (+ max-name-width plot-h-border plot-h-border)))
		     (plot-left-border (or fixed-left-gap (+ max-name-width plot-h-border)))
		     (plot-right-border (if fixed-right-gap
					  (- width fixed-right-gap) (+ max-name-width plot-h-border plot-width)))
		     (data-agg (ph::get-plot-agg win 'data-plot t)))
		(s-value win :title (GET-win-TITLE-STRING title))
		(setup-raster-plot-win win data-agg font
				       (if times-in-fns (round start) start) (if times-in-fns (round stop) stop)
				       plot-left-border plot-right-border plot-top-border plot-bottom-border plot-width
				       width height raster-source-label)
		(loop for events in event-data-lists
		      for event-element-label in event-element-labels
		      for event-element-index from 0
		      for plot-height from plot-top-border by raster-spacing
		      do
		      ;; Event element name.
		      (when include-labels
			(s-value event-element-label :top (round (- plot-height half-font-height)))
			(opal:add-component data-agg event-element-label))
		      (add-raster-events data-agg events start stop duration event-width event-height plot-left-border plot-width plot-height))
		(resurrect-opal-win win :visible t :update t))
	      win)
	    into wins finally (return (atomize-list wins))))))


(defvar *virtual-rasters* t)

(defun add-raster-events (data-agg
			  events start stop duration
			  event-width event-height
			  plot-left-border plot-width plot-height) 
  (if *virtual-rasters*
      (progn (s-value (g-value data-agg :window) :visible t)
	     (opal::update (g-value data-agg :window))
	     (draw-virtual-events data-agg events
				  start stop duration
				  event-width event-height
				  plot-left-border plot-width plot-height))
      (loop for time in events when (and (> stop time) (>= time start)) do
	    (Add-event-tic data-agg plot-left-border time start duration event-width event-height
			   plot-width plot-height))))

(defun Add-event-tic (data-agg plot-left-border time start duration event-width event-height plot-width plot-height)
  (opal:add-component		
   data-agg				
   (create-instance nil opal:line (:constant t) (:line-style (pick-thickness event-height))
		    (:x1 (round (+ plot-left-border (* plot-width (/ (- time start) duration)))))
		    (:x2 (round (+ event-width plot-left-border (* plot-width (/ (- time start) duration)))))
		    (:y1 plot-height) (:y2 plot-height))))


(create-instance 'virtual-event virtual-line
		 ; (:line-style (o-formula (fifth (gvl :item-values))))
		 )


(defun draw-virtual-events (event-agg events
			      start stop duration
			      event-width event-height
			      plot-left-border plot-width plot-height)
  ;;  (declare (optimize (safety 0) (speed 3) (space 1)))
  (let* ((event-line-style (pick-thickness event-height))
	 (itemlist (virtual-event-core events
			   start stop duration
			   event-width event-height event-line-style
			   plot-left-border plot-width plot-height)))
    (virtual-agg-finishing (make-v-agg virtual-event (list-to-array-generic itemlist) 'v-events) event-agg)))

(defun virtual-event-core (events
			   start stop duration
			   event-width event-height event-line-style
			   plot-left-border plot-width plot-height)
  ;;  (declare (optimize (safety 0) (speed 3) (space 1)))
  (loop for event in events when (and (> stop event) (>= event start))
;;	do (format t "Event ~A~%" event)
	collect
	(let* ((x1 (round (+ plot-left-border (* plot-width (/ (- event start) duration)))))
	       (x2 (round (+ event-width plot-left-border (* plot-width (/ (- event start) duration)))))
	      (y1 plot-height)
	      (y2 plot-height))
	  (declare (fixnum x1 y1 x2 y2))
	  (let* ((thickness-pixels event-height)
		 (itemlist
		  (list (the fn x1) (the fn y1)
			(the fn (if (= x1 x2) (1+ x1) x2))
			(the fn y2)
			event-line-style
			thickness-pixels
			event)))
	    ;; (format t "itemlist: ~A~%" itemlist)
	    (list-to-array-generic itemlist)))))



(defun raster-error-steps ()
  (loop for label in '("Voltage LTE" "Particle LTE" "Conc Int LTE")
	for data in (list *VOLTAGE-ERROR-STEP-CHANGES* *particle-ERROR-STEP-CHANGES* *conc-int-ERROR-STEP-CHANGES*)
	when (> (length data) 0) collect label into labels and collect data into events
	finally (raster-plots :event-element-labels labels :event-data-lists events
			      :title (format nil "~A: LTE Criteria" *simulation-name*))))



;;; Written by Nicolas Gazeres
;;;
;;; EVENT-LISTS-RASTER
;;; ---------------------
;;; cette petite fonction a pour but de fixer le bug de chez lyle
;;; s'il n'y a pas de labels a raster-plots, il ne trace pas les listes fournies...
;;; Here: event-times = '((1.0 2.0 ...) (2.5 4.0 4.5 ...) ... )

(defun EVENT-LISTS-RASTER (event-times &key
				       (start *user-start-time*)
				       (stop *user-stop-time*)
				       (merging NIL)
				       (event-width 1) (event-height 4)
				       (raster-spacing 6) (max-traces-per-plot 50)
				       (no-labels NIL)
				       title
				       update-fixed-gap-parameters
				       (fixed-top-gap 0)
				       (fixed-bottom-gap 0)				       
				       (fixed-right-gap 0)
				       (fixed-left-gap 0)
				       win
				       width height
				       )
  (let (merged-events)
    (when merging
      (setq merged-events (sort (copy-list (loop for event-list in event-times append event-list)) #'<)))
    (raster-plots :only-event-generators nil
		  :max-traces-per-plot max-traces-per-plot
		  :start start
		  :stop stop
		  :event-width event-width
		  :event-height event-height
		  :raster-spacing raster-spacing

		  :win win
		  :title title
		  :width width
		  ;; :height height
		  
		  :update-fixed-gap-parameters update-fixed-gap-parameters
		  ; :use-fixed-right-gap use-fixed-right-gap
		  :fixed-right-gap fixed-right-gap
		  ; :use-fixed-left-gap use-fixed-left-gap
		  :fixed-left-gap fixed-left-gap
		  ; :use-fixed-top-gap  use-fixed-top-gap
		  :fixed-top-gap  fixed-top-gap		  
		  ; :use-fixed-bottom-gap  use-fixed-bottom-gap
		  :fixed-bottom-gap  fixed-bottom-gap		  
		  
		  :include-labels (not no-labels)
		  :event-element-labels
		  (unless no-labels
		    (append (loop for k below (length event-times) collect (format NIL "Event ~A" k))
			    (if merging '("Merged Events"))))
		  :event-data-lists (append
				     event-tImes
				     (if merging (list merged-events))))))