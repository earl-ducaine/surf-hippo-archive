;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: SURF; Base: 10 -*-
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


;;; SYS Source file: colorizing.lisp

(IN-PACKAGE "SURF")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;
;;;;;;;;;;;;; Colorizing Simulations
;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun colorize-simulation (&optional now)
  (declare (optimize (safety 0) (speed 3) (space 0)))
  (when (or now
	    (>= (the sf (float-mod (the sf (*t[n]*)) (the sf *colorize-simulation-step*)))
		(the sf (float-mod (the sf *real-time*) (the sf *colorize-simulation-step*)))))
    (when *enable-sparse-data* (update-sparse-data))
    (update-histology-color nil t)))

(defun update-histology-color (&optional wins force (lock nil))
  (loop for win in (flatten-list (or wins *colorized-windows* (windows-of-mode :histology))) do
	(s-value win :lock-color lock)
	(when force (s-value win :colorize t))
;	(s-value win :show-time (and (g-value win :enable-colorize-time) (g-value win :colorize)))
	; (when *enable-colorize-time* (add-temp-comment win ""))
	(UPDATE-ALL-TIME-COMMENTS)
	(opal:update win t)
					; (s-value win :show-time nil)
					; (s-value win :colorize nil)
	))



#|
(defun update-virtual-cell-elements (&optional (win *standard-graphics-output*))
  (loop for comp in (g-value (get-plot-agg win 'segments) :components)
	when (member OPAL:VIRTUAL-AGGREGATE (g-value comp :is-a))
	do (OPAL::UPDATE-METHOD-VIRTUAL-AGGREGATE comp)))
|#
  
(proclaim '(notinline collect-elements-values-and-nodes))
(defun collect-elements-values-and-nodes (elements data-type)
  (loop for elt in elements
	collect (multiple-value-bind (sparse-data-key ordered-sparse-data-key)
		    (element-sparse-data-keys elt data-type)
		  (get-ordered-element-sparse-data elt sparse-data-key ordered-sparse-data-key)) into elements-values
	collect (typecase elt (segment (segment-node-2 elt)) (soma (soma-node elt))) into element-nodes
	finally (return (values elements-values element-nodes))))

#|
(defun replay-colorized-simulation-internal (win start-time-internal stop-time-internal time-step-internal
						 display-time elements elements-values element-nodes num-elements
						 elements-value-array last-elements-value-array)
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (s-value win :enable-colorize-time display-time)
  (let* ((*enable-colorize-time* display-time)
	 (*colorizing-simulation* t)
	 (time-list (the cons (mapcar #'(lambda (val) (coerce val 'double-float)) (current-sparse-data-times))))
	 (elements-value-array (or elements-value-array (make-array (list num-elements) :element-type 'double-float)))
	 (last-elements-value-array (or last-elements-value-array (make-array (list num-elements) :element-type 'double-float)))
	 last-time)
    (declare (double-float start-time-internal stop-time-internal time-step-internal))
    ;; List of lists traversal and surgery adapted from the lisp::map1 function.
    (loop for time double-float in time-list do
	  (do ((i 0 (1+ i))
	       (l elements-values (cdr l)))
	      ((null (car l)))
	    (declare (fixnum i))
	    (setf (aref last-elements-value-array i) (aref elements-value-array i)) 
	    (setf (aref elements-value-array i) (the df (caar l)))
	    (setf (car l) (cdar l)))
	  (if (or (<= start-time-internal time stop-time-internal)
		  (and (> time stop-time-internal)
		       last-time (< (the df last-time) stop-time-internal)))
	      (when last-time
		(let ((loop-end-time (min time stop-time-internal)))
		  (loop for show-time double-float from (the df last-time) to loop-end-time by time-step-internal do
			(loop for node in element-nodes
			      for i fixnum from 0
			      do (setf (node-voltage-n node)
				       (interpolate-data-df (aref elements-value-array i) time
							    (aref last-elements-value-array i) last-time show-time)))
			(setq *real-time* (s-flt show-time))
			(update-histology-color win t))))
	      (when (and (> time stop-time-internal) last-time (> (the df last-time) stop-time-internal))
		(return-from replay-colorized-simulation-internal
		  (values elements-value-array last-elements-value-array))))
	  (setq last-time time))
    (values elements-value-array last-elements-value-array)))
|#

(proclaim '(inline load-and-show-colorized-data))
(defun load-and-show-colorized-data (element-nodes elements-value-array last-elements-value-array show-time last-time next-time win)
  (declare (optimize (safety 0) (speed 3) (space 1))
	   (type (simple-array double-float) elements-value-array last-elements-value-array))
  (loop for node in element-nodes 
	for i fixnum from 0
	do (setf (node-voltage-n node)
		 (interpolate-data-df (aref elements-value-array i) next-time
				      (aref last-elements-value-array i) last-time show-time)))
  (setq *real-time* (s-flt show-time))
  (update-histology-color win t))

(defun replay-colorized-simulation-internal (win start-time-internal stop-time-internal time-step-internal
						 display-time elements elements-values element-nodes num-elements
						 elements-value-array last-elements-value-array)
  (declare (optimize (safety 0) (speed 3) (space 1)))
  ; (mapcar #'(lambda (win) (s-value win :enable-colorize-time display-time)) (coerce-to-list win))
  (s-value-list win :enable-colorize-time display-time)
  (let* ((*enable-colorize-time* display-time)
	 (*colorizing-simulation* t)
	 (time-list (the cons (mapcar #'(lambda (val) (coerce val 'double-float)) (current-sparse-data-times))))
	 (elements-value-array (or elements-value-array (make-array (list num-elements) :element-type 'double-float)))
	 (last-elements-value-array (or last-elements-value-array (make-array (list num-elements) :element-type 'double-float)))
	 last-time)
    (declare (type (simple-array double-float) elements-value-array last-elements-value-array)
	     (double-float start-time-internal stop-time-internal time-step-internal))
    ;; List of lists traversal and surgery adapted from the lisp::map1 function.
    (loop for show-time double-float from start-time-internal to stop-time-internal by time-step-internal
	  do (loop until (or (null time-list) (<= show-time (the df (car time-list)))) do
		   (do ((i 0 (1+ i))
			(l elements-values (cdr l)))
		       ((null (car l)))
		     (declare (fixnum i))
		     (setf (aref last-elements-value-array i) (aref elements-value-array i)) 
		     (setf (aref elements-value-array i) (the df (caar l)))
		     (setf (car l) (cdar l)))
		   (setq last-time (the df (car time-list))
			 time-list (cdr time-list)))
	  when (car time-list)
	  do (load-and-show-colorized-data element-nodes
					   elements-value-array last-elements-value-array
					   show-time last-time (the df (car time-list)) win))
    (values elements-value-array last-elements-value-array)))



(defun replay-colorized-simulation (&key (start-time 0) (stop-time *user-stop-time*) (time-step 0.1)
					 win (repetitions 1) (display-time t) (elements (cell-elements)) data-type)
  "Runs colorized animation of stored values of DATA-TYPE for ELEMENTS in WIN [if NIL, will run in
all histology windows]."
  (let ((num-elements (the fn (length elements)))
	(start-time-internal (d-flt start-time))
	(stop-time-internal (d-flt stop-time))
	(time-step-internal (d-flt time-step))
	;; These are supplied by replay-colorized-simulation-internal will be reused if more than one repetition.
	elements-value-array-int last-elements-value-array-int )
    (loop for trial from 1 by (if repetitions 1 0) do
	  (multiple-value-bind (elements-values element-nodes)
	      (collect-elements-values-and-nodes elements data-type)
	    (multiple-value-bind (elements-value-array last-elements-value-array)
		;; Use an explicit function for debugging purposes.
		(replay-colorized-simulation-internal
		 win
		 start-time-internal stop-time-internal time-step-internal
		 display-time elements elements-values element-nodes num-elements
		 elements-value-array-int last-elements-value-array-int)
	      (setf elements-value-array-int elements-value-array
		    last-elements-value-array-int last-elements-value-array)))
	  when (and repetitions (= trial repetitions)) do (return))
    nil))

(defun show-sparse-data (&optional (target-time *real-time*) data-type win)
  "Update colorization in histology WIN with DATA-TYPE at TARGET-TIME [ms, default *REAL-TIME*]."
  (when (current-sparse-data-times)
    (loop for element in (cell-elements) do
	  (multiple-value-bind (sparse-data actual-time)
	      (retrieve-sparse-data element target-time data-type)
	    (set-element-value element sparse-data data-type)
	    (setq *real-time* (s-flt actual-time))))
    (loop for win in (coerce-to-list (or win *colorized-windows* (windows-of-mode :histology)))
	  do (s-value win :colorize t))
    (update-histology-color win t)))

(defun set-element-value (element value &optional data-type)
  (declare (ignore data-type))
  (when value
    (let ((element (element element)))
      (typecase element
	(segment (set-segment-voltage element value))
	(soma (set-soma-voltage element value))))))

(defun set-element-value-fast (element value)
  (typecase element
    (segment (set-segment-voltage element value))
    (soma (set-soma-voltage element value))))

(defun set-color-map-menu ()
  "Menu for reseting the color mapping used in the colorizing code."
  (let ((default-color-map *default-color-map*))
    (choose-variable-values
     `(*default-color-map* "Choose a color map:" :choose ,*COLOR-MAP-FUNCTIONS*)
     :text "Reset Colorizing Map")
    (unless (equal default-color-map *default-color-map*) (set-color-map))))
      
  
(defun set-color-map (&optional map)
  "Resets the color mapping used in the colorizing code. MAP can be any symbol given by
*COLOR-MAP-FUNCTIONS*, e.g.

     '(SH-ORIGINAL-RGB-MAP HOT-RGB-MAP JET-RGB-MAP GRAY-RGB-MAP PINK-RGB-MAP HOT-COLD-RGB-MAP)

Note that if too many color maps are used (for SunOs, more than 3), an XLIB:ALLOC-ERROR will occur.
This will neccesitate restarting Lisp."
  (when map (setq *default-color-map* map))
  (fill-*cell-fill-styles*)
  nil)
	
(defun test-color-maps (&optional (maps *color-map-functions*))
  (loop for map in (coerce-to-list maps) do
	(setq *default-color-map* map)
	(plot-color-map map)
	(fill-*cell-fill-styles*)
	(let (*standard-graphics-output*)
	  (voltage-color-scale :win-width 300 :win-height *standard-graphics-height* :title map
			       :test t
			       :position-relative-to-window :middle
			       :center-x 0
			       :center-y 0
			       :scale-height (* 0.8 *standard-graphics-height*)
			       :scale-width 160))
	(lock-windows)))


(defvar *voltage-color-scale-border* 0)

(defun voltage-color-scale (&key (num-of-steps (voltage-color-dimension))
				 (max-voltage 50) (min-voltage -100)
				 (win-width 100) (win-height 120)
				 title
				 (update t)
				 test
				 (win (if test
					(get-plot-window nil 'voltage-colors nil
							 :name title 
							 :*create-new-plot-windows* nil
							 :width win-width :height win-height)
					(or *standard-graphics-output*
					    (get-histology-window 'histology title :exclude-auxiliary-type :keys))))
				 remove
				 (font *plot-axis-font*)
				 (position-relative-to-window :lower-right)
				 (center-x -60) (center-y 65) ; Scale position relative to the center of the window.
				 (border *voltage-color-scale-border*)
					; (scale-gap 0) ; don't use
				 (scale-height 100) (scale-width 40)
				 (label-scale-gap 5) (labeled-voltages '(-100 -50 0 50)))
  (loop for win in (coerce-to-list win) do
	(let* ((font (or (g-value win :font) font))
	       (center-x (case position-relative-to-window
			   ((:lower-middle :middle :upper-middle) center-x)
			   ((:lower-right :middle-right :upper-right) (+ center-x (- border) (* 0.5 (g-value win :width))))
			   ((:lower-left :middle-left :upper-left) (+ center-x border (* -0.5 (g-value win :width))))))
	       (center-y (case position-relative-to-window
			   ((:middle-right :middle-left :middle) center-y)
			   (:lower-right (if (view-angle-comment-p win)
					   (+ border (g-value font :font-height) 2 center-y (* -0.5 (g-value win :height)))
					   (+ border center-y (* -0.5 (g-value win :height)))))
			   ((:lower-left :lower-middle :lower-right) (+ center-y border (* -0.5 (g-value win :height))))
			   ((:upper-left :upper-middle :upper-right) (+ center-y (- border) (* 0.5 (g-value win :height))))))
	       (voltage-range (- max-voltage min-voltage))
	       (voltage-increment (/ voltage-range num-of-steps))
	       (labeled-voltages (sort (typecase labeled-voltages
					 (cons labeled-voltages)
					 (t (let ((label-increment (/ (- max-voltage min-voltage) (1- labeled-voltages))))
					      (loop for volts from min-voltage to max-voltage by label-increment
						    collect volts))))
				       '<))
	       
	       (label-width		; estimated-max-label-width
		(max (opal:string-width font (format nil "~A mV" max-voltage))
		     (opal:string-width font (format nil "~A mV" min-voltage))))
	       (scale-thickness (/ scale-height (1+ num-of-steps)))
	       (scale-center-x (+ center-x (* 0.5 label-width)))
	       (scale-center-y center-y)
	       (scale-left (- scale-center-x (* 0.5 scale-width)))
	       (scale-right (+ scale-center-x (* 0.5 scale-width)))
	       (string-right (- scale-left label-scale-gap))
	       (agg (clear-and-add-plot-agg win 'voltage-scale :add (not remove))))
	  (unless remove
	    (loop for step from 0.0 to num-of-steps do
		  (let* ((y1 (+ scale-center-y (* step scale-thickness) (* 0.5 scale-thickness) (* -0.5 scale-height)))
			 (y2 y1)
			 (voltage (+ (* step voltage-increment) min-voltage))
			 (color (get-opal-variable-color voltage)))
		    (when (>= voltage (car labeled-voltages))
		      (add-string (format nil "~A mV" (if (> voltage-range 20) (round voltage) voltage))
				  string-right
				  (+ (* 0.5 (g-value font :font-height)) y1) agg
				  :dimensions-in-pixels t
				  :justification :right
				  :font font)
		      (setf labeled-voltages (cdr labeled-voltages)))
		    (add-line scale-left y1 scale-right y2 agg :color color ; linestyle
			      :dimensions-in-pixels t
			      :thickness (max 5 scale-thickness) ))))
	  (when update (resurrect-opal-win win :show t)))
	collect win into out-wins
	finally (return out-wins)))
	  


(defun plot-color-map (map &optional (length 100))
  (multiple-value-bind (r g b)
      (rgb-map-values map length)
    (plot-timed-data (list r g b) '(red green blue) nil :title map
		     :x-are-fns t
		     :x-label "%"
		     :x-label-horizontal-position :center
		     :line-styles :thick-dashed
		     :x-max length
		     :x-min 0
		     :y-min 0
		     :y-max 1.0)))

(defun colorize-menu (&optional win)
  (let (dummy1
	(dummy2 :no_replay)
	(dummy3 1)
	(dummy4 (or (and win (g-value win :colorize-start-time)) 0.0))
	(dummy5 (or (and win (g-value win :colorize-stop-time)) *user-stop-time*))
	(dummy6 (or (and win (g-value win :colorize-step)) 0.1))
	dummy7
	(dummy8 (if win (g-value win :show-time) *enable-colorize-time*))
	(dummy22 (and win (g-value win :colorize)))
	(dummy24 (if win (g-value win :include-voltage-color-scale) *enable-colorize-scale*))
	(loaded-sparse-data-p (loaded-sparse-data-p)))
    (choose-variable-values
     `((*colorize-simulation* "Colorize simulations" :boolean)
       (dummy22 "Enable colorization for current wins" :boolean)
       (dummy24 "When colorizing, include voltage scale" :boolean)
					; (dummy8 "Display simulation time" :boolean)
       (*enable-sparse-data* "Enable storing of sparse data for all nodes" :boolean)
       (*colorize-simulation-step* "Colorize simulation step [ms]" :float)
       (dummy1 ,(format nil "Select windows to colorize (~A currently selected)" (if win
										   (g-value win :title)
										   (if *colorized-windows*
										     (length *colorized-windows*)
										     "All")))
	       :boolean)
       ,(when loaded-sparse-data-p `(:comment "Sparse Data Animation"))
       ,(when loaded-sparse-data-p `(dummy2 "Replay loaded sparse data" :choose (:Sequence :Instant :no_replay)))
       ,(when loaded-sparse-data-p `(dummy3 "Repetitions" :integer))
       ,(when loaded-sparse-data-p `(dummy4 "Start time [ms] (used for Instant above)" :float))
       ,(when loaded-sparse-data-p `(dummy5 "Stop time [ms]" :float))
       ,(when loaded-sparse-data-p `(dummy6 "Time step [ms]" :float)))
     :label "Colorizing Menu")
    (let ((color-wins (coerce-to-list
		       (if dummy1
			 (setq *colorized-windows* (window-selection-menu "Choose Histology Windows to Colorize"
									  (histology-wins-with-current-cells)
									  (intersection *colorized-windows* *output-windows*)))
			 (or win *colorized-windows* (histology-wins-with-current-cells))))))
      (mapcar #'(lambda (win)
					; (s-value win :enable-colorize-time dummy8)
		  (s-value win :update-colorize (and win (not (equal dummy22 (g-value win :colorize)))))
		  (s-value win :colorize dummy22)
		  (s-value win :include-voltage-color-scale dummy24)
		  (s-value win :show-time dummy8)
		  (s-value win :colorize-start-time dummy4) 
		  (s-value win :colorize-stop-time dummy5)
		  (s-value win :colorize-step dummy6))
	      color-wins)
      (case dummy2
	(:sequence
	 (replay-colorized-simulation
	  :start-time dummy4 :stop-time dummy5 :time-step dummy6
	  :repetitions dummy3
	  :win color-wins))
	(:instant
	 (show-sparse-data dummy4 nil color-wins))
	(t (voltage-color-scale :remove (not (and win (g-value win :colorize)
						  (g-value win :include-voltage-color-scale))))
	   (UPDATE-ALL-TIME-COMMENTS)
	   (mapcar #'(lambda (win)
		       (opal:update win t))
		   color-wins))))
    
    (unless (and *circuit-drawn* *standard-graphics-output*)
      (just-draw))))