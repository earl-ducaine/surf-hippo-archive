;;; -*- Mode: Lisp; Syntax: Common-lisp; Package: SURF-HIPPO ; Base: 10; -*-
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


;;; SYS Source file: user-cell-graphics.lisp

;;; Some of the user graphics functions.

(IN-PACKAGE "SURF-HIPPO")


(defun get-generic-histology-window ()
  (get-histology-window 'histology
			(find-histology-window-title)
			:create-new-win t
			:exclude-auxiliary-type :keys))

(defun just-draw (&key (win *standard-graphics-output*)
		       (colorize (when win (g-value win :colorize)))
		       (background-color (if win (g-value win :background-color) opal::white))
		       (label-all-synapses (when win (g-value win :label-all-synapses))) ; "Enable synapse markers"
		       (draw-marked-synapses (when win (g-value win :draw-marked-synapses))) ; "Mark all synapses"
		       (draw-all-synapse-rfs (when win (g-value win :draw-all-synapse-rfs))) ; "Draw all synapse RFs"
		       (draw-synapse-rfs (when win (g-value win :draw-synapse-rfs))) ; "Enable synapse RFs and connections"
		       
		       (scale (if win (g-value win :scale) 3.0))
		       (phi-deg (if (and win (g-value win :phi-deg)) (g-value win :phi-deg) 0.0))
		       (theta-deg (if (and win (g-value win :theta-deg)) (g-value win :theta-deg) 0.0))
		       (DRAW-AXONS T)
		       (DRAW-SYNAPSE-CXNS T))
  "Draw the current circuit without invoking the Histology Menu. Arguments are the same as those for
SET-MISC-HISTO-SLOTS."
  (process-circuit-structure)		; Just in case this is not done yet
  (when *circuit-loaded*
    (let ((*automatic-run* t)
	  (win (if (or (not win) *create-new-histology-window*)
		 (get-generic-histology-window)
		 win)))
      (SET-MISC-HISTO-SLOTS :win win :scale scale :phi-deg phi-deg :theta-deg theta-deg
			    :colorize colorize
			    :background-color (get-opal-color background-color)
			    :draw-axons draw-axons :draw-synapse-cxns draw-synapse-cxns
			    :label-all-synapses label-all-synapses ; "Enable synapse markers"
			    :draw-marked-synapses draw-marked-synapses ; "Mark all synapses"
			    :draw-all-synapse-rfs draw-all-synapse-rfs ; "Draw all synapse RFs"
			    :draw-synapse-rfs draw-synapse-rfs ; "Enable synapse RFs and connections"
			    )
      (drawing-menu win t *create-new-histology-window* t))))

(defun histo ()
  "Invoke the Histology Menu for drawing the circuit."
  (drawing-menu))

#|
(defun histology () (drawing-menu))

(defun test ()
  (time (draw-cells *standard-graphics-output* t)))
|#

(defun unmark-elements (&optional (win *standard-graphics-output*)
				  &key (erase-channels t) (erase-synapses t))
  (mark-elements :win win :erase-channels erase-channels :erase-synapses erase-synapses))

(defun mark-elements (&key (win *standard-graphics-output*)
			   (update t)
			   specific-elts
			   erase-channels draw-channels erase-synapses draw-synapses (key-diameter 15)
			   )
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (let ((element-marker-agg
	 (when (or erase-channels draw-channels erase-synapses draw-synapses specific-elts)
	   (clear-and-add-plot-agg win `element-markers
				   :clear (or erase-channels erase-synapses) 
				   :add (or draw-channels draw-synapses specific-elts) :where :front)))
	(*grape-size* (grape-size win)))
    (when (or erase-channels draw-channels erase-synapses draw-synapses specific-elts)
      (element-key-window win :draw-channels draw-channels :draw-synapses draw-synapses
			  :add-key (g-value win :include-element-key-window) :Key-diameter Key-diameter))
    (when (or draw-channels draw-synapses specific-elts)
      (loop for cell-element in (cell-elements (g-value win :cells)) do
	    (let ((node (element-physical-node-fast cell-element)))
	      (multiple-value-bind (x y)
		  (X-Y-HISTOLOGY-WIN-FROM-VIEW-VALUES (node-absolute-location node) win)
		(when (x-y-in-win-values x y win)
		  (let ((colors
			 (flatten-list
			  (when (and draw-channels (or specific-elts (g-value win :label-all-channels)))
			    (loop for channel in (get-node-elements-of-type node 'channel)
				  when (or (not specific-elts) (member channel specific-elts))
				  collect
				  (loop for type-info in (g-value win :channel-type-graphics-parameters)
					when (and (my-assoc 'label-type type-info)
						  (eq (get-a-value 'type type-info) (channel-type channel)))
					return (element-parameter (get-a-value 'type type-info) 'color))))
			  (when (and draw-synapses (or specific-elts (g-value win :label-all-synapses)))
			    (loop for synapse in (get-node-synapses-for-marking node)
				  when (or (not specific-elts) (member synapse specific-elts))
				  collect
				  (loop for type-info in (g-value win :synapse-type-graphics-parameters)
					when (and (my-assoc 'label-type type-info)
						  (eq (get-a-value 'type type-info) (synapse-type synapse)))
					return (element-parameter (get-a-value 'type type-info) 'color)))))))
		    ;; (format t "Ready to make grapes...~%")
		    (when colors (OPal:add-component element-marker-agg (make-grapes colors win x y))))))))))
  (when update
    (histology-window-finishing win)
    (opal::update win t))
  nil)

(defun color-cell-element (element &optional color)
  "Assigns a COLOR (including 'RED, 'GREEN, 'BLUE, 'YELLOW, 'ORANGE, 'CYAN, 'PURPLE, 'BLACK, 'WHITE)
to the cell element or elements associated with ELEMENT (can be an atom or list). To clear a color,
COLOR should be NIL, or not supplied."
  (loop for cell-elt in (coerce-to-list (element element))
	when (cell-p cell-elt)
	do (color-cell-element (cell-elements cell-elt) color)
	else
	do (element-parameter (element-cell-element cell-elt) 'color color)
	(element-color-index cell-elt (when color (color-index color))))
  nil)

(defun element-color-index (element &optional (color-index nil color-index-supplied))
  (let ((node (element-physical-node element)))
    (if color-index-supplied
	(setf (node-color-index node) color-index)
	(node-color-index node))))

(defun shade-cell-element (element &optional (shading 100))
  "Assigns a SHADING (percentage will be bounded between 0 and 100 percent)
to the cell element or elements associated with ELEMENT (can be an atom or list)."
  (loop for cell-elt in (coerce-to-list (element element))
	when (cell-p cell-elt) do (shade-cell-element (cell-elements cell-elt) shading)
	else do (element-parameter (element-cell-element cell-elt) 'shading (s-flt (bound-val shading 100 0))))
  nil)



;; SET-TYPE-COLOR Sets the graphics color for elements of type TYPE (name or object).
(defun set-type-color (type color)
  "Possible colors include: 'RED, 'GREEN, 'BLUE, 'YELLOW, 'ORANGE, 'CYAN, 'PURPLE, 'BLACK, 'WHITE"
  (setq color (read-from-string (string color)))
  (element-parameter type 'color (get-opal-color color)))


(defun set-type-graphics (element value &optional (parameter 'color) (win *standard-graphics-output*))
  "SET-TYPE-GRAPHICS Sets graphic qualities for type associated with ELEMENT (e.g. types of channels
and synapses), and specifically enables the graphics of the type, e.g.:

     (set-type-graphics 'DR-HH 'cyan 'color)

For colors, possible colors include: 'RED, 'GREEN, 'BLUE, 'YELLOW, 'ORANGE, 'CYAN, 'PURPLE, 'BLACK, 'WHITE.
Valid PARAMETER arguments include 'COLOR, and 'HEIGHT (for light synapse RF plotting)."
  (let ((*automatic-run* t))		; Disable the menus
    (loop for type in (coerce-to-list (element-type element)) do
	  (let ((win-slot (typecase type
			    (synapse-type :synapse-type-graphics-parameters)
			    (channel-type :channel-type-graphics-parameters))))
	    (unless (and (not *create-new-histology-window*) win)
	      (setq win (get-generic-histology-window)))
	    (case parameter
	      (color (set-type-color type value)))
	    (setq *create-new-histology-window* nil *circuit-drawn* t) ; For using this during a circuit load.
	    (let* ((old-type-parameters (loop for type-info in (g-value win win-slot)
					      when (eq type (cdr-assoc 'type type-info))
					      do (return type-info)))
		   (new-type-parameters
		    (cons (acons parameter value (remove (assoc parameter old-type-parameters) old-type-parameters))
			  (remove old-type-parameters (g-value win win-slot)))))
	      (s-value win win-slot new-type-parameters) ; This will be digested a bit in
					; synapse-graphics-menu and channel-graphics-menu.
	      (typecase (element type)
		(synapse-type
		 (s-value win :label-all-synapses t)
		 (s-value win :draw-marked-synapses t)
		 (s-value win :draw-synapse-rfs t)
		 (s-value win :draw-all-synapse-rfs t)
		 (synapse-graphics-menu win))
		(channel-type
		 (s-value win :label-all-channels t)
		 (s-value win :draw-marked-channels t)
		 (channel-graphics-menu win))))))))


(defun set-misc-histo-slots (&key (win *standard-graphics-output*)
				  (colorize (when win (g-value win :colorize)))
				  (background-color (if win (g-value win :background-color) opal::white))
				  (label-all-synapses (when win (g-value win :label-all-synapses)))
				  (draw-marked-synapses (when win (g-value win :draw-marked-synapses)))
				  (draw-all-synapse-rfs (when win (g-value win :draw-all-synapse-rfs)))
				  (draw-synapse-rfs (when win (g-value win :draw-synapse-rfs)))
		       
				  (scale (if win (g-value win :scale) 3.0))
				  (phi-deg (if (and win (g-value win :phi-deg)) (g-value win :phi-deg) 0.0))
				  (theta-deg (if (and win (g-value win :theta-deg)) (g-value win :theta-deg) 0.0))

				  (DRAW-AXONS T)
				  (DRAW-SYNAPSE-CXNS T))	      
  "Set some basic graphics parameters without using the menus. e.g.

 (SET-MISC-HISTO-SLOTS :scale 3.0 :phi-deg 90.0 :DRAW-AXONS nil)

Angle args are in degrees, and scale arg is in microns/pixel."
  (unless win (setq win (get-generic-histology-window)))
  (setq *create-new-histology-window* nil *circuit-drawn* t) ; For using this during a circuit load.
  (s-value win :colorize colorize)
  (s-value win :background-color (get-opal-color background-color))
  (s-value win :scale scale)
  (s-value win :phi (deg-to-rad phi-deg))
  (s-value win :theta (deg-to-rad theta-deg))

  (s-value win :draw-axons draw-axons)

  (let ((update-syn-graphics (or t
			      (xor (g-value win :draw-synapse-cxns) draw-synapse-cxns)
			      (xor (g-value win :label-all-synapses) label-all-synapses) ; "Enable synapse markers"
			      (xor (g-value win :draw-marked-synapses) draw-marked-synapses) ; "Mark all synapses"
			      (xor (g-value win :draw-all-synapse-rfs) draw-all-synapse-rfs) ; "Draw all synapse RFs"
			      (xor (g-value win :draw-synapse-rfs) draw-synapse-rfs) ; "Enable synapse RFs and connections"
			      )))
    (s-value win :draw-synapse-cxns draw-synapse-cxns)
    (s-value win :label-all-synapses label-all-synapses) ; "Enable synapse markers"
    (s-value win :draw-marked-synapses draw-marked-synapses) ; "Mark all synapses"
    (s-value win :draw-all-synapse-rfs draw-all-synapse-rfs) ; "Draw all synapse RFs"
    (s-value win :draw-synapse-rfs draw-synapse-rfs) ; "Enable synapse RFs and connections"
    (s-value win :update-synapse-rfs update-syn-graphics)
    (s-value win :update-marked-synapses update-syn-graphics)))



(defun label-element (element &optional (win *standard-graphics-output*) (update t) label-agg)
  "Labels the cell element associated with ELEMENT in the histology WIN [default *STANDARD-GRAPHICS-OUTPUT*]."
  (let* ((original-labeled-elements (g-value win :labeled-elements))
	 (cell-elements (loop for elt in (coerce-to-list (element-cell-element element))
			      unless (member elt original-labeled-elements)
			      do (push elt (g-value win :labeled-elements)) and
			      collect elt))
	 (label-agg (when cell-elements (or label-agg (add-plot-agg win `node-labels :front)))))
    (when label-agg
      (mark-segments-and-somas
       cell-elements
       :win win
;       :mark-agg label-agg
       :label-agg label-agg
       :type 'node-labels)
      (when update (histology-window-finishing win))
      label-agg)))



(defun mark-segments-and-somas (elements &key window-menu (win *standard-graphics-output*) (type 'marked-nodes) label-agg 
					 mark-agg (MARKER-DIAMETER (or (and win (g-value win :marker-diameter)) 10))
					 (mark-somas t) (mark-fill opal:black-fill) (update t))
  "Add marker to the cell elements associated with ELEMENTS - in all cases, a marker and/or label will
be drawn referring to the underlying segment or soma. Markers are added to the histology in WIN, if
supplied, otherwise if WINDOW-MENU is non-nil, then a window selection menu is invoked, otherwise,
the current value of *STANDARD-GRAPHICS-OUTPUT* is used."
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (when (and (or mark-agg label-agg) elements)
    (loop for win in (coerce-to-list (or win (if window-menu (win-menu) *standard-graphics-output*))) do
	  (when update (opal:update win))
	  (let ((out '())
		(mark-agg (and mark-agg (typecase mark-agg
					  (schema mark-agg)
					  (t (clear-and-add-plot-agg win type :add t :where :front)))))
		(label-agg (and label-agg (typecase label-agg
					    (schema label-agg)
					    (t (clear-and-add-plot-agg win type :add t :where :front))))))
	    (loop for element-comp in (coerce-to-list elements)
		  do (let* ((element (if (listp element-comp) (car element-comp) element-comp))
			    (end-location (if (and (listp element-comp) (nth 2 element-comp))
					      (nth 2 element-comp)
					      (element-absolute-location element)))
			    (x-y-2 (if (electrode-p element)
				       (electrode-sourcepoint win element)
				       (x-y-histology-win-from-view end-location win))))
		       (when (x-y-in-win x-y-2 win)
			 (when label-agg
			   (label-cell-node element label-agg :x-offset (or (g-value win :label-x-offset) 0)
					    :explicit-node-xy-pixel-location
					    (when (electrode-p element) (electrode-sourcepoint win element))))
			 (when (and mark-agg (or (not (soma-p element)) mark-somas))
			   (push (virtual-cell-element-marker-specs
				  win element-comp element marker-diameter mark-fill x-y-2)
				 out)))))
	    (when mark-agg (install-virtual-cell-element-markers win mark-agg out type)))
	  (when update (opal:update win)))))


(defun virtual-cell-element-marker-specs (win element-comp element marker-diameter mark-fill x-y-2)
  (let ((radius (the fn (round (* 0.5 (or MARKER-DIAMETER (element-marker-diameter element win)))))))
    ;; (xcenter ycenter radius fill soma diameter left top)
    (list (the fn (car x-y-2)) (the fn (cadr x-y-2)) radius 
	  (case mark-fill
	    (use-element-colors
	     (if (listp element-comp)
		 (colors-to-fill (nth 1 element-comp))
		 opal:black-fill))
	    (t mark-fill))
	  nil
	  (the fn (or MARKER-DIAMETER (element-marker-diameter element win)))
	  (the fn (- (car x-y-2) radius))
	  (the fn (- (cadr x-y-2) radius)))))


(defun install-virtual-cell-element-markers (win mark-agg item-list type) 
  (virtual-agg-finishing
   (make-v-agg (if (g-value win :suppress-element-marker-borders)
		   virtual-cell-element-marker-no-border
		   virtual-cell-element-marker)
	       (list-to-array-generic item-list)
	       type)
   mark-agg
   (g-value win :where-element-markers-go)))


(defun spin-histology (&key (win *standard-graphics-output*))
  (let ((*automatic-run* t))
    (s-value win :adjust-histology-window :fix)
	
    (loop for theta from 0.0 by 20 do
	  (SET-MISC-HISTO-SLOTS :win win
				:scale 0.8
				:phi-deg (mod (/ theta 10)  360)
				:theta-deg (mod theta  360))

	  (drawing-menu win t nil t))))