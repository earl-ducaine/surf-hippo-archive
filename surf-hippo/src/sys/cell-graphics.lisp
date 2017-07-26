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


;;; SYS Source file: cell-graphics.lisp

(IN-PACKAGE "SURF-HIPPO")

;;; DRAWING-MENU
(defun drawing-menu (&optional (win *standard-graphics-output*) (parented *circuit-drawn*)
			       (create-new-win *create-new-histology-window*) draw-cells-now)
  (clean-up-*output-windows*)
  (when (and *circuit-loaded*
	     (or (not win)
		 (not (g-value win :locked))
		 (go-ahead-menu (format nil "Unlock window ~A" (g-value win :title)))))
    (process-circuit-structure)		; Just in case this is not done yet
    (when win (s-value win :locked nil))

    ;; These two forms shouldn't be necessary, but this is a cheap fix. 
    (unless (and (opal-obj-exists win)
		 (not (g-value win :auxiliary-type)))
      (setq win nil))
    (unless (and (opal-obj-exists *standard-graphics-output*)
		 (not (g-value *standard-graphics-output* :auxiliary-type)))
      (setq *standard-graphics-output* nil))
    (let* ((new-win (not win))
	   dummy28
	   (prototype
	    (if (and *circuit-drawn* win)
	      win
	      (setq dummy28 t
		    win (get-histology-window 'histology
					      (find-histology-window-title)
					      ; (concatenate-strings *simulation-name* ": Histology")
					      :exclude-auxiliary-type :keys))))
	   (dummy1 (rad-to-deg (g-value win :phi)))
	   (dummy2 (rad-to-deg (g-value win :theta)))
	   (dummy3 (case (g-value win :adjust-histology-window)
		     (:automatic :automatic)
		     ((:fix :match :menu) :fix)))
	   (dummy4 *label-stimulus-times*)
	   (dummy5 *motion-snapshots*)
	   (dummy8 (/ 1 (g-value win :scale)))
	   (dummy9 (or (not (= (g-value win :width) (g-value win :last-drawn-width)))
		       (not (= (g-value win :height) (g-value win :last-drawn-height)))
		       draw-cells-now (not *circuit-drawn*) dummy28))
	   (dummy10 (when create-new-win :Create_new))
	   dummy11 dummy12 dummy15  dummy17
	   (dummy18 (and (are-there-light-synapses) (g-value win :draw-synapse-stimulus)))
	   dummy22 dummy24 (dummy25 100.0) (dummy26 t) dummy27 dummy28 dummy29
	   (menu-list
	    `((dummy2 ,(format nil "Viewing angle theta [degrees]~% Retina: 0/90 => flat/radial mount]") :float)
	      (dummy1 ,(format nil "Viewing angle phi [degrees]~%[Retina: 0 => flat & radial mount]") :float)
	      (dummy8 "Drawing scale (pixels/micron):" :number)
	      (dummy3 "Method to size histology window:" :choose (:fix :match :menu :automatic))
	      ,(when (are-there-light-synapses)
		 `(dummy18 ,(format nil "Draw synapse stimulus~% (=> Orient the view to stimulus plane)") :boolean))
	      (dummy10 "Action on Histology window:" :choose (:Refresh :Raise :Create_new))
	      (dummy17 "Edit histology rendering details" :boolean)
	      ,(when (loaded-sparse-data-p) `(dummy29 "Colorize menu" :boolean)))))
      (unless (g-value win :cell-types) (s-value win :cell-types (cell-type-names)))
      (unless (g-value win :cells) (s-value win :cells (cell-names)))
      (unless *automatic-run*
	(when (> (hash-table-count (CELL-HASH-TABLE)) 1)
	  (push (list 'dummy22
		      (cond ((g-value win :restrict-cells-to-cell-types)
			     (CONCATENATE-STRINGs
			      (format nil "Select cell or cell types to draw~%")
			      "(now showing types: "
			      (CONCATENATE-STRING-LIST (g-value win :cell-types)
						       :string-count-to-add-linefeed 5 :string-spacer " ")
			      ")"))
			    ((not (= (hash-table-count (CELL-HASH-TABLE)) (length (g-value win :cells))))
			     (CONCATENATE-STRINGs
			      (format nil "Select cell or cell types to draw~%")
			      "(now showing cells: "
			      (CONCATENATE-STRING-LIST (g-value win :cells)
						       :string-count-to-add-linefeed 5 :string-spacer " ")
			      ")"))
			    (t (format nil "Select cell or cell types to draw:")))
		      :boolean) menu-list))

	(choose-variable-values menu-list :label (if new-win
						   "Setting Up Cell Drawing"
						   (format nil "Drawing Menu for ~A" (g-value win :title)))
				:title (unless new-win (format nil "Drawing Menu for ~A" (g-value win :title)))))
      ;; Avoid getting a new window if one was generated at the beginning of this function.
      (when (and *circuit-drawn* (eq :Create_new dummy10))
	(setq win (get-histology-child-window win parented
					      :exclude-auxiliary-type :keys)))
      (s-value win :update-synapse-stimulus (or (not (= dummy5 *motion-snapshots*))
						(xor dummy4 *label-stimulus-times*)
						(xor dummy18 (g-value win :draw-synapse-stimulus))))
      (s-value win :draw-synapse-stimulus dummy18)
      (s-value win :raise (eq dummy10 :Raise))

      (load-window-with-cells win dummy22)

      (when (g-value win :draw-synapse-stimulus)
	(case *light-stimulus-plane*
	  (:xy (setq dummy1 0.0 dummy2 0.0)) 
	  (:xz (setq dummy1 0.0 dummy2 90.0))))

      (setq dummy9 (or dummy9
		       (eq dummy10 :Refresh)
		       (g-value win :complete-update)
		       (g-value win :update-anatomy)
		       (not (= (g-value win :width) (g-value win :last-drawn-width)))
		       (not (= (g-value win :height) (g-value win :last-drawn-height)))
		       (not (= dummy1 (rad-to-deg (g-value win :phi))))
		       (not (= dummy2 (rad-to-deg (g-value win :theta))))
		       (not (equal dummy3 (g-value win :adjust-histology-window)))
		       (equal dummy3 :menu) (equal dummy3 :match)
		       (not (= dummy8 (/ 1 (g-value win :scale))))
		       dummy10))

      (setq *create-new-histology-window* nil)
      (s-value win :adjust-histology-window dummy3)
      (s-value win :update-anatomy dummy9)
      (s-value win :draw-anatomy t)
      (s-value win :complete-update (or (g-value win :complete-update) dummy9))

      (when (equal dummy3 :match)
	(let ((dummy1 :none))
	  (choose-variable-values
	   `((dummy1 "Match window dimensions to another plot:" :choose (:Width_&_Height :Width :Height :none) :rank-margin 4))
	   :title "Editing Histology Graphics" :text (g-value prototype :title))
	  (match-win-dimensions-menu prototype dummy1)))
      (let ((x-shift (* -1 (g-value prototype :scale) (aref (g-value prototype :current-xfrm) 2 0))) ; x-shift
	    (y-shift (* -1 (g-value prototype :scale) (aref (g-value prototype :current-xfrm) 2 1))) ;  y-shift
	    (width (* (g-value prototype :scale) (g-value prototype :width)))
	    (height (* (g-value prototype :scale) (g-value prototype :height))))
	(cond-every
	 (dummy9 (set-histology-window-angle-scale-parameters win (deg-to-rad dummy2) (deg-to-rad dummy1) (/ 1 dummy8)))
	 (dummy17 (histology-rendering-menu win))
	 (dummy9 (resize-histology-window win x-shift y-shift width height))))
      (if (g-value win :cells)
	(progn
	  (setq *circuit-drawn* (or *circuit-drawn* dummy9))
	  (when (or dummy9 dummy17)
	    (draw-cells win)))
	(clear-window win))
      (when dummy29 (colorize-menu win))
      win)))


;;; DRAW-CELLS
(defun draw-cells (&optional (win *standard-graphics-output*) complete-update)
  (process-circuit-structure)		; Just in case this is not done yet
  (unless win
    (let ((title (concatenate-strings *simulation-name* ": Histology")))
      (setq win (get-histology-window 'histology title :exclude-auxiliary-type :keys))
      (ADD-TIME-COMMENT win)
      (s-value win :complete-update t)
					; (s-value win :title title)
      (load-window-with-cells win t)))
  (add-time-comment win)
;  (s-value win :show-time nil)
					; (s-value win :colorize nil)
  (when complete-update (s-value win :complete-update t))
  (let ((something-done
	 (or (g-value win :update-anatomy) (g-value win :complete-update)
	     (g-value win :update-axons)
	     (g-value win :update-synapse-cxns)
	     (g-value win :update-marked-nodes)
	     (g-value win :update-marked-channels) (g-value win :update-marked-synapses) (g-value win :complete-update)
	     (g-value win :update-synapse-rfs)(g-value win :update-synapse-stimulus))))

     (when (g-value win :complete-update)
       (s-value win :update-anatomy t)
       (s-value win :update-axons t)
       (s-value win :update-synapse-cxns t)
       (s-value win :update-marked-nodes t)
       (s-value win :update-marked-channels t)
       (s-value win :update-marked-synapses t)
       (s-value win :update-element-key-window t)
       (s-value win :update-synapse-rfs t))

    (cond-every
     (something-done
      (unless nil			; *colorizing-simulation*
	(add-temp-comment win "Redrawing..." :update nil))
      )
     ((or (g-value win :update-anatomy)
	  (g-value win :complete-update)
	  (g-value win :update-axons)
	  (g-value win :update-synapse-cxns)
	  (g-value win :update-marked-nodes)
	  (g-value win :update-marked-channels)
	  (g-value win :update-marked-synapses) 
	  (g-value win :update-element-key-window)
	  (g-value win :update-synapse-rfs))
      ;; So that the virtual aggregate created for the virtual elements appears properly.
      (unless nil			; *colorizing-simulation*
	(s-value win :visible t) (opal:update win)))
     (t 
      (voltage-color-scale :remove (not (and (g-value win :colorize)
				        (g-value win :include-voltage-color-scale)))))
     
     ((or (g-value win :update-anatomy) (g-value win :complete-update))
      (unless nil			; *colorizing-simulation*
	(s-value win :session-name *simulation-name*))
      (draw-segments win (g-value win :draw-anatomy))
      (draw-extracellular-electrodes win (g-value win :draw-extracellular-electrodes))
      (draw-somas win (and (g-value win :draw-somas) (g-value win :draw-anatomy)))

      (unless nil			; *colorizing-simulation*
	(draw-chosen-one (or (g-value win :chosen-ones) (g-value win :chosen-one)) win)))

     ((and				; (not *colorizing-simulation*)
       (or (g-value win :update-axons) (g-value win :complete-update)))
      (draw-axons win (g-value win :draw-axons))
      (s-value win :update-axons nil))
     
     ((and				; (not *colorizing-simulation*)
       (or (g-value win :update-synapse-cxns) (g-value win :complete-update)))
      (draw-synapse-connections win (g-value win :draw-synapse-cxns))
      (s-value win :update-synapse-cxns nil))

     ((and				; (not *colorizing-simulation*)
       (or (g-value win :update-electrodes) (g-value win :update-anatomy) (g-value win :complete-update)))
      (draw-electrodes win (g-value win :draw-electrodes))
      (s-value win :update-electrodes nil))

     ((and				; (not *colorizing-simulation*)
       (or (g-value win :update-sources) (g-value win :update-anatomy) (g-value win :complete-update)))
      (draw-sources win (member :draw (g-value win :source-graphics)))
      (s-value win :update-sources nil))

     ((and				; (not *colorizing-simulation*)
       (or (g-value win :update-marked-nodes) (g-value win :update-anatomy) (g-value win :complete-update)))
      (mark-nodes win (member :mark (g-value win :all-node-graphics)))
      ;; fix this later
					 (mark-segment-chains win t nil nil)
      (s-value win :update-anatomy nil))

     ((and				; (not *colorizing-simulation*)
       (or (g-value win :update-synapse-rfs) (g-value win :complete-update)))
      (draw-synapse-rfs win (g-value win :draw-synapse-rfs))
      (s-value win :update-synapse-rfs nil))
   
     ((and				; (not *colorizing-simulation*)
       (or (g-value win :update-element-key-window)
	   (g-value win :update-marked-channels) (g-value win :update-marked-synapses) (g-value win :complete-update)))
      (mark-elements :win win :erase-channels (g-value win :update-marked-channels)
		     :update nil
		     :draw-channels (g-value win :draw-marked-channels)
		     :erase-synapses (g-value win :update-marked-synapses)
		     :draw-synapses (g-value win :draw-marked-synapses))
      (s-value win :update-element-key-window nil)
      (s-value win :update-marked-channels nil)
      (s-value win :update-marked-synapses nil))
   
     ((and				; (not *colorizing-simulation*)
       (or (g-value win :update-synapse-stimulus) (g-value win :complete-update)))
      (draw-synapse-stimulus win (g-value win :draw-synapse-stimulus))
      (s-value win :update-synapse-stimulus nil))     

     ((and				; (not *colorizing-simulation*)
       (or (g-value win :update-plotted-nodes) (g-value win :complete-update)  (g-value win :update-marked-nodes)))
      (mark-plotted-nodes win (or (member :mark (g-value win :plotted-node-graphics))
				  (member :label (g-value win :plotted-node-graphics))
				  (member :label (g-value win :all-node-graphics))
				  (member :mark (g-value win :all-node-graphics))))
      (s-value win :update-marked-nodes nil)
      (s-value win :update-plotted-nodes nil)))
    (s-value win :complete-update nil)
    (unless nil				; *colorizing-simulation*
      (adjust-all-labels win))
    (cond (something-done
	   (s-value win :last-drawn-width (g-value win :width))
	   (s-value win :last-drawn-height (g-value win :height))
	   (add-temp-comment win ""
			     :update t :resurrect *colorizing-simulation*)
	   )
	  ((g-value win :update-colorize) (update-histology-color win))
	  ((g-value win :raise)
	   (add-temp-comment win "" :update t :resurrect t)
	   (s-value win :raise nil)))
    (unless nil				; *colorizing-simulation*
      (REFRESH-MARKERs-POSITION win)))
  (histology-window-finishing win)
  win)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Time comment for windows.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(create-instance 'time-comment WINDOW-COMMENT
		 (:visible nil
;			   (o-formula  
;                            (and        ;(gvl :parent)
;                             nil
;                             (format t "Visible....?~%")
;                             (gvl :parent :window :enable-colorize-time)
;                             (gvl :parent :window :show-time)
;                             *circuit-processed*))
			   )
		 (:parts
		  `((:background ,opal:rectangle
		     (:filling-style ,(o-formula (or (gvl :parent :window :default-graphics-background)
						     opal::white-fill)))
		     (:left ,(o-formula (gvl :parent :left)))
		     (:Top ,(o-formula (gvl :parent :top)))
		     (:width ,(o-formula (+ (the fn (gvl :parent :label :width)) (* 2 *background-border-width*))))
		     (:height ,(o-formula (+ (the fn (gvl :parent :label :height)) (* 2 *background-border-width*))))
		     (:box '(0 0 0 0))
		     (:line-style nil))
		    (:frame ,opal:rectangle
		     (:left ,(o-formula (gvl :parent :background :left)))
		     (:top ,(o-formula (gvl :parent :background :top)))
		     (:width ,(o-formula (gvl :parent :background :width)))
		     (:height ,(o-formula (gvl :parent :background :height)))
		     (:visible nil))
		    (:label ,opal:cursor-multi-text ; opal:multifont-text
		     (:line-style ,(o-formula (gvl :parent :window :default-line-style)))
		     (:string ,(o-formula (if (gvl :parent :window :show-time)
					      (format nil "Time: ~,2fms" *real-time*)
					      "")))
		     (:left ,(o-formula (+ (the fn (gvl :parent :left)) *background-border-width*)))
		     (:top ,(o-formula (+ (the fn (gvl :parent :top)) *background-border-width*)))
		     (:font ,(o-formula (gvl :parent :window :font)))))))


(defun update-all-time-comments ()
  (loop for time-comment in (g-value time-comment :is-a-inv) do
	(opal::recompute-formula (g-value time-comment :label) :string)))

(defun remove-time-comment (&optional (win *standard-graphics-output*))
  (let ((top-time-comment (GET-TOP-LEVEL-THING win time-comment)))
    (when top-time-comment
      (opal:remove-component (g-value win :aggregate) top-time-comment)
      (opal:destroy top-time-comment))))

(defun add-time-comment (&optional (win *standard-graphics-output*))
  (when win
    (add-temp-comment win "")
    (or (GET-TOP-LEVEL-THING win time-comment)
	(let* ((agg (get-agg win))
	       (new-time-comment (create-instance nil time-comment
						  (:font (g-value win :temp-comment-font))
						  (:position :upper-left))))
	  (opal:add-component agg new-time-comment)
	  (s-value new-time-comment :frame :visible nil)
	  (s-value new-time-comment :visible t)
	  (g-value agg :left)
	  (s-value agg :left (loop for comp in (g-value agg :components) minimizing (g-value comp :left)))
	  (opal:update win t)
	  new-time-comment))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun load-window-with-cells (&optional (win *standard-graphics-output*) use-menu)
  (let* ((old-cells (g-value win :cells))
	 (dummy1 (g-value win :restrict-cells-to-cell-types))
	 (dummy2 (if dummy1 :cell-types :cells))
	 dummy3)
    (when use-menu
      (choose-variable-values '((dummy2 "Choose by:" :choose (:cell-types :cells))
				(dummy3 "Clear or add all cells from/to windows" :choose (:clear :fill)))
			      :label "Criterium for displaying cells")
      (case dummy3
	(:clear (s-value win :cells nil)
		(s-value win :cell-types nil))
	(:fill (s-value win :cells (cell-names))
	       (s-value win :cell-types (cell-type-names))))
      (s-value win :restrict-cells-to-cell-types (case dummy2 (:cell-types t) (t nil))))
    (s-value win :cells
	     (if (g-value win :restrict-cells-to-cell-types)
	       (flatten-list
		(loop for type in
		      (if use-menu
			(s-value win :cell-types (select-hash-values-menu (CELL-TYPE-HASH-TABLE) "Select cell types to draw"
									  :selected-list (g-value win :cell-types)))
			(g-value win :cell-types))
		      collect (names-of-things (cell-type-cells type))))
	       (if use-menu
		 (select-hash-values-menu (CELL-HASH-TABLE) "Select cells to draw"
					  :continue-prompt nil :selected-list (g-value win :cells))
		 (or old-cells (CELL-NAMES)))))
    (s-value win :update-anatomy (or (g-value win :update-anatomy) (not (subsetp old-cells (g-value win :cells)))))))

(defun min-max-xy-cell-element (win)
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (let ((min-x 0.0)
	(min-y 0.0)
	(max-x 0.0)
	(max-y 0.0)
	(first-time t))
    (declare (single-float min-x min-y max-x max-y))
    (loop for cell in (element (g-value win :cells) 'cell) do
	  (do ((cell-elements (cell-elements cell) (cdr cell-elements)))
	      ((null cell-elements))
	    (let* ((location (node-absolute-location ; (where (car cell-elements))
			      (typecase (car cell-elements)
				(soma (soma-node (car cell-elements)))
				(segment (segment-node-2 (car cell-elements))))))
		   (vpx (the sf (get-win-view-plane-x location win)))
		   (vpy (the sf (get-win-view-plane-y location win))))
	      (if first-time
		(setq min-x vpx
		      min-y vpy
		      max-x vpx
		      max-y vpy
		      first-time nil)
		(cond-every
		 ((> min-x vpx) (setq min-x vpx))
		 ((> min-y vpy) (setq min-y vpy))
		 ((< max-x vpx) (setq max-x vpx))
		 ((< max-y vpy) (setq max-y vpy))))))
	  finally (return (list min-x min-y max-x max-y)))))

(defun min-max-xy-coordinates (win type)
  (let ((vpx 0.0) (vpy 0.0)
	(locations (collect-locations-of-drawable-things (collect-drawable-things-w-connections win type))))
    (declare (single-float vpx vpy))
    (when locations
      (loop for location in locations
	    do (setq vpx (the sf (get-win-view-plane-x location win))
		     vpy (the sf (get-win-view-plane-y location win)))
	    minimize vpx into min-x
	    maximize vpx into max-x
	    minimize vpy into min-y
	    maximize vpy into max-y
	    finally (return (list min-x min-y max-x max-y))))))

(defun resize-histology-window (win &optional x-shift y-shift width height)
  (process-circuit-structure)		; Just in case.
  (let ((dummy3 x-shift)
	(dummy4 y-shift)	
	(dummy6 width)
	(dummy7 height)
	(dummy8 (g-value win :scale)))
    (case (g-value win :adjust-histology-window)
      (:automatic
       (loop for min-max-xy in
	     (list (when (and (are-there-light-synapses) (is-light-moving) (g-value win :draw-synapse-stimulus))
		     (draw-moving-thing nil t))
		   (min-max-xy-coordinates win 'axon)
		   (min-max-xy-coordinates win 'synapse)
		   (min-max-xy-cell-element win))
	     when min-max-xy
	     minimizing (nth 0 min-max-xy) into min-x-all
	     and minimizing (nth 1 min-max-xy) into min-y-all
	     and maximizing (nth 2 min-max-xy) into max-x-all
	     and maximizing (nth 3 min-max-xy) into max-y-all
	     finally
	     (when max-x-all
	       (setf dummy3 (* 0.5 (+ max-x-all min-x-all))
		     dummy4 (* 0.5 (+ max-y-all min-y-all))
		     ;; The factor of 1.2 to make the window a bit bigger than the cell(s).
		     dummy6 (* 1.3 (max *minimum-cell-histo-x-span (- max-x-all min-x-all)))
		     dummy7 (+ (* *HISTOLOGY-WINDOW-MIN-HEIGHT-EXTRA dummy8) ; a little extra height
			       (* 1.3 (max *minimum-cell-histo-y-span (- max-y-all min-y-all))))))))	       
      (:menu
       (choose-variable-values
	'((dummy3 "Center of window along X direction [um]:" :number)
	  (dummy4 "Center of window along Y direction [um]:" :number)
	  (dummy6 "Histology window width [um]" :float)
	  (dummy7 "Histology window height [um]" :float))
	:label "Histology XY (center moves when cell is redrawn)")))
    (reset-histology-xfrm win dummy3 dummy4 dummy6 dummy7 dummy8)
    nil))


(defun histology-rendering-menu (&optional (win *standard-graphics-output*))
  (let (dummy1
	dummy2 (dummy3 (princ-to-string (g-value win :segment-default-color)))
	dummy4 dummy5
	(dummy6 (STRING-capitalize (OPAL-COLOR-TO-STRING (g-value win :background-color))))
	(dummy10 (g-value win :source-graphics))
	dummy11
	(dummy13 (g-value win :restrict-to-PROXIMAL-SEGMENT))
	(dummy14 (g-value win :draw-somas))
	(dummy15 (g-value win :where-somas-go))
	(dummy16 (g-value win :draw-anatomical-soma))
	(dummy17 (g-value win :where-segments-go))
	(dummy18 (g-value win :all-node-graphics))
	(dummy19 (g-value win :plotted-node-graphics))
	(dummy20 (g-value win :draw-electrodes))
	(dummy21 (g-value win :draw-extracellular-electrodes))
	;; (dummy22 (g-value win :colorize)) (dummy24 (g-value win :include-voltage-color-scale))
	dummy23
	(dummy24 (princ-to-string (g-value win :segment-color-shading)))
	(dummy25 (g-value win :show-time))
	menu-list)
    (unless dummy3 (setq dummy3 :black))
    (setq menu-list `((dummy18 "For all cell segments:" :x-choose (:mark :label) :label-left)
		      (dummy19 "For all plotted segments:" :x-choose (:mark :label) :label-left)
		      (dummy23 "Mark specific segments or disable/enable marking"  :boolean)
		      (dummy13 "Draw only proximal segments" :boolean)
		      (dummy14 "Draw soma(s)" :boolean)
		      (dummy15 "Position of somas relative to cell:" :choose (:front :back) :label-left)
		      ,(when (electrodes) `(dummy20 "Draw electrodes" :boolean))
		      ,(when (extracellular-electrodes) `(dummy21 "Draw extracellular electrodes" :boolean))
		      (*override-screen-max* "Override screen size limits on window" :boolean)
		      ;; (dummy17 "Position of segment relative to cell:" :choose (:front :back))
		      (dummy6 "Background color:" :choose ("Black" "White") :label-left)
		      (dummy3 "Default segment color:" :choose ,(mapcar 'princ-to-string *line-styles-colors*) :rank-margin 4)
		      (dummy24 "Segment shading:" :choose ,(mapcar 'princ-to-string *line-styles-shadings*) :rank-margin 6)
		      (dummy25 "Display simulation time" :boolean)
		      ;; (dummy22 "Enable colorization" :boolean)
		      ;; (dummy24 "When colorizing, include voltage scale" :boolean)
		      (dummy11 "Modify other element graphics" :boolean)))
    (cond-every
     ((loop for cell in (element (g-value win :cells) 'cell) when
	    (or (element-parameter (cell-soma cell) 'soma-outline)
		(element-parameter (cell-soma cell) 'soma-points))
	    do (return t))
      (push '(dummy16 "Draw anatomical soma(s) rendering [else circuit sphere]" :boolean) menu-list))
     ((are-there-axons) (push '(dummy4 "Axon graphics menu" :boolean) menu-list))
     ((are-there-voltage-synapses) (push '(dummy5 "Synapse connection graphics menu" :boolean) menu-list))
     ((are-there-sources) (push '(dummy10 "Current or voltage sources:" :x-choose (:Draw :label) :label-left) menu-list))
     ((are-there-synapses) (push '(dummy1 "Synapse location, RF graphics menu" :boolean) menu-list))
     ((are-there-channels) (push '(dummy2 "Channel graphics menu" :boolean) menu-list)))
    (choose-variable-values menu-list :label "Details of Cell Drawing")

    (let ((background-color (string-to-opal-color dummy6)))
      (s-value win :show-time dummy25)
      (cond-every
       (dummy4 (axon-graphics-menu win))
       (dummy5 (synapse-cxn-graphics-menu win))
       (dummy1 (synapse-graphics-menu win))
       (dummy2 (channel-graphics-menu win))
       (dummy11 (element-graphics-menu win))
       (dummy23 (mark-segment-chains-menu win)))
      (s-value win :update-marked-nodes (or (g-value win :update-marked-nodes)
					    dummy23
					    (not (equal (g-value win :all-node-graphics) dummy18))))
      (s-value win :update-plotted-nodes (or (g-value win :update-plotted-nodes)
					     (g-value win :update-marked-nodes)
					     (not (equal dummy19 (g-value win :plotted-node-graphics)))))
      (s-value win :update-electrodes (or (g-value win :update-electrodes)
					  (not (equal dummy20 (g-value win :draw-electrodes)))))
      (s-value win :update-sources (or (g-value win :update-sources)
				       (not (equal dummy10 (g-value win :source-graphics)))))
      ;; (s-value win :update-colorize (not (equal dummy22 (g-value win :colorize))))
      (s-value win :update-anatomy
	       (or (g-value win :update-anatomy)
		   (not (= (read-from-string dummy24) (g-value win :segment-color-shading)))
		   (not (equal (g-value win :background-color) background-color))
		   ;; (not (equal dummy24 (g-value win :include-voltage-color-scale)))
		   (not (equal dummy21 (g-value win :draw-extracellular-electrodes)))
		   (not (equal dummy16 (g-value win :draw-anatomical-soma)))
		   (not (equal (read-from-string dummy3) (g-value win :segment-default-color)))
		   (not (equal dummy13 (g-value win :restrict-to-PROXIMAL-SEGMENT)))
		   (xor dummy14 (g-value win :draw-somas))
		   (not (equal (g-value win :where-segments-go) dummy17))
		   (not (equal dummy15 (g-value win :where-somas-go)))))
      (s-value win :segment-color-shading (s-flt (read-from-string dummy24)))
      (s-value win :background-color background-color)
      (s-value win :default-graphics-color (default-graphics-color win))
      (when (s-value win :restrict-to-PROXIMAL-SEGMENT dummy13)
	(setq dummy13 (if (numberp (g-value win :PROXIMAL-SEGMENT-level))
			(g-value win :PROXIMAL-SEGMENT-level)
			1))
	(choose-variable-values '((dummy13 "How many levels of proximal segments:" :number)) :label "Draw only proximal segments")
	(s-value win :proximal-segment-level dummy13))
      (s-value win :segment-default-color (read-from-string dummy3))
      (s-value win :source-graphics dummy10)
      (s-value win :where-segments-go dummy17)
      (s-value win :draw-anatomical-soma dummy16)
      (s-value win :where-somas-go dummy15)
      (s-value win :draw-somas dummy14)
      (s-value win :draw-electrodes dummy20)
      (s-value win :draw-extracellular-electrodes dummy21)
      (s-value win :all-node-graphics dummy18) 
      (s-value win :plotted-node-graphics dummy19)
      ;; (s-value win :colorize dummy22) (s-value win :include-voltage-color-scale dummy24)
      ))) 


(defun axon-graphics-menu (&optional (win *standard-graphics-output*))
  (let* (dummy1
	 (dummy2 (g-value win :axon-color-from-synapse))
	 (dummy3 *axon-graphics-diameter*)
	 (dummy4 (g-value win :use-connection-midpoints))
	 (dummy16 (g-value win :where-axons-go))
	 (dummy20 (princ-to-string (g-value win :axon-color)))
	 (dummy21 (princ-to-string (g-value win :axon-color-shading)))
	 (dummy24 (g-value win :restrict-axons-to-cells))
	 (dummy25 (> (length (g-value win :restrict-axons-to-synapse-types)) 0))
	 (dummy28 (g-value win :draw-axons)))
    (choose-variable-values
     `((dummy28 "Draw axons" :boolean)
       (dummy3 "Axon graphics diameter [um]" :float)
       (dummy16 "Position of axons relative to cell -" :choose (:front :back))
       (dummy4 "Draw with midpoints" :boolean)
       (dummy2 "Get axon color from target synapse" :boolean)
       (dummy20 "Axon default color" :choose ,(mapcar 'princ-to-string *line-styles-colors*) :rank-margin 4)
       (dummy21 "Axon shading" :choose ,(mapcar 'princ-to-string *line-styles-shadings*) :rank-margin 6)
       (dummy24 "Restrict axons to drawn cells" :boolean)
       (dummy25 "Restrict axons to synapse types" :boolean)
       (dummy1 "Edit viewed target synapse types" :boolean))
     :label "Setting Up Axon Graphics")
    (when dummy1
      (let ((types
	     (loop for name in 
		   (select-hash-values-menu (SYNAPSE-TYPE-HASH-TABLE)
					    "Select types of target synapses for plotted axons"
					    :inclusion-key #'(lambda (type)
							       (case (synapse-type-control type)
								 ((voltage channel) t)))
					    :selected-list (loop for type
								 in (g-value win :restrict-axons-to-synapse-types)
								 collect (element-name type)))
		   collect (element name))))
	(s-value win :update-axons (or (g-value win :update-axons) (not (equal types (g-value win :restrict-axons-to-synapse-types)))))
	(s-value win :restrict-axons-to-synapse-types types)))
    (s-value win :update-axons
	     (or (g-value win :update-axons)
		 (xor dummy4 (g-value win :use-connection-midpoints))
		 (not (= (read-from-string dummy21)  (g-value win :axon-color-shading)))
		 (not (= dummy3 *axon-graphics-diameter*))
		 (xor (g-value win :axon-color-from-synapse) dummy2)
		 (xor dummy28 (g-value win :draw-axons))
		 (not (equal (g-value win :where-axons-go) dummy16))
		 (not (equal (read-from-string dummy20) (g-value win :axon-color)))
		 (xor dummy24 (g-value win :restrict-axons-to-cells))))
    (s-value win :axon-color-shading (read-from-string dummy21))
    (setq *axon-graphics-diameter* dummy3)
    (s-value win :use-connection-midpoints dummy4)
    (s-value win :axon-color-from-synapse dummy2)
    (s-value win :draw-axons dummy28)
    (s-value win :restrict-axons-to-cells dummy24)
    (s-value win :where-axons-go dummy16)
    (s-value win :axon-color (read-from-string dummy20))))

(defun synapse-cxn-graphics-menu (&optional (win *standard-graphics-output*))
  (let* (dummy1
	 (dummy2 (g-value win :synapse-cxn-color-from-synapse))
	 (dummy3 *synapse-cxn-graphics-diameter*)
	 (dummy4 (g-value win :use-connection-midpoints))
	 (dummy16 (g-value win :where-synapse-cxns-go))
	 (dummy20 (princ-to-string (g-value win :synapse-cxn-color)))
	 (dummy21 (princ-to-string (g-value win :synapse-cxn-color-shading)))
	 (dummy24 (g-value win :restrict-synapse-cxns-to-cells))
	 (dummy25 (> (length (g-value win :restrict-synapse-cxns-to-synapse-types)) 0))
	 (dummy28 (g-value win :draw-synapse-cxns)))
    (choose-variable-values
     `((dummy28 "Draw synapse connections" :boolean)
       (dummy3 "Synapse connection graphics diameter [um]" :float)
       (dummy4 "Draw with midpoints" :boolean)
       (dummy16 "Position of synapse connections relative to cell -" :choose (:front :back))
       (dummy2 "Get synapse-cxn color from target synapse" :boolean)
       (dummy20 "Synapse-Cxn default color" :choose ,(mapcar 'princ-to-string *line-styles-colors*) :rank-margin 4)
       (dummy21 "Synapse-Cxn shading" :choose ,(mapcar 'princ-to-string *line-styles-shadings*) :rank-margin 6)
       (dummy24 "Restrict synapse connections to drawn cells" :boolean)
       (dummy25 "Restrict synapse connections to synapse types" :boolean)
       (dummy1 "Edit viewed target synapse types" :boolean))
     :label "Setting Up Synapse-Cxn Graphics")
    (when dummy1
      (let ((types
	     (loop for name in 
		   (select-hash-values-menu (SYNAPSE-TYPE-HASH-TABLE)
					    "Select types of target synapses for plotted synapse-cxns"
					    :inclusion-key #'(lambda (type)
							       (case (synapse-type-control type)
								 ((voltage channel) t)))
					    :selected-list (loop for type
								 in (g-value win :restrict-synapse-cxns-to-synapse-types)
								 collect (element-name type)))
		   collect (element name))))
	(s-value win :update-synapse-cxns
		 (or (g-value win :update-synapse-cxns)
		     (not (equal types (g-value win :restrict-synapse-cxns-to-synapse-types)))))
	(s-value win :restrict-synapse-cxns-to-synapse-types types)))
    (s-value win :update-synapse-cxns
	     (or (g-value win :update-synapse-cxns)
		 (xor dummy4 (g-value win :use-connection-midpoints))
		 (and (numberp (g-value win :synapse-cxn-color-shading))
		      (numberp (read-from-string dummy21))
		      (not (= (read-from-string dummy21) (g-value win :synapse-cxn-color-shading))))
		 (not (= dummy3 *synapse-cxn-graphics-diameter*))
		 (xor (g-value win :synapse-cxn-color-from-synapse) dummy2)
		 (xor dummy28 (g-value win :draw-synapse-cxns))
		 (not (equal (g-value win :where-synapse-cxns-go) dummy16))
		 (not (equal (read-from-string dummy20) (g-value win :synapse-cxn-color)))
		 (xor dummy24 (g-value win :restrict-synapse-cxns-to-cells))))
    (s-value win :synapse-cxn-color-shading (read-from-string dummy21))
    (setq *synapse-cxn-graphics-diameter* dummy3)
    (s-value win :use-connection-midpoints dummy4)
    (s-value win :synapse-cxn-color-from-synapse dummy2)
    (s-value win :draw-synapse-cxns dummy28)
    (s-value win :restrict-synapse-cxns-to-cells dummy24)
    (s-value win :where-synapse-cxns-go dummy16)
    (s-value win :synapse-cxn-color (read-from-string dummy20)))) 


(defun synapse-graphics-menu (&optional (win *standard-graphics-output*))
  (let (reset-draw-all-synapse-rfs-connections
	reset-label-all-synapses
	reset-draw-all-synapse-rfs
	update-flag original-label-flag
	(dummy1 (g-value win :label-all-synapses))
	(dummy2 (g-value win :draw-marked-synapses))
	dummy3
	(dummy4 *syn-rf-connection-thickness*)
	(dummy5 *syn-rf-connection-dash*)
	dummy6
	dummy7
	(dummy8 (or (g-value win :where-synapse-stimulus-goes) *WHERE-SYNAPSE-STIMULUS-GOES*))
	dummy10 dummy11
	(dummy12 (g-value win :draw-all-synapse-rfs))
	(dummy13 (or (g-value win :synapse-rf-height) 100.0))
	(dummy14 (g-value win :draw-all-synapse-rfs-connections))
	(dummy16 (g-value win :use-same-synapse-rfs-height))
	dummy17 dummy18 (dummy19 *motion-snapshots*)
	(dummy20 (g-value win :draw-synapse-rfs))
	(dummy21 *label-stimulus-times*))
    (choose-variable-values
     `((dummy2 "Enable synapse markers" :boolean)
       ,(if dummy1 `(dummy3 "Clear all synapse markers" :boolean) `(dummy1 "Mark all synapses" :boolean))
       (dummy11 "Edit synapse marker colors" :boolean)
       (dummy10 "Set individual synapse type parameters" :boolean)
       (dummy20 "Enable synapse RFs and connections" :boolean)
       ,(if dummy12 `(dummy17 "Clear all synapse RFs" :boolean) `(dummy12 "Draw all synapse RFs" :boolean))
       (dummy16 "Use same synapse RF height above cells" :boolean)
       ,(if dummy14 `(dummy18 "Clear all RF connections" :boolean) `(dummy14 "Include all RF connections" :boolean))
       ,(when (are-there-light-synapses) `(:comment "Light stimulus graphics"))
       ,(when (is-light-moving)
	  `(dummy19 "Number of moving stimulus snapshots" :integer)
	  `(dummy21 "If moving stimulus drawn, label times of stimulus snapshots" :boolean))
       ,(when (are-there-light-synapses)
	  `(dummy7 "View in plane of synapse stimulus (automatic if stimulus shown)" :boolean)
	  `(dummy8 "Position of stimulus drawing relative to cell:" :choose (:front :back) :label-left))
       (dummy6 "Some more details" :boolean))
     :label "Setting Up Synapse Graphics")
    (when dummy6
      (setq dummy5 (CHOOSE-DASH-PATTERNS dummy5 "Dash Pattern for Synapse RF Connections"))
      (choose-variable-values
       '((*SYN-RF-SHAPE-SHADING* "Synapse RF shape shading (0 => transparent)" :float)
	 (dummy13 "Common synapse RF height above cells" :float)
	 (*syn-rf-connection-shading* "Shading of RF connections (percent)" :float)
	 (dummy4 "Thickness of RF connections" :float))
       :label "Synapse RF Graphics Details"))
    (setq update-flag (or dummy11
			  (not (eq dummy8 (g-value win :where-synapse-stimulus-goes)))
			  (xor dummy21 *label-stimulus-times*)
			  (xor dummy19 *motion-snapshots*)
			  (and (or (g-value win :draw-synapse-stimulus) dummy7)
			       (case *light-stimulus-plane*
				 (:xy (or (not (= 0.0 (rad-to-deg (g-value win :phi))))
					  (not (= 0.0 (rad-to-deg (g-value win :theta))))))
				 (:xz (or (not (= 0.0 (rad-to-deg (g-value win :phi))))
					  (not (= 90.0 (rad-to-deg (g-value win :theta))))))))
			  (not (eq dummy4 *syn-rf-connection-thickness*))
			  (not (eq dummy5 *syn-rf-connection-dash*))
			  (xor dummy20 (g-value win :draw-synapse-rfs))
			  (xor dummy1 (g-value win :label-all-synapses)) dummy3
			  (xor dummy2 (g-value win :draw-marked-synapses))
			  (xor dummy12 (g-value win :draw-all-synapse-rfs)) dummy17
			  (not (= dummy13 (if (g-value win :synapse-rf-height) (g-value win :synapse-rf-height) 100.0)))
			  (xor dummy14 (g-value win :draw-all-synapse-rfs-connections)) dummy18
			  (xor dummy16 (g-value win :use-same-synapse-rfs-height))))
    (s-value win :where-synapse-stimulus-goes dummy8) 
    (setq  *motion-snapshots* dummy19
	   *label-stimulus-times* dummy21
	   *syn-rf-connection-thickness* dummy4
	   *syn-rf-connection-dash* dummy5)
    (when dummy7
      (case *light-stimulus-plane*
	(:xy (set-histology-window-angle-scale-parameters win (deg-to-rad 0.0) (deg-to-rad 0.0) (g-value win :scale)))
	(:xz (set-histology-window-angle-scale-parameters win (deg-to-rad 90.0) (deg-to-rad 0.0) (g-value win :scale)))))
    (s-value win :synapse-rf-height dummy13)
    (s-value win :use-same-synapse-rfs-height dummy16)
    (let ((synapse-type-graphics-parameters 
	   (loop for type in (synapse-types) when (element-in-circuit type)
		 collect
		 (multiple-value-bind (dummy5 dummy7 dummy8 dummy9)
		     (loop for type-info in (g-value win :synapse-type-graphics-parameters)
			   when (eq type (cdr-assoc 'type type-info))
			   do (return (values (cdr-assoc 'label-type type-info)
					      (cdr-assoc 'height type-info)
					      (cdr-assoc 'draw-rf-shape type-info)
					      (cdr-assoc 'draw-rf-connections type-info)))
			   finally (return (values nil (g-value win :synapse-rf-height) nil nil)))
		   (setq original-label-flag dummy5)
		   (cond (dummy1 (setq dummy5 t))
			 (dummy3 (setq dummy5 nil)))
		   (cond (dummy12 (setq dummy8 t))
			 (dummy17 (setq dummy8 nil)))
		   (cond (dummy14 (setq dummy9 t))
			 (dummy18 (setq dummy9 nil)))
		   (when dummy16 (setq dummy7 dummy13))
		   (let ((orig-height dummy7)
			 (orig-rf-enable dummy8)
			 (orig-rf-conn-enable dummy9)
			 (menu-list `((dummy5 ,(format nil "Label synapse type ~a" (synapse-type-name type)) :boolean))))
		     (case (synapse-type-control type)
		       (light
			(push '(dummy7 "RF height above cells" :float) menu-list)
			(push '(dummy8 "Draw synapse type RF" :boolean) menu-list)
			(push '(dummy9 "Draw RF connections to cells" :boolean) menu-list))
		       (setq dummy8 nil dummy9 nil))
		     (when dummy10 
		       (choose-variable-values menu-list :label (format nil "Graphics Options for Synapse Type ~a" (synapse-type-name type))))
		     (setq update-flag (or update-flag
					   (xor original-label-flag dummy5)
					   (not (= orig-height dummy7))
					   (xor orig-rf-enable dummy8)
					   (xor orig-rf-conn-enable dummy9)))
		     (setq reset-label-all-synapses (not dummy5)
			   reset-draw-all-synapse-rfs (not dummy8)
			   reset-draw-all-synapse-rfs-connections (not dummy9))		     
		     `((type . ,type) (label-type . ,dummy5) (height . ,dummy7) (draw-rf-shape . ,dummy8) (draw-rf-connections . ,dummy9)))))))
      (s-value win :synapse-type-graphics-parameters synapse-type-graphics-parameters))
    (when dummy11 (update-synapse-type-colors win))
    (s-value win :label-all-synapses (unless (or reset-label-all-synapses dummy3) dummy1))
    (s-value win :draw-all-synapse-rfs (unless (or reset-draw-all-synapse-rfs dummy17) dummy12))
    (s-value win :draw-all-synapse-rfs-connections (unless (or reset-draw-all-synapse-rfs-connections dummy18) dummy14))
    (s-value win :draw-synapse-rfs dummy20)
    (s-value win :draw-marked-synapses dummy2)
    (s-value win :update-synapse-stimulus update-flag)
    (s-value win :update-synapse-rfs (or (g-value win :update-synapse-rfs) update-flag))
    (s-value win :update-marked-synapses (or (g-value win :update-marked-synapses) update-flag))))

(defun channel-graphics-menu (&optional (win *standard-graphics-output*))
  (let ((dummy1 (g-value win :label-all-channels))
	(dummy2 (g-value win :draw-marked-channels))
	dummy3
	reset-label-all-channels
	update-flag original-label-flag
	dummy10 dummy11)
    (choose-variable-values
     `((dummy2 "Enable channel labels" :boolean)
       ,(if dummy1 `(dummy3 "Clear all channel labels" :boolean) `(dummy1 "Label all channels on cells" :boolean))
       (dummy10 "Set individual type parameters" :boolean)
       (dummy11 "Edit channel colors" :boolean))
     :label "Setting Up Channel Graphics")
    (setq update-flag (or dummy11
			  (xor dummy1 (g-value win :label-all-channels))
			  dummy3
			  (xor dummy2 (g-value win :draw-marked-channels))))
    (let ((channel-type-graphics-parameters 
	   (loop for type in (channel-types) when (element-in-circuit type) collect
		 (multiple-value-bind (dummy5)
		     (loop for type-info in (g-value win :channel-type-graphics-parameters)
			   when (eq type (cdr-assoc 'type type-info))
			   do (return (values (cdr-assoc 'label-type type-info)))
			   finally (return (values nil)))
		   (setq original-label-flag dummy5)
		   (when dummy1 (setq dummy5 t))
		   (when dummy3 (setq dummy5 nil))
		   (let ((menu-list `((dummy5 ,(format nil "Label channel type ~a" (channel-type-name type)) :boolean))))
		     (when dummy10 
		       (choose-variable-values menu-list :label (format nil "Graphics Options for Channel Type ~a" (channel-type-name type))))
		     (setq update-flag (or update-flag (xor original-label-flag dummy5)))
		     (setq reset-label-all-channels (not dummy5))
		     `((type . ,type) (label-type . ,dummy5)))))))
      (s-value win :channel-type-graphics-parameters channel-type-graphics-parameters))
    (when dummy11 (update-channel-type-colors win))
    (s-value win :label-all-channels (unless (or reset-label-all-channels dummy3) dummy1))
    (s-value win :draw-marked-channels dummy2)
    (s-value win :update-marked-channels update-flag)))


(defun element-graphics-menu (&optional (win *standard-graphics-output*))
  (when win
    (let (dummy1
	  (dummy2 (g-value win :node-label-background-p))
	  dummy3
	  (dummy4 (g-value win :where-element-markers-go))
	  (dummy5 (g-value win :suppress-element-marker-borders))
	  (dummy6 (g-value win :grape-size-reference))
	  (dummy7 (if (g-value win :grape-size-microns) (g-value win :grape-size-microns) 10.0))
	  (dummy8 (if (g-value win :grape-size-pixels) (g-value win :grape-size-pixels) 10))
	  (dummy9 (g-value win :include-element-key-window))
	  dummy10 dummy11
	  (dummy15 (or (g-value win :label-x-offset) 0))
	  (dummy16 (g-value win :view-angle-comment-p))
	  dummy18
	  (dummy20 (or (g-value win :marker-diameter) 10))
	  (menu-list
	   ;; (*node-graphics-coefficient* "Coefficient for element marker size" :float)
	   `((dummy1 "Edit window font" :boolean)
	     (dummy4 "Position of element markers relative to cell:" :choose (:front :back) :label-left)
	     (dummy9 "Include element key window" :boolean)
	     (dummy2 "Add background to node labels" :boolean)
	     (dummy15 "Label X offset [pixels]" :integer)
	     (dummy20 "Segment/soma marker diameter" :integer)
	     (dummy5 "Suppress drawing of element marker borders" :boolean)
	     (dummy6 "Reference for element marker size:" :choose (:microns :pixel) :label-left)
	     (dummy7 "Relative size for element markers [microns]" :float)
	     (dummy8 "Pixel size for element markers" :integer)
	     ,(when (g-value win :markers) '(dummy3 "Edit marked points" :boolean))
	     ,(when (g-value win :markers) '(dummy10 "Edit marked points font" :boolean))

	     (dummy11 "Edit scale bar" :boolean)
	     (dummy18 "Edit default line style (e.g. for scale bar)" :boolean)
	     (dummy16 "Include view angle" :boolean))))	     
      (choose-variable-values menu-list :label "Histology Details....")
      (when dummy18
	(s-value win :default-line-style-base (line-style-menu :default-style (g-value win :default-line-style)
							       :label (format nil "~A Default Line Style" (g-value win :title)))))
      (cond-every
       (dummy11 (edit-histology-scale-bar win))
       (dummy3 (mark-coords-pointer-menu win))
       (dummy10 (ph::Edit-marked-points-font win))
       (dummy1 (s-value win :comment-font (s-value win :font (font-menu (g-value win :font) (g-value win :title))))))
      (let ((update-element-graphics
	     (or dummy11
		 (not (= dummy20 (or (g-value win :marker-diameter) 10)))
		 (not (= dummy15 (g-value win :label-x-offset)))

		 (xor dummy2 (g-value win :node-label-background-p))
		 (xor dummy11 (g-value win :include-scale-bar))
		 (xor (g-value win :suppress-element-marker-borders) dummy5)
		 (not (equal (g-value win :where-element-markers-go) dummy4))
		 (not (eq (g-value win :grape-size-reference) dummy6))
		 (not (= (g-value win :grape-size-microns) dummy7))
		 (not (= (g-value win :grape-size-pixels) dummy8)))))
	(s-value win :marker-diameter dummy20)
	(s-value win :label-x-offset dummy15)
	(s-value win :view-angle-comment-p dummy16)
	(s-value win :update-marked-nodes (or (g-value win :update-marked-nodes) update-element-graphics))
	(s-value win :update-marked-channels (or (g-value win :update-marked-channels) update-element-graphics))
	(s-value win :update-marked-synapses (or (g-value win :update-marked-synapses) update-element-graphics))
	(s-value win :update-plotted-nodes (or (g-value win :update-plotted-nodes) update-element-graphics))
	(s-value win :update-element-key-window (or (xor dummy9 (g-value win :element-key-window))
						    (xor dummy9 (g-value win :include-element-key-window))))
	(s-value win :include-element-key-window dummy9)
	(s-value win :node-label-background-p dummy2)
	(s-value win :suppress-element-marker-borders dummy5)
	(s-value win :where-element-markers-go dummy4)
	(s-value win :grape-size-reference dummy6)
	(s-value win :grape-size-microns dummy7)
	(s-value win :grape-size-pixels dummy8)))
    nil))

(defun edit-histology-scale-bar (win)
  (let ((dummy11 (g-value win :include-scale-bar))
	(dummy12 (not (g-value win :auto-histology-scale-bar-um-length)))
	(dummy13 (g-value win :histology-scale-bar-um-length))
	(dummy17 (g-value win :fix-histology-scale-bar-length))
	(dummy19 (g-value win :visible-histology-scale-bar-sides))
	(dummy21 (g-value win :include-histology-scale-bar-label)))
    (choose-variable-values
     '((dummy11 "Include scale bar" :boolean)
       (dummy12 "Disable automatic scale bar length" :boolean)
       (dummy13 "Length of scale bar [um]" :number)
       (dummy17 "Fix histology scale bar length" :boolean)
       (dummy19 "Include histology scale bar sides" :boolean)
       (dummy21 "Include histology scale bar label" :boolean))
     :label "Histology Scale Bar Details....")
    (s-value win :include-scale-bar dummy11)
    (s-value win :auto-histology-scale-bar-um-length (not dummy12))
    (s-value win :histology-scale-bar-um-length dummy13)
    (s-value win :fix-histology-scale-bar-length dummy17)
    (s-value win :visible-histology-scale-bar-sides dummy19)
    (s-value win :include-histology-scale-bar-label dummy21))
  nil)


      
(defun mark-nodes-menu (&optional (win *standard-graphics-output*))
  (let (dummy1 (dummy2 "") branches)
    (choose-variable-values
     '((dummy1 "Choose branch entered below (T) or use menu" :boolean)
       (dummy2 "Branch name" :string))
     :label "Choosing Cell Branch")
    (setq branches
	  (if dummy1 (list dummy2)
	      (choose-list-values (loop for branch in *branch-list* collect (element-name (car (last branch))))
				  (loop for segco in (g-value win :marked-segments-and-colors)
					collect (element-name (caar segco)))
				  :label "Select branch(s) to display")))
    (s-value win :marked-segments-and-colors
	     (loop for name-color
		   in (color-CHOOSE-BUTTON-menu
		       (loop for branch in branches
			     when (setq dummy1 (find (name-element branch) (g-value win :marked-segments-and-colors) :key `caar))
			     collect (list branch (cadr dummy1))
			     else collect (list branch nil))
		       "Assigning Colors to Branches" "Branches (proximal node)"
		       :xclusive-choice nil)
		   when (cadr name-color)
		   collect (list (get-branch-elements (car name-color) 'segment) (cadr name-color))))))

(defun mark-segment-chains-menu (&optional (win *standard-graphics-output*) chosen-one)
  (when chosen-one (s-value win :chosen-one chosen-one))
  (let (dummy1
	(dummy2 (or (when (segment-p (element (g-value win :chosen-one))) (element-name (g-value win :chosen-one))) ""))
	(dummy3 (g-value win :disable-segment-chain-marking))
	dummy4 dummy5 (dummy6 t) dummy7 dummy8 
	(dummy14 (princ-to-string (g-value win :segment-color-shading))))
    (choose-variable-values
     `((dummy2 "Reference for segment" :string)
       (dummy8 "Label reference" :boolean)
       (dummy4 "Mark distal segments starting from reference" :boolean)
       (dummy5 "Mark proximal segments starting from reference" :boolean)
       (dummy3 "Disable segment chain marking" :boolean)
       (dummy7 "Clear any previously marked segments" :boolean)
       (dummy14 "Segment shading:" :choose ,(mapcar 'princ-to-string *line-styles-shadings*) :rank-margin 6))
     :title (format nil "~A: Choose Cell Segments for Marking" (g-value win :title)))
    (when dummy8 (label-element dummy2 win nil))
    (s-value win :disable-segment-chain-marking dummy3)
    (when dummy7
      (s-value win :marked-segments-and-colors nil)
      (s-value win :labeled-elements nil))
    (s-value win :segment-color-shading (s-flt (read-from-string dummy14)))
    (unless dummy3
      (let* ((reference-segment (element-cell-element dummy2 'segment))
	     (keys-colors
	      (when (segment-p reference-segment)
		(color-CHOOSE-BUTTON-menu
		 (no-nils (list (element-name reference-segment)
				(when dummy4 (format nil "~A: Distal segments" (element-name reference-segment)))
				(when dummy5 (format nil "~A: Proximal segments" (element-name reference-segment)))))
		 (format nil "Color for Marked Segments Referenced by ~A" (element-name reference-segment))
		 "" :xclusive-choice nil))))
	(when reference-segment
	  (loop for chain in (no-nils (list (list reference-segment)
					    (when dummy4 (remove reference-segment (SEGMENTS-OUT reference-segment)))
					    (when dummy5 (remove reference-segment (SEGMENTS-in reference-segment)))))
		for key-color in keys-colors do
		(update-win-marked-segments-and-colors win (list chain (or (cadr key-color) (get-opal-color 'red))))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; ELEMENT-TYPE-SYM should be 'synapse-type or 'channel-type ....
(defun update-type-colors (element-type-sym &optional win)
  (let ((types (case element-type-sym
		 (synapse-type (synapse-types))
		 (channel-type (channel-types)))))
    (loop for name-color in
	  (color-CHOOSE-BUTTON-menu
	   (loop for type in types
		 when (or (not win) (instance-in-cell type element-type-sym (element (g-value win :cells) 'cell)))
		 collect (list (element-name type) (element-parameter type 'color)))
	   (format nil "Assigning Colors to ~As" element-type-sym)
	   (format nil "~As" element-type-sym) :xclusive-choice nil)
	  when (cadr name-color) do (element-parameter (element (car name-color) element-type-sym) 'color (cadr name-color)))))

(defun update-synapse-type-colors (&optional win)
  (update-type-colors 'synapse-type win))

(defun update-channel-type-colors (&optional win)
  (update-type-colors 'channel-type win))

(defun init-histology-window-draw-slots (win &optional parent-win)
  (s-value win :complete-update t)
  (when parent-win
    (s-value win :labeled-elements (g-value parent-win :labeled-elements))
    (s-value win :where-element-markers-go (g-value parent-win :where-element-markers-go))

    (s-value win :restrict-axons-to-cells (g-value parent-win :restrict-axons-to-cells))
    (s-value win :restrict-axons-to-synapse-types (g-value parent-win :restrict-axons-to-synapse-types))

    (s-value win :restrict-synapse-cxns-to-cells (g-value parent-win :restrict-synapse-cxns-to-cells))
    (s-value win :restrict-synapse-cxns-to-synapse-types (g-value parent-win :restrict-synapse-cxns-to-synapse-types))
    
    (s-value win :where-somas-go (g-value parent-win :where-somas-go))
    (s-value win :where-segments-go (g-value parent-win :where-segments-go))
    (s-value win :where-axons-go (g-value parent-win :where-axons-go))

    (s-value win :where-synapse-cxns-go (g-value parent-win :where-synapse-cxns-go))

    (s-value win :segment-color (g-value parent-win :segment-color))

    (s-value win :restrict-cells-to-cell-types (g-value parent-win :restrict-cells-to-cell-types))
    (s-value win :cells (g-value parent-win :cells))
    (s-value win :cell-types (g-value parent-win :cell-types))
    (s-value win :channel-type-graphics-parameters (g-value parent-win :channel-type-graphics-parameters))
    (s-value win :synapse-type-graphics-parameters (g-value parent-win :synapse-type-graphics-parameters))
    (s-value win :grape-size-microns (g-value parent-win :grape-size-microns))
    (s-value win :grape-size-pixels (g-value parent-win :grape-size-pixels))
    (s-value win :grape-size-reference (g-value parent-win :grape-size-reference))

    (s-value win :disable-segment-chain-marking (g-value parent-win :disable-segment-chain-marking))
    )
  (s-value win :auto-histology-scale-bar-um-length
	   (if parent-win (g-value parent-win :auto-histology-scale-bar-um-length) t))
  (s-value win :segment-color-shading (if parent-win (g-value parent-win :segment-color-shading) 100.0))
  ;;  (s-value win :draw-anatomical-soma (if parent-win (s-value parent-win :draw-anatomical-soma) t))

  (s-value win :histology-scale-bar-um-length
	   (if parent-win (g-value parent-win :histology-scale-bar-um-length) 100.0))
  
  (s-value win :include-scale-bar (if parent-win (g-value parent-win :include-scale-bar) t))
  (s-value win :adjust-histology-window (if parent-win (g-value parent-win :adjust-histology-window) :automatic))
  (s-value win :draw-somas (if parent-win (g-value parent-win :draw-somas) t))
  (s-value win :draw-axons (if parent-win (g-value parent-win :draw-axons) t))
  (s-value win :axon-color-from-synapse (if parent-win (g-value parent-win :axon-color-from-synapse)))

  (s-value win :draw-synapse-cxns (if parent-win (g-value parent-win :draw-synapse-cxns) t))
  (s-value win :synapse-cxn-color-from-synapse (if parent-win (g-value parent-win :synapse-cxn-color-from-synapse)))

  (s-value win :draw-anatomy (if parent-win (g-value parent-win :draw-anatomy) t)) 
  (s-value win :draw-synapse-stimulus (if parent-win (g-value parent-win :draw-synapse-stimulus)))
  (s-value win :draw-synapse-rfs (if parent-win (g-value parent-win :draw-synapse-rfs)))
  (s-value win :plotted-node-graphics (if parent-win (g-value parent-win :plotted-node-graphics)))
  (s-value win :all-node-graphics (if parent-win (g-value parent-win :all-node-graphics)))
  (s-value win :draw-marked-synapses (if parent-win (g-value parent-win :draw-marked-synapses)))
  (s-value win :include-element-key-window (not parent-win))
  (s-value win :label-x-offset (if parent-win (g-value parent-win :label-x-offset) 0))
  (s-value win :draw-marked-channels (if parent-win (g-value parent-win :draw-marked-channels)))

  (s-value win :source-graphics (if parent-win (g-value parent-win :source-graphics) '(:draw))))

(defun unchoose-chosen-ones (&optional (wins *standard-graphics-output*))
  (loop for win in (coerce-to-list wins)
	when win do (progn
		      (add-temp-comment win "")
		      (draw-chosen-one nil win t)
		      (s-value win :chosen-one nil)
		      (opal::update win t)))
  nil)

(defun rechoose-chosen-ones (&optional (wins *standard-graphics-output*))
  (loop for win in (no-nils (coerce-to-list wins)) do (draw-chosen-one nil win) (opal::update win t))
  nil)


(defun draw-chosen-one (&optional chosen-ones (win *standard-graphics-output*) erase)
  (when chosen-ones
    (typecase chosen-ones
      (cons (s-value win :chosen-ones (loop for chosen-one in chosen-ones collect (element-cell-element chosen-one))))
      (t (s-value win :chosen-one (element-cell-element chosen-ones)))))
  (let* ((chosen-ones (or (g-value win :chosen-ones) (list (g-value win :chosen-one))))
	 (agg (clear-and-add-plot-agg  win `colored-node-agg :add (and (not erase) chosen-ones) :where :front)))
    (when agg
      (loop for chosen-one in chosen-ones do
	    (typecase chosen-one
	      (segment (draw-segment win chosen-one agg :color 'blue :shading *chosen-one-shading*))
	      (soma (draw-somas win t :soma-agg agg :color 'blue :target-soma chosen-one :shading *chosen-one-shading*)))))))

(defun mark-nodes (win draw &optional update)
  (let ((mark-agg (clear-and-add-plot-agg win `marked-nodes :add draw :where :front)))
    (when mark-agg
      (loop for segs-and-color in (g-value win :marked-segments-and-colors) do
	    (loop for seg in (car segs-and-color) do
		  (draw-segment win seg mark-agg :color (cadr segs-and-color) :shading 50)))))
  (when update (histology-window-finishing win)))

(defun mark-segment-chains (&optional (win *standard-graphics-output*) (draw t) (use-menu t) (update t))
  (when (and draw use-menu) (mark-segment-chains-menu win))
  (draw-segments win
                 (and draw
		      (not (g-value win :disable-segment-chain-marking))
		      (g-value win :marked-segments-and-colors))
                 'marked-segments)
  (when update (histology-window-finishing win)))
  
(defun update-win-marked-segments-and-colors (win new-chain-and-color)
  (let* (new-chain-and-color-is-a-replacement 
	 (cleaned-up-marked-segments-and-colors
	  (loop for chain-and-color in (g-value win :marked-segments-and-colors)
		collect	(if (= (length (intersection (car chain-and-color) (car new-chain-and-color))) (length (car chain-and-color)))
			  (setq new-chain-and-color-is-a-replacement new-chain-and-color)
			  chain-and-color))))
    (s-value win :marked-segments-and-colors
	     (if new-chain-and-color-is-a-replacement
	       cleaned-up-marked-segments-and-colors
	       (cons new-chain-and-color cleaned-up-marked-segments-and-colors)))))

(defun update-labeled-elements (&optional (win *standard-graphics-output*))
  (let ((top-agg (get-agg win)))
    (loop for comp in (g-value top-agg :components)
	  when (case (g-value comp :type)
		 (node-labels t))
	  do (opal::remove-component top-agg comp))
    (let ((original-labeled-elements (g-value win :labeled-elements)))
      (s-value win :labeled-elements nil)
      (label-element original-labeled-elements win nil))))


;; For both plotted and all nodes
(defun mark-plotted-nodes (win draw)
  (let ((label-agg (clear-and-add-plot-agg win `plotted-node-labels :add draw :where :front))
	(mark-agg (clear-and-add-plot-agg win `plotted-nodes :add draw :where :front)))
    (when draw
      (cond-every
       ((g-value win :all-node-graphics)
	(mark-segments-and-somas
	 (somas-and-segments (element (g-value win :cells) 'cell) nil nil)
	 :win win :type 'all-nodes :marker-diameter (or (g-value win :marker-diameter) 10) :mark-fill *marked-node-fill*
	 :mark-agg (when (member :mark (g-value win :all-node-graphics)) mark-agg)
	 :label-agg (when (member :label (g-value win :all-node-graphics)) label-agg)
	 :update nil))
       ((and (not (member :label (g-value win :all-node-graphics))) (g-value win :plotted-node-graphics))
	(mark-segments-and-somas
	 (plotted-somas-and-segments (element (g-value win :cells) 'cell)) ; Mark plotted nodes
	 :win win :type 'plotted-nodes
	 :mark-agg (when (member :mark (g-value win :plotted-node-graphics)) mark-agg)
	 :marker-diameter (or (g-value win :marker-diameter) 10) :mark-fill *plotted-node-fill* 
	 :label-agg (when (member :label (g-value win :plotted-node-graphics)) label-agg)
	 :update nil))))))

(defun plotted-somas-and-segments (&optional (cells (cells)))
  (somas-and-segments cells t nil))

(defun element-key-window (key-ref-win &key draw-channels draw-synapses add-key (key-diameter 15) (left-border 10))
  (let* ((win
	  (if (opal-obj-exists (g-value key-ref-win :element-key-window))
	    (g-value key-ref-win :element-key-window)
	    (s-value key-ref-win :element-key-window
		     (create-aux-histology-window :auxiliary-type :keys
						  :title (concatenate-strings (g-value key-ref-win :title) " Keys")))))
	 (element-label-key-agg (clear-and-add-plot-agg win `element-label-key
							:add (and add-key (or draw-channels draw-synapses))
							:where :front))
	 (key-font (opal:get-standard-font :serif :bold-italic :medium))
	 (key-step (+ 7 (max key-diameter (g-value key-font :font-height))))
	 (y 10)
	 (element-type-names-and-colors
	  (concatenate
	   'list
	   (when draw-channels
	     (loop for type-info in (g-value key-ref-win :channel-type-graphics-parameters)
		   when (cdr-assoc 'label-type type-info)
		   collect (list (format nil "Channel type ~A" (channel-type-name  (cdr-assoc 'type type-info))) 
				 (element-parameter (cdr-assoc 'type type-info) 'color))))
	   (when draw-synapses
	     (loop for type-info in (g-value key-ref-win :synapse-type-graphics-parameters)
		   when (cdr-assoc 'label-type type-info)
		   collect (list (format nil "Synapse type ~A" (synapse-type-name  (cdr-assoc 'type type-info))) 
				 (element-parameter (cdr-assoc 'type type-info) 'color)))))))
    (when element-label-key-agg
      (loop for element-type-name-and-color in element-type-names-and-colors
	    when (cadr element-type-name-and-color)
	    maximize
	    (g-value (opal:add-component element-label-key-agg (create-instance nil opal:text 
										(:visible t)
										(:top y)
										(:string (car element-type-name-and-color))
										(:left (o-formula (round (+ key-diameter left-border left-border))))
										(:font key-font)))
		     :width)
	    into max-text-width
	    and do
	    (opal:add-component element-label-key-agg (create-instance nil opal:circle
								       (:constant t)
								       (:left left-border)
								       (:top y)
								       (:height key-diameter)
								       (:width key-diameter)
								       (:filling-style (color-to-fill (cadr element-type-name-and-color)))))
	    (setq y (+ y key-step)) 
	    finally
	    (s-value win :width (+ max-text-width key-diameter left-border left-border left-border))
	    (s-value win :height y)))
    (s-value win :width (max (g-value win :width)
			     (g-value (create-instance nil OPal:text
						       (:string (concatenate-strings (g-value key-ref-win :title) " Element Keys"))
						       (:font (opal:get-standard-font :serif :bold-italic :medium)))
				      :width)))
    (s-value win :visible (and
			   (when element-label-key-agg (g-value element-label-key-agg :components))
			   add-key element-type-names-and-colors (or draw-channels draw-synapses)))
    (opal:update win))) 


(defun get-node-synapses-for-marking (node)
  (loop for syn in (get-node-elements-of-type node 'synapse t)
	unless (eq node (synapse-pre-synaptic-node syn))
	collect syn))

(defun grape-size (win)
  (* 2 (round (/  
	       (case (g-value win :grape-size-reference)
		 (:microns
		  (round
		   (/ (or (g-value win :grape-size-microns) *grape-size-microns*)
		      (g-value win :scale))))
		 (:pixel (or (g-value win :grape-size-pixels) *grape-size*))
		 (t *grape-size*))
	       2.0))))

(defun make-grapes (colors win x y)
  (create-instance nil GRAPES
		   ;; Assigning the window here kills on OPAL::ADD-COMPONENT-METHOD-AGGREGATE...
		   ;; (:window win)
		   (:grape-colors colors)
		   (:center-x x)
		   (:center-y y)))

(defun synapse-color (syn)
  (element-parameter (synapse-type syn) 'color))

(defun element-marker-diameter (element win)
  (setq element (element-cell-element element))
  (round (the sf (* *node-graphics-coefficient*
		    (/ (the sf (typecase element
				 (soma (soma-diameter element))
				 (segment (sqrt (the sf (* (segment-diameter element) (segment-length element)))))))
		       (the sf (g-value win :scale)))))))


#|
(defun point-out-node (agg)
  (let* ((cell (node-cell node))
	 ;; (cell-origin (cell-origin cell)) Offset a bit for soma node
	 (y-offset (if (equalp (node-relative-location node) (list 0.0 0.0 0.0))
		       (- (* 0.5 (soma-diameter (cell-soma cell)))) 0.0))
	 (pointer-components '()))
    (s-value histology-pointer :start-where (list :element-of agg))
    (s-value histology-pointer :window win)
    (push (label-cell-node node :x-offset 13 :y-offset (+ -16 y-offset))
	  pointer-components)
    (let* ((node-location (node-absolute-location node))
	   (view-plane-x (get-win-view-plane-x node-location win))
	   (view-plane-y (get-win-view-plane-y node-location win)))
      (push (add-arrow
	     (- view-plane-x 10.0) (+ view-plane-y -10.0 y-offset) 60.0 agg :thickness 3)
	    pointer-components)
      (push (add-ok-button win (g-value win :aggregate) :label "Press When Done")
	    pointer-components)
      (opal:update win)
      (inter:wait-interaction-complete)
      (remove-components-in-list pointer-components win)
      (opal:update win))))
|#

;; need these garnet functions.
(defun clear-histology-window ())

(defun draw-synapse-stimulus-ok (win)
  (and *enable-light*
       (ARE-THERE-LIGHT-SYNAPSES)
       (g-value win :draw-synapse-stimulus)
       (case *light-stimulus-plane*
	 (:xy (and (close-to (g-value win :cos-theta) 1.0)
		   (close-to (g-value win :cos-phi) 1.0)))
	 (:xz (and (close-to (g-value win :cos-theta) 0.0)
		   (close-to (g-value win :cos-phi) 1.0))))))
  
;;;; DRAW-SYNAPSE-STIMULUS
(defun draw-synapse-stimulus (win draw-stimulus)
  (let ((agg (clear-and-add-plot-agg win `light
				     :add (and draw-stimulus (draw-synapse-stimulus-ok win))
				     :where (or (g-value win :where-synapse-stimulus-goes)
						(if (or (eq :back *WHERE-SYNAPSE-STIMULUS-GOES*)
							(eq :front *WHERE-SYNAPSE-STIMULUS-GOES*))
						  *WHERE-SYNAPSE-STIMULUS-GOES*
						  :back)))))
    (when agg
      (case *light-stimulus*
	((:moving-bar :on-moving-bar :off-moving-bar :reversing-bar :moving-spot :on-moving-spot :off-moving-spot)
	 (draw-moving-thing agg))
	((:on-bar :off-bar :bar) (draw-bar agg))
	(:moving-bar-grating (draw-grating agg))	
	(:apparent-motion (draw-apparent-motion agg))
	((:on-spot :off-spot :spot) (draw-spot agg))
	(:annulus (draw-annulus agg)))
      (when *use-aperture* (draw-aperture agg)))))


(defun draw-annulus (agg)
  (add-circle *light-start-position-x* *light-start-position-y*
	      (/ *spot-outside-diameter* 2.0) agg
					; :halftone-percent *stimulus-graphic-shading-percent* :color 'blue
	      :drawing-function :and)
  (add-circle *light-start-position-x* *light-start-position-y*
	      (/ *spot-inside-diameter* 2.0) agg
					; :halftone-percent *stimulus-graphic-shading-percent* :color 'blue
	      :drawing-function :and)
  (draw-stimulus-time "Annulus" agg))

(defun draw-aperture (agg)
  (add-circle *aperture-center-x* *aperture-center-y*
	      *aperture-radius* agg
	      :line-style thin-dashed-2-blue-line
	      :drawing-function :and))

(defun draw-spot (agg)
  (if *fast-full-field-spot*
      (draw-stimulus-time "Full Field Spot" agg)
      (progn
	(add-circle *light-start-position-x* *light-start-position-y*
		    (/ *spot-outside-diameter* 2.0) agg
		    :halftone-percent *stimulus-graphic-shading-percent* :color 'blue
		    :drawing-function :and)
	(draw-stimulus-time "Spot" agg))))


;; X-ANATOMY, Y-ANATOMY Translate coordinates in the stimulus frame to the anatomical frame.
(defun x-anatomy (x-stimulus y-stimulus x-anatomy-shift theta) 
  (+ (+ (* (cos theta) x-stimulus)
	(* (sin (- theta)) y-stimulus))
     x-anatomy-shift))

(defun y-anatomy (x-stimulus y-stimulus y-anatomy-shift theta) 
  (+ (+ (* (sin theta) x-stimulus)
	(* (cos theta) y-stimulus))
     y-anatomy-shift))

;;; DRAW-APPARENT-MOTION
(defun draw-apparent-motion (agg)
  (add-line
   (x-anatomy (+ *bar-a-position-x* (/ *bar-a-length* 2)) *bar-a-position-y* *light-start-position-x* *light-theta*)
   (y-anatomy (+ *bar-a-position-x* (/ *bar-a-length* 2)) *bar-a-position-y* *light-start-position-y* *light-theta**)

   (x-anatomy (- *bar-a-position-x* (/ *bar-a-length* 2)) *bar-a-position-y* *light-start-position-x* *light-theta*)
   (y-anatomy (- *bar-a-position-x* (/ *bar-a-length* 2)) *bar-a-position-y* *light-start-position-y* *light-theta*)

   agg :stipple-percent *stimulus-graphic-shading-percent* :thickness *bar-a-width* :color `blue)
  (add-line
   (x-anatomy (+ *bar-b-position-x* (/ *bar-b-length* 2)) *bar-b-position-y* *light-start-position-x* *light-theta*)
   (y-anatomy (+ *bar-b-position-x* (/ *bar-b-length* 2)) *bar-b-position-y* *light-start-position-y* *light-theta*)

   (x-anatomy (- *bar-b-position-x* (/ *bar-b-length* 2)) *bar-b-position-y* *light-start-position-x* *light-theta*)
   (y-anatomy (- *bar-b-position-x* (/ *bar-b-length* 2)) *bar-b-position-y* *light-start-position-y* *light-theta*)
   agg :stipple-percent *stimulus-graphic-shading-percent* :thickness *bar-b-width* :color `blue)
  (draw-stimulus-time "Apparent Motion" agg)
  (add-string (format nil "A")
	      (x-anatomy *bar-a-position-x* *bar-a-position-y* *light-start-position-x* *light-theta*)
	      (y-anatomy *bar-a-position-x* *bar-a-position-y* *light-start-position-y* *light-theta*)
	      agg)
  (add-string (format nil "B")
	      (x-anatomy *bar-b-position-x* *bar-b-position-y* *light-start-position-x* *light-theta*)
	      (y-anatomy *bar-b-position-x* *bar-b-position-y* *light-start-position-y* *light-theta*)
	      agg))

(defun draw-bar (agg)
  (add-line
   (x-anatomy (/ *bar-length* 2) 0.0 *light-start-position-x* *light-theta*)
   (y-anatomy (/ *bar-length* 2) 0.0 *light-start-position-y* *light-theta*)
   (x-anatomy (/ *bar-length* -2) 0.0 *light-start-position-x* *light-theta*)
   (y-anatomy (/ *bar-length* -2) 0.0 *light-start-position-y* *light-theta*)
   agg
   :stipple-percent *stimulus-graphic-shading-percent*
   :thickness *bar-width* :color `blue)
  (draw-stimulus-time "Bar" agg))

(defun collect-snapshot-times ()
  (delete-duplicates
   (loop for i from 0 to (1- *motion-snapshots*) collect
	 (if (= *motion-snapshots* 1)
	     *light-stimulus-start-time*
	     (+ *light-stimulus-start-time*
		(truncate (* i (/ (- (min *light-stimulus-stop-time* *user-stop-time*)
				     *light-stimulus-start-time*)
				  (1- *motion-snapshots*)))))))))


(defun draw-moving-thing (agg &optional only-stimulus-sweep)
  (let (min-x min-y max-x max-y) 
    (dolist (snapshot-time (collect-snapshot-times))
      (let* ((y-translation-stim-frame
	      (cond ((>= snapshot-time *light-stimulus-stop-time*)
		     (* (if *light-direction* *light-speed* (- *light-speed*))
			(- *light-stimulus-stop-time* *light-stimulus-start-time*)))
		    ((< snapshot-time *light-stimulus-stop-time*)
		     (* (if *light-direction* *light-speed* (- *light-speed*))
			(- snapshot-time *light-stimulus-start-time*)))
		    (t 0.0)))
	     (x-translation (+ (* (sin (- *light-theta*)) y-translation-stim-frame) *light-start-position-x*))
	     (y-translation (+ (* (cos *light-theta*) y-translation-stim-frame) *light-start-position-y*))
	     time-label-y time-label-x)
	(case *light-stimulus*
	  ((:moving-bar :on-moving-bar :off-moving-bar :reversing-bar)
	   (let ((x1 (x-anatomy (/ *bar-length* 2) y-translation-stim-frame *light-start-position-x* *light-theta*))
		 (y1 (y-anatomy (/ *bar-length* 2) y-translation-stim-frame *light-start-position-y* *light-theta*))
		 (x2 (x-anatomy (/ *bar-length* -2) y-translation-stim-frame *light-start-position-x* *light-theta*))
		 (y2 (y-anatomy (/ *bar-length* -2) y-translation-stim-frame *light-start-position-y* *light-theta*)))
	     (if only-stimulus-sweep
	       (setq min-x (if min-x (min min-x x1 x2) (min x1 x2))
		     min-y (if min-y (min min-y y1 y2) (min y1 y2))
		     max-x (if max-x (max max-x x1 x2) (max x1 x2))
		     max-y (if max-y (max max-y y1 y2) (max y1 y2)))
	       (add-line x1 y1 x2 y2 agg
			 :stipple-percent *stimulus-graphic-shading-percent*
			 :thickness *bar-width* :color `blue))
	     (setq time-label-y (min y1 y2)
		   time-label-x (min x1 x2))))
	  ((:moving-spot :on-moving-spot :off-moving-spot)
	   (if only-stimulus-sweep
	     (setq min-x (if min-x
			   (min min-x (- x-translation (/ *spot-outside-diameter* 2.0)))
			   (- x-translation (/ *spot-outside-diameter* 2.0)))
		   min-y (if min-y
			   (min min-y (- y-translation (/ *spot-outside-diameter* 2.0)))
			   (- y-translation (/ *spot-outside-diameter* 2.0)))
		   max-x (if max-x
			   (max max-x (+ x-translation (/ *spot-outside-diameter* 2.0)))
			   (+ x-translation (/ *spot-outside-diameter* 2.0)))
		   max-y (if max-y
			   (max max-y (+ y-translation (/ *spot-outside-diameter* 2.0)))
			   (+ y-translation (/ *spot-outside-diameter* 2.0))))
	     (add-circle x-translation y-translation
			 (/ *spot-outside-diameter* 2.0) agg
			 :halftone-percent *stimulus-graphic-shading-percent* :color 'blue
			 :drawing-function :and))
	   (setq time-label-y y-translation
		 time-label-x x-translation)))
	(when (and (not only-stimulus-sweep) *label-stimulus-times*)
	  (add-string (format nil "~a ms" (round snapshot-time))
		      time-label-x time-label-y ; x-translation y-translation
		      agg :size :medium
		      :background (g-value agg :window :node-label-background-p)
		      :y-pixel-offset -5))))
    (unless only-stimulus-sweep (draw-motion-arrow agg))
    (list min-x min-y max-x max-y)))


(defun draw-motion-arrow (agg)
 (draw-stimulus-time "Motion" agg t))

;;; DRAW-grating
(defun draw-grating (agg)
  (declare (ignore agg)))

;; LABEL-CELL-NODE
(defun label-cell-node (node agg &key (x-offset 0) (y-offset 0) (extra-label nil) explicit-node-xy-pixel-location font)
  (let* ((node (element-node node))
	 (node-location (where node))
	 (win (g-value agg :window))
	 (view-plane-x (get-win-view-plane-x node-location win))
	 (view-plane-y (get-win-view-plane-y node-location win)))
    (add-string (if extra-label (format nil "~a~a"  (node-name node) extra-label)
		    (format nil "~a" (node-name node)))
		view-plane-x view-plane-y agg
		:x-pixel (when explicit-node-xy-pixel-location (car explicit-node-xy-pixel-location))
		:y-pixel (when explicit-node-xy-pixel-location (cadr explicit-node-xy-pixel-location))
		:x-pixel-offset (round x-offset)
		:y-pixel-offset	(round y-offset)
		:background (g-value win :node-label-background-p)
		:color (g-value win :node-label-color)
		:font font)))

(defun electrode-endpoint (win segment)
  (x-y-histology-win-from-view (node-absolute-location (segment-node-1 segment)) win))

(defun electrode-sourcepoint (win segment)
  (list (+ 45 (car (electrode-endpoint win segment)))
	(+ -45 (cadr (electrode-endpoint win segment)))))


(defun draw-electrode (win segment agg &key (color opal:purple) (shading 100))
  (opal:add-component
   agg
   (create-instance nil opal:line	; gg::arrow-line
		    (:constant nil) (:open-p t)
		    ;; (:filling-style opal:red-fill)
		    (:line-style (create-instance nil opal:line-style (:constant t) (:line-thickness 2)
						  (:foreground-color (get-opal-color color shading))))
		    (:x1 (car (electrode-sourcepoint win segment)))

		    (:x2 (car (electrode-endpoint win segment)))
		    (:y2 (cadr (electrode-endpoint win segment)))
		    (:y1 (cadr (electrode-sourcepoint win segment))))
   
   :where :front)
  (opal:add-component
   agg
   (create-instance nil opal:circle
		    (:constant nil) (:open-p t)
		    (:line-style (create-instance nil opal:line-style (:constant t) (:line-thickness 2)
						  (:foreground-color (get-opal-color color shading))))
		    (:left (- (car (electrode-endpoint win segment)) 8))
		    (:top (- (cadr (electrode-endpoint win segment)) 8))
		    (:height 16) (:width 16))
   
   :where :front))


(defun draw-electrodes (win &optional (draw-electrodes t) (type 'electrodes))
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (unless (g-value win :where-electrodes-go) (s-value win :where-electrodes-go :front))
  (let ((electrode-agg (clear-and-add-plot-agg win type :add draw-electrodes)))
    (when electrode-agg
      (loop for electrode in (electrodes)
	    when (member (element-cell electrode) (element (g-value win :cells) 'cell))
	    do (draw-electrode win electrode electrode-agg)))))

(defun draw-extracellular-electrodes (win &optional (draw-electrodes t) (type 'extracellular-electrodes))
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (let ((electrode-agg (clear-and-add-plot-agg win type :add draw-electrodes)))
    (when electrode-agg
      (loop for electrode in (extracellular-electrodes)
	    do (draw-extracellular-electrode win electrode electrode-agg)))))

(defun draw-extracellular-electrode (win electrode agg &key (color opal:green) (shading 150))
  (let ((xy (x-y-histology-win-from-view (element-absolute-location electrode) win)))
    (opal:add-component
     agg
     (create-instance nil opal:circle
		      (:constant nil) (:open-p t)
		      (:line-style (create-instance nil opal:line-style (:constant t) (:line-thickness 2)
						    (:foreground-color (get-opal-color color shading))))
		      (:left (car xy))
		      (:top (cadr xy))
		      (:height 12) (:width 12))
   
     :where :front)))


;;; DRAW-SOURCES
(defun draw-sources (win draw-them)
  (let ((agg (clear-and-add-plot-agg win `sources :add draw-them :where :front)))
    (when agg
      (loop for source in (nconc (hash-table-list (ISOURCE-HASH-TABLE)) (hash-table-list (VSOURCE-HASH-TABLE)))
	    when (member (element-cell source) (element (g-value win :cells) 'cell))
	    do
	    (let* ((node-location (element-absolute-location source))
		   (x-y
		    (if (electrode-p (element-cell-element source))
			(electrode-sourcepoint win (element-cell-element source))
			(x-y-histology-win-from-view node-location (g-value agg :window))))
		   ;; Offset a bit for soma source	      
		   (y-offset (if (equalp (element-relative-location source) (list 0.0 0.0 0.0))
				 (* -0.5 (soma-diameter (cell-soma (element-cell source)))) 0.0)))
	      (when (member :label (g-value win :source-graphics))
		;; (g-value win :label-sources)
		(label-cell-node (element-physical-node source) agg :x-offset 17 :y-offset (+ 30 0
					; y-offset
											      )
				 :extra-label (if (eq (named-structure-symbol source) 'isource) "-Isrc"   "-Vsrc")))
	      (opal:add-component
	       agg
	       (create-instance nil gg::arrow-line
				(:constant nil) (:open-p t)
				;; (:filling-style opal:red-fill)
				(:line-style (create-instance nil opal:line-style (:constant t) (:line-thickness 2)
							      (:foreground-color opal:red)))
				(:x1 (+ 15 (car x-y))) (:x2 (car x-y))
				(:y2 (+ (cadr x-y) 5)) (:y1 (+ (cadr x-y) 30)))
	       :where :front)))
      (opal:move-component (g-value win :aggregate) agg :where :front)))) 
		     

(defun draw-stimulus-time (label agg &optional motion-arrow)
  (destroy-stimulus-line agg)
  (when (or (eq *light-stimulus* :apparent-motion)
	    (> *light-stimulus-start-time* 0) (< *light-stimulus-stop-time* *user-stop-time*))
    (opal:add-component
     agg
     (create-instance nil opal:text (:visible t) (:type 'stimulus-label)
		      (:top *histology-window-stimulus-time-distance-from-top*)
		      (:string (format nil "~Dms" (round *user-start-time*)))
		      (:left (o-formula (round (- *histology-window-stimulus-time-distance-from-left*
						  (* 0.5 (gvl :width))))))
		      (:font (opal:get-standard-font :serif :bold-italic :medium)))
     :where :front)
    (opal:add-component
     agg
     (create-instance nil opal:text
		      (:visible t) (:type 'stimulus-label)
		      (:top *histology-window-stimulus-time-distance-from-top*)
		      (:string (format nil "~Dms" (round *user-stop-time*)))
		      (:left (o-formula (round (- (+ *histology-window-stimulus-time-distance-from-left*
						     *histology-window-stimulus-time-length*)
						  (* 0.5 (gvl :width))))))
		      (:font (opal:get-standard-font :serif :bold-italic :medium)))
     :where :front)
    (loop for start in (if (eq *light-stimulus* :apparent-motion)
			 (list *bar-a-start-time* *bar-b-start-time*)
			 (list *light-stimulus-start-time*))
	  for stop in (if (eq *light-stimulus* :apparent-motion)
			(list *bar-a-stop-time* *bar-b-stop-time*)
			(list *light-stimulus-stop-time*))
	  for label in '("A" "B")
	  do
	  (let ((x1 (round (+ *histology-window-stimulus-time-distance-from-left*
			      (* (max 0.0 (/ start *user-stop-time*)) *histology-window-stimulus-time-length*))))
		(x2 (+  *histology-window-stimulus-time-distance-from-left*
			(round (* (min 1.0 (/ stop *user-stop-time*)) *histology-window-stimulus-time-length*)))))
	    (when (= x1 x2) (setq x2 (1+ x1)))
	    (when (eq *light-stimulus* :apparent-motion)
	      (opal:add-component
	       agg
	       (create-instance nil opal:text (:visible t) (:type 'stimulus-label)
				(:top *histology-window-stimulus-time-distance-from-top*)
				(:string label)
				(:left (o-formula (round (- (+ x1 (* 0.5 (- x2 x1))) (* 0.5 (gvl :width))))))
				(:font (opal:get-standard-font :serif :bold-italic :large)))
	       :where :front))	    
	    (opal:add-component
	     agg
	     (create-instance nil opal:line
			      (:visible t) (:type 'stimulus-label)
			      (:x1 x1) (:x2 x2)
			      (:y1 (- *histology-window-stimulus-time-distance-from-top* 3))
			      (:y2 (- *histology-window-stimulus-time-distance-from-top* 3))
			      (:draw-function :copy)
			      (:line-style
			       (create-instance
				nil opal:line-style
				(:line-thickness 6)
				(:filling-style (create-instance nil opal:filling-style (:foreground-color opal:black)))))
			      (:line-thickness 6)) :where :front))))
  ;; Time line
  (unless (and (>= *light-stimulus-stop-time* *user-stop-time*) (= *light-stimulus-start-time* 0)
	       (not (eq *light-stimulus* :apparent-motion)))
    (opal:add-component agg (create-instance nil opal:line
					     (:visible t) (:type 'stimulus-label)
					     (:x1 *histology-window-stimulus-time-distance-from-left*)
					     (:x2 (o-formula (+ (gvl :x1) *histology-window-stimulus-time-length*)))
					     (:y1 *histology-window-stimulus-time-distance-from-top*)
					     (:y2 *histology-window-stimulus-time-distance-from-top*)
					     (:draw-function :copy)) :where :front))
    
  (unless (eq *light-stimulus* :apparent-motion)
    (let ((stimulus-label
	   (opal:add-component agg (create-instance nil opal:text (:visible t) (:type 'stimulus-label)
						    (:top *histology-window-stimulus-time-distance-from-top*)
						    (:string (if (and (>= *light-stimulus-stop-time* *user-stop-time*)
								      (= *light-stimulus-start-time* 0))
							       (concatenate-strings label " - On for entire run")
							       label))
						    (:left (o-formula (round (- (+ *histology-window-stimulus-time-distance-from-left*
										   (* 0.5 *histology-window-stimulus-time-length*))
										(* 0.5 (gvl :width))))))
						    (:font (opal:get-standard-font :serif :bold-italic :large)))
			       :where :front)))
      (when motion-arrow
	(let* ((arrow-center-x (truncate (+ (g-value stimulus-label :left)
					    (* 0.5 (g-value stimulus-label :width)))))
	       (arrow-center-y (truncate (+ *histology-window-stimulus-time-distance-from-top*
					    17 (g-value stimulus-label :height)))))
	  (opal:add-component agg (create-instance nil gg::arrow-line
						   (:type 'stimulus-label) (:open-p t) (:line-style opal:line-2)
						   (:x1 (round (+ arrow-center-x (* -15 (sin (- (- *light-theta*) (if *light-direction* 0 pi)))))))
						   (:x2 (round (+ arrow-center-x (* 15 (sin (- (- *light-theta*) (if *light-direction* 0 pi)))))))
						   (:y1 (round (+ arrow-center-y (* -1 -15 (cos (- *light-theta* (if *light-direction* 0 pi)))))))
						   (:y2 (round (+ arrow-center-y (* -1 15 (cos (- *light-theta* (if *light-direction* 0 pi))))))))
			      :where :front))))))

(defun destroy-stimulus-line (agg)
  (loop for comp in (g-value agg :components)
	when (case (g-value comp :type)
	       (stimulus-label t))
	do
	(opal:remove-component agg comp)
	(opal:destroy comp)))
	
(defun draw-segment (win segment agg &key color (shading 100))
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (if (electrode-p segment)
      (draw-electrode win segment agg :color color :shading shading)
      (let* ((cell (segment-cell segment))
	     (start-location
	      (if (segment-dummy-proximal-node-location segment)
		  (list (+ (the sf (first (cell-origin cell)))
			   (the sf (first (segment-dummy-proximal-node-location segment))))

			(+ (the sf (second (cell-origin cell)))
			   (the sf (second (segment-dummy-proximal-node-location segment))))

			(+ (the sf (third (cell-origin cell)))
			   (the sf (third (segment-dummy-proximal-node-location segment)))))
		  (node-absolute-location (segment-node-1 segment))))
	     (end-location (element-absolute-location segment)))
	(add-line (get-win-view-plane-x start-location win)
		  (get-win-view-plane-y start-location win)
		  (get-win-view-plane-x end-location win)
		  (get-win-view-plane-y end-location win)
		  agg
		  :thickness (segment-diameter segment)
		  :color color
		  :stipple-percent shading
		  :where :front)))) 




#|
(defun draw-plotted-segments (cell agg)
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (dolist (segment (cell-segments cell))
    (let ((synp (and *label-synapses
		 (get-node-elements-of-type segment 'synapse))))
    (if (or *label-nodes 
	    (member (node-name (segment-node-2 segment)) *plot-path-nodes* :test #'equal)
	    (member (node-name (segment-node-2 segment)) *plot-nodes* :test #'equal)
	    (member (node-name (segment-node-2 segment)) *analysis-nodes* :test #'equal)
	    synp)
	(let ((location (element-absolute-location segment)))
	  (add-circle
	   (get-win-view-plane-x location win)
	   (get-win-view-plane-y location win)
	   (* (g-value agg :parent :window :scale) *plot-node-pixel-radius)
	   agg :filled t :where :back
	   :color (if synp 'red))
	  (if (or (and synp *include-synapse-name-with-label)
	       *label-nodes 
	       (member (node-name (segment-node-2 segment)) *plot-path-nodes* :test #'equal)
	       (member (node-name (segment-node-2 segment)) *plot-nodes* :test #'equal)
	       (member (node-name (segment-node-2 segment)) *analysis-nodes* :test #'equal))
	       
	      (label-cell-node (segment-node-2 segment) agg :x-offset -10 :y-offset -10 )))))))
|#

;;; CIRCLE-TO-POLYLINE-LIST This assumes that the circle is perpendicular to the x axis.
(defun circle-to-polyline-list (center-x center-y center-z diameter &optional (number-vertices 16))
  (let ((radius (* 0.5 diameter)))
    (loop for angle from 0 by (/ (* 2 pi-single) number-vertices)
	  for i from 0 to number-vertices
	  collect (list (float center-x) (+ (* radius (sin angle)) center-y) (+ (* radius (cos angle)) center-z)))))

;;; DRAW-SOMAs
(defun draw-somas (win draw-somas &key soma-agg color target-soma (shading 100))
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (unless soma-agg (setq soma-agg (clear-and-add-plot-agg win `soma :add draw-somas :where (g-value win :where-somas-go))))
  (let ((virtual-somas))
    (when soma-agg
      (draw-virtual-somas
       win color
       (do ((cells (element (g-value win :cells) 'cell) (cdr cells)))
	   ((null cells) virtual-somas)
	 (let* ((cell (car cells))
		(virtual-soma
		 (let ((soma (cell-soma cell)))
		   (when (or (eq target-soma soma) (not target-soma))
		     (cond
		       ((and target-soma (soma-segments cell))
			(loop for seg in (soma-segments cell) do
			      (draw-segment win seg soma-agg :color 'blue :shading *chosen-one-shading*))
			nil)
		       ((and (g-value win :draw-anatomical-soma)
			     (or (element-parameter soma 'soma-outline)
				 (element-parameter soma 'soma-points)))
			(let* ((soma-center-correction (mapcar '- (soma-center-correction soma) (cell-origin cell)))
			       (soma-center-correction-points (concatenate 'list soma-center-correction '(0.0)))
			       (soma-outline (loop for point in ; Make copies so sort doesn't screw up original
						   (element-parameter soma 'soma-outline)
						   collect (mapcar '- point soma-center-correction)))
			       (soma-points (loop for soma-circle in (element-parameter soma 'soma-points)
						  collect
						  ;; add a zero at the end since the format of the
						  ;; SOMA-POINTS entries is (X Y Z Diameter). 
						  (mapcar '- soma-circle soma-center-correction-points))))
			  (loop for soma-circle in
				;; To make the pancakes cover the behind ones.
				(sort soma-points (if (>= (g-value win :sin-phi) 0) '< '>) :key 'car)
				do
				(add-polyline 
				 (loop for polyline-3d-point in
				       (circle-to-polyline-list (nth 0 soma-circle) (nth 1 soma-circle)
								(nth 2 soma-circle) (nth 3 soma-circle))
				       collect (get-win-view-plane-x polyline-3d-point win)
				       collect (get-win-view-plane-y polyline-3d-point win))
				 soma-agg :filled t :halftone-percent 20
				 :element soma
				 :line-style
				 (pick-thickness (round (the sf (/ 0.1 (the sf (g-value win :scale))))))
				 :drawing-function :or :color color))
			  (when soma-outline
			    (add-polyline 
			     (loop for soma-point in soma-outline
				   collect (get-win-view-plane-x soma-point win)
				   collect (get-win-view-plane-y soma-point win))
			     soma-agg :filled t :halftone-percent 20 :where :back
			     :element soma
			     :line-style opal:line-2 :drawing-function :or :color color)))
			nil)
		       ((element-parameter soma 'cylinder-soma-segments) nil)
		       (t soma))))))
	   (when virtual-soma (push virtual-soma virtual-somas))))
       soma-agg))))

		
(defun draw-virtual-somas (win color somas &optional agg)
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (when somas
    (let ((soma-agg (or agg (clear-and-add-plot-agg win 'v-somas :add t)))
	  (fill (color-to-fill color))
	  itemlist)
      (when soma-agg
	(loop for soma in somas
	      do (let ((x-y (x-y-histology-win-from-view (node-absolute-location (soma-node soma)) win)))
		   (when (x-y-in-win x-y win)
		     (let* ((radius (the fn (round (/ (the sf (* 0.5 (soma-diameter soma)))
						      (the sf (g-value win :scale))))))
			    (soma-color (or (get-a-value 'color (soma-parameters soma))
					    (get-a-value 'color (node-parameters (soma-node soma)))))
			    (soma-shading (or (get-a-value 'shading (soma-parameters soma))
					      (get-a-value 'shading (cell-parameters (soma-cell soma)))))
			    (this-soma-fill (if (or soma-color soma-shading)
					      (color-to-fill (or soma-color 'black)
							     (or soma-shading (g-value (g-value win :segment-color-shading))))
					      fill)))
		       ;; (xcenter ycenter radius fill soma diameter left top)
		       (push (list (the fn (car x-y))
				   (the fn (cadr x-y))
				   radius
				   this-soma-fill
				   soma
				   (the fn (+ radius radius))
				   (the fn (- (the fn (car x-y)) radius))
				   (the fn (- (the fn (cadr x-y)) radius)))
			     itemlist)))))
	(when itemlist
	  (let ((v-agg (make-v-agg virtual-soma (list-to-array-generic itemlist) 'v-somas)))
	    (s-value v-agg :colorizeable t)
	    (virtual-agg-finishing v-agg soma-agg (or (g-value win :where-somas-go) :front))))))))
			       
;; To make the width/height calculation simpler - not clear that this helps
(defun virtual-agg-finishing (v-agg parent-agg &optional (where :front))
  (g-value v-agg :width) (g-value v-agg :height)
  (g-value v-agg :top) (g-value v-agg :left)
  (s-value v-agg :width (o-formula (gvl :parent :window :width)))
  (s-value v-agg :height (o-formula (gvl :parent :window :height)))
  (opal:add-component parent-agg v-agg :where where))


#|
(defun synapse-rf-points (syn height &key include-center include-shape)
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (let* ((type-parameters (synapse-type-parameters (synapse-type syn)))
	 (syn-parameters (synapse-parameters syn))
	 (spatial-rf-array (cdr-assoc 'SPATIAL-RF type-parameters))
	 (spatial-rf-function-args (cdr-assoc 'SPATIAL-RF-FUNCTION-ARGS type-parameters))
	 (spatial-rf-grid-size-x (spatial-rf-grid-size-x spatial-rf-function-args type-parameters))
	 (spatial-rf-grid-size-y (spatial-rf-grid-size-y spatial-rf-function-args type-parameters))
	 (spatial-rf-x-size (if spatial-rf-array (array-dimension spatial-rf-array 0) 1))
	 (spatial-rf-y-size (if spatial-rf-array (array-dimension spatial-rf-array 1) 1))
	 (light-input-offset-distance
	  (if (assoc 'LIGHT-OFFSET-DISTANCE type-parameters)
	      (coerce-to-single (cdr-assoc 'LIGHT-OFFSET-DISTANCE type-parameters)) 0.0))
	 (light-input-offset-angle
	  (if (assoc 'LIGHT-OFFSET-ANGLE type-parameters)
	      (coerce-to-single (cdr-assoc 'LIGHT-OFFSET-ANGLE type-parameters)) 0.0))
	 (syn-rf-center-x (syn-rf-center-x syn light-input-offset-distance light-input-offset-angle))
	 (syn-rf-center-y (syn-rf-center-y syn light-input-offset-distance light-input-offset-angle)))
    (declare (fixnum spatial-rf-x-size spatial-rf-y-size))
    (declare (single-float light-input-offset-distance light-input-offset-angle syn-rf-center-x syn-rf-center-y))
    (values
     (when include-shape
       (case (cadr (assoc 'rf-shape type-parameters))
	 (t
	  (loop for x in
		'(10.0 4.9999995 -5.0000005 -10.0 -4.999999 4.999999 10.0)
		for y in 
		'(0.0 8.6602545 8.660254 -8.742278e-7 -8.6602545 -8.6602545 1.7484556e-6)
		collect

		(case *light-stimulus-plane*
		  (:xy
		   (list (the sf (+ syn-rf-center-x (the sf x)))
			 (the sf (+ syn-rf-center-y (the sf y)))
			 height))
		  (:xz
		   (list (the sf (+ syn-rf-center-x (the sf x)))
			 height
			 (the sf (+ syn-rf-center-y (the sf y))))))))))
     (when include-center
       (case *light-stimulus-plane*
	 (:xy (list syn-rf-center-x syn-rf-center-y height))
	 (:xz (list syn-rf-center-x height syn-rf-center-y)))))))

|#

(defun synapse-rf-points (syn height &key include-center include-shape)
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (syn-rf-geometry-values
   (syn)
   (values
    (when include-shape
      (loop for base-x in (first (cdr-assoc 'rf-shape type-parameters))
	    for base-y in (second (cdr-assoc 'rf-shape type-parameters))
	    collect (let ((nominal-x (+ syn-rf-center-x (the sf base-x)))
			  (nominal-y (+ syn-rf-center-y (the sf base-y)))
			  (nomimal-z height))
		      (case *light-stimulus-plane*
			(:xy (list nominal-x nominal-y nomimal-z))
			(:xz (list nominal-x height nominal-y)))))
      (case *light-stimulus-plane*
	(:xy (list syn-rf-center-x syn-rf-center-y height))
	(:xz (list syn-rf-center-x height syn-rf-center-y)))))))


(defun draw-synapse-rfs (win draw-them &key (halftone-percent *syn-rf-shape-shading*)
			     (thickness *syn-rf-connection-thickness*)
			     (connection-shading *syn-rf-connection-shading*)
			     (dash *syn-rf-connection-dash*))
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (let ((synapse-rf-agg (clear-and-add-plot-agg win `synapse-rf-agg :add draw-them :where :front))
	(halftone-percent (or halftone-percent 0))
	(shape-array-index -1)
	(connections-array-index -1)
;	(shape-item-array (make-array 0 :adjustable t))
	(shape-item-list '())
;	(connections-item-array (make-array 0 :adjustable t))
	(connections-item-list '())
	(rf-shape-line-style faint-line)
	shape-virtual-agg connections-virtual-agg)
    (declare (fixnum shape-array-index connections-array-index))
    (when synapse-rf-agg
      (loop for type-info in (g-value win :synapse-type-graphics-parameters)
	    when (eq 'light (synapse-type-control (cdr-assoc 'type type-info)))
	    do
	    (let* ((type (cdr-assoc 'type type-info))
		   (color (element-parameter type 'color))
		   (fill (when (> halftone-percent 0) (color-to-fill color halftone-percent)))
		   (height (coerce-to-single (cdr-assoc 'height type-info)))
		   (line-style (ACCESS-*LINE-STYLES-ARRAY* thickness color connection-shading dash))
		   ;; (rf-shape-line-style (colored-line-style color 0))
		   ;; (rf-shape (cdr-assoc 'rf-shape type-info))
		   (draw-rf-shape (cdr-assoc 'draw-rf-shape type-info))
		   (draw-rf-connections (cdr-assoc 'draw-rf-connections type-info)))
	      (when (or draw-rf-shape draw-rf-connections)
		(loop for syn being the hash-value of (SYNAPSE-HASH-TABLE) when (eq type (synapse-type syn))
		      when (member (element-cell syn) (element (g-value win :cells) 'cell)) do 
		      (multiple-value-bind (shape-points connection-points)
			  (synapse-rf-points syn height :include-center draw-rf-connections :include-shape draw-rf-shape)
			(when draw-rf-shape
			  (push
			   (list
				 (loop for point in shape-points
				       nconc (x-y-cons-histology-win (list (get-win-view-plane-x point win)
									   (get-win-view-plane-y point win))
								     win))
				 rf-shape-line-style
				 fill)
			   shape-item-list)
;                          (incf shape-array-index)
;                          (setf (aref (adjust-array shape-item-array (1+ shape-array-index)) shape-array-index)
;                                (list
;                                 (loop for point in shape-points
;                                       nconc (x-y-cons-histology-win (list (get-win-view-plane-x point win)
;                                                                           (get-win-view-plane-y point win))
;                                                                     win))
;                                 rf-shape-line-style
;                                 fill))


			  )
			(when draw-rf-connections
			  (push
			   (nconc
				 (x-y-cons-histology-win (list (get-win-view-plane-x (element-absolute-location syn) win)
							       (get-win-view-plane-y (element-absolute-location syn) win))
							 win)
				 (x-y-cons-histology-win (list (get-win-view-plane-x connection-points win)
							       (get-win-view-plane-y connection-points win))
							 win)
				 (list line-style))
			   connections-item-list)
;                          (incf connections-array-index)
;                          (setf (aref (adjust-array connections-item-array (1+ connections-array-index))
;                                      connections-array-index)
;                                (nconc
;                                 (x-y-cons-histology-win (list (get-win-view-plane-x (element-absolute-location syn) win)
;                                                               (get-win-view-plane-y (element-absolute-location syn) win))
;                                                         win)
;                                 (x-y-cons-histology-win (list (get-win-view-plane-x connection-points win)
;                                                               (get-win-view-plane-y connection-points win))
;                                                         win)
;                                 (list line-style)))
			  ))))
	      (when (or connections-item-list shape-item-list
;			(> shape-array-index -1)(> connections-array-index -1)
			)
;		(s-value win :visible t) ; Need this here to make virtual agg work right.
;		(opal:update win)
		)

	      (when shape-item-list ;(> shape-array-index -1)
		(setq shape-virtual-agg (create-instance nil opal:virtual-aggregate
							 (:item-prototype (create-instance nil virtual-polyline))
							 (:item-array (list-to-array-generic shape-item-list))
							 ; (:item-array shape-item-array)
							 (:point-in-item nil)))
		(virtual-agg-finishing shape-virtual-agg synapse-rf-agg))
	      (when connections-item-list ;(> connections-array-index -1)
		(setq connections-virtual-agg
		      (create-instance nil opal:virtual-aggregate
				       (:item-prototype (create-instance nil virtual-segment))
				       (:item-array (list-to-array-generic connections-item-list))
				       ; (:item-array connections-item-array)
				       (:point-in-item nil)))
		(virtual-agg-finishing connections-virtual-agg synapse-rf-agg :back))
	      (s-value win :visible t)
	      (resurrect-opal-win win))))))


(defun colorize-voltages (target-time)
  (loop for comp in (g-value (get-plot-agg *standard-graphics-output* `histology) :components)
	when (car (g-value comp :cell-elements))
	do
	(let ((voltage (get-element-value (car (g-value comp :cell-elements)) target-time)))
	  (when voltage
	    (let ((color (get-opal-variable-color voltage)))
	      (case (car (g-value comp :is-a))
		(opal:circle
		 (s-value comp :filling-style (create-instance nil opal:filling-style (:FOREGROUND-COLOR color))))
		(opal:line
		 (s-value (g-value comp :line-style :filling-style) :foreground-color color)))))))
  (opal:update *standard-graphics-output*))


(defun color-cell (cell &optional (color 'blue) (win *standard-graphics-output*))
  (let ((element-cell (coerce-to-list (element-cell cell))))
    (loop for comp in (g-value (get-plot-agg win 'histology) :components)
	  when (and (loop for elt in (g-value comp :cell-elements)
			  when (or (eq cell 'all)
				   (member (node-cell (element-physical-node elt)) element-cell
					   :test 'equal))
			  do (return t))
		    (eq opal:line (car (g-value comp :is-a))))
	  do (s-value (g-value comp :line-style) :foreground-color (case color
								     (green opal:green)
								     (orange opal:orange)
								     (cyan opal:cyan)
								     (red opal:red)
								     (black opal:black)
								     (blue opal:blue))))
    (opal:update win t)))

(defun pick-thickness (thickness &optional (color :black))
  (case color
    ((black :black)
     (loop for style in *DENDRITE-LINES*
	   when (= (g-value style :line-thickness) thickness)
	   do (return style)
	   finally (return (create-instance nil opal:line-style
					    (:constant t)
					    (:line-thickness thickness)))))
    ((gray :gray :grey grey)
     (loop for style in *dendrite-gray-lines*
	   when (= (g-value style :line-thickness) thickness)
	   do (return style)
	   finally (return (create-instance nil opal:line-style
					    (:constant t)
					    (:foreground-color (get-color-from-library 0.8 0.8 0.8))
					    (:line-thickness thickness)))))

    ((red :red)
     (loop for style in *dendrite-red-lines*
	   when (= (g-value style :line-thickness) thickness)
	   do (return style)
	   finally (return (create-instance nil opal:line-style
					    (:constant t)
					    (:foreground-color (get-color-from-library 0.8 0.8 0.8))
					    (:line-thickness thickness)))))

    ((light-red :light-red)
     (loop for style in *dendrite-light-red-lines*
	   when (= (g-value style :line-thickness) thickness)
	   do (return style)
	   finally (return (create-instance nil opal:line-style
					    (:constant t)
					    (:foreground-color (get-color-from-library 0.8 0.8 0.8))
					    (:line-thickness thickness)))))
    (t
     (create-instance nil opal:line-style
		      (:constant nil)
		      (:line-thickness thickness)
		      (:foreground-color (get-opal-color color))))))


(defun get-segments-to-plot (win)
  (loop for cell in (coerce-to-list (element (g-value win :cells) 'cell))
	nconcing (if (g-value win :restrict-to-PROXIMAL-SEGMENT)
		     (loop for seg in (PROXIMAL-DENDRITE-LIST nil (g-value win :PROXIMAL-SEGMENT-LEVEL))
			   when (eq (segment-cell seg) cell) collect seg)
		     (copy-list (cell-segments cell)))))

(defun cell-segments-to-plot (win cell)
  (if (g-value win :restrict-to-PROXIMAL-SEGMENT)
      (loop for seg in (PROXIMAL-DENDRITE-LIST nil (g-value win :PROXIMAL-SEGMENT-LEVEL))
	    when (eq (segment-cell seg) cell) collect seg)
      (cell-segments cell)))


(proclaim '(notinline virtual-segment-item-spec))
(defun virtual-segment-item-spec (x1 y1 x2 y2 lstyle thickness segment)
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (let ((array (make-array 7)))
    (setf (aref array 0) x1
	  (aref array 1) y1
	  (aref array 2) x2
	  (aref array 3) y2
	  (aref array 4) lstyle
	  (aref array 5) thickness
	  (aref array 6) segment)
    array))


(defun segment-dummy-proximal-node-absolute-location (cell segment)
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (let ((cell (or cell (segment-cell segment))))
    (list (+ (the sf (first (cell-origin cell)))
	     (the sf (first (segment-dummy-proximal-node-location segment))))
	  (+ (the sf (second (cell-origin cell)))
	     (the sf (second (segment-dummy-proximal-node-location segment))))
	  (+ (the sf (third (cell-origin cell)))
	     (the sf (third (segment-dummy-proximal-node-location segment)))))))

(proclaim '(notinline virtual-segment-core))
(defun virtual-segment-core (segments cell itemlist colors win segment-agg)
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (let ((segment-color-shading (the sf (or (g-value win :segment-color-shading) 100.0)))
	(win-scale (the sf (g-value win :scale)))
	(half-win-width (schema-half-width win))
	(half-win-height (schema-half-height win)))
    (declare (single-float win-scale half-win-width half-win-height))
    (do ((segments segments (cdr segments)))
	((null segments) (values itemlist colors))
      (let ((segment (car segments)))
	(if (get-a-value 'electrode (segment-parameters segment)) ; (electrode-p segment)
	    (draw-electrode win segment segment-agg)
	    (multiple-value-bind (x1 y1)
		(x-y-histology-win-from-view-values-with-dims
                 (the cons (if (segment-dummy-proximal-node-location segment)
			       (segment-dummy-proximal-node-absolute-location cell segment)
			       (node-absolute-location (segment-node-1 segment))))
                 half-win-width half-win-height win)
	      (declare (fixnum x1 y1))
	      (multiple-value-bind (x2 y2)
		  (x-y-histology-win-from-view-values-with-dims
                   (node-absolute-location (segment-node-2 segment)) half-win-width half-win-height win)
		(declare (fixnum x2 y2))
		(when (or (x-y-in-win-values x1 y1 win) (x-y-in-win-values x2 y2 win))
		  (let ((thickness-pixels (the fn (round (the sf (/ (the sf (segment-diameter segment)) win-scale)))))
			(segment-color-shading
			 (or (get-a-value 'shading (segment-parameters segment))
			     (get-a-value 'shading (cell-parameters (segment-cell segment)))
			     segment-color-shading))
			(color (or (car colors)
				   (get-a-value 'color (segment-parameters segment))
				   (get-a-value 'color (node-parameters (segment-node-2 segment)))
				   (g-value win :segment-default-color)
				   (g-value win :default-graphics-color))))
		    ;; (format t "color ~A~%" color)
                    (push (virtual-segment-item-spec
                           (the fn x1) (the fn y1) (the fn x2) (the fn y2)
			   (access-*line-styles-array*-for-segments-fast thickness-pixels color segment-color-shading nil)
                           thickness-pixels segment)
                          itemlist))))		       
	      (when colors (setq colors (cdr colors)))))))))


(defun histology-window-finishing (win)
  (update-labeled-elements)
  (move-top-agg-components win 'marked-segments)
  (move-top-agg-components win 'node-labels)
  (move-top-agg-components win 'histology-scale-bar))


(defun move-top-agg-components (win type)
  (let ((top-agg (get-agg win)))
    (when top-agg
      (let ((components (copy-list (g-value top-agg :components))))
	(loop for comp in components
	      when (eq (g-value comp :type) type)
	      do (opal::move-component top-agg comp :where :front))))))

(defun parse-marked-segments-and-colors (win)
  (loop for marked-segments-and-color in (g-value win :marked-segments-and-colors)
	nconc (copy-list (car marked-segments-and-color)) into colored-segments
	nconc (copy-list (loop for seg in (car marked-segments-and-color)
			       collect (cadr marked-segments-and-color)))
	into colors1 finally (return (list colored-segments colors1))))

(defun draw-segments (win &optional (draw-segments t) (type 'segments))
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (let ((segment-agg (clear-and-add-plot-agg win type :add draw-segments)))
    (when segment-agg
      (s-value segment-agg :type type)
      (let* (colors
	     itemlist
	     (cells (when (eq type 'segments) (element (g-value win :cells) 'cell)))
	     (marked-segments (case type
				(marked-segments
				 (let ((marked-segments-and-colors (parse-marked-segments-and-colors win)))
				   (setq colors (cadr marked-segments-and-colors))
				   (car marked-segments-and-colors))))))
	
	(loop for cell in cells do
	      (multiple-value-setq (itemlist colors)
		(virtual-segment-core (cell-segments-to-plot win cell) cell itemlist colors win segment-agg)))
	(when marked-segments
	  ;; (format t "parsing marked-segments ~A~%" marked-segments)
	  (multiple-value-setq (itemlist colors)
	    (virtual-segment-core marked-segments nil itemlist colors win segment-agg)))
	(when itemlist
	  (let ((v-agg (make-v-agg virtual-segment (list-to-array-generic itemlist) 'v-segments)))
	    (s-value v-agg :colorizeable t)
	    (virtual-agg-finishing v-agg segment-agg)))))))


(defun make-v-agg (prototype array contains)
  (create-instance nil opal:virtual-aggregate
		   (:item-prototype prototype)
		   (:item-array array)
		   (:point-in-item t)
		   (:contains contains)))

(defun collect-things-colors (win things default-color)
  (loop for thing in things collect
	(typecase thing
	  (axon (if (and (g-value win :axon-color-from-synapse) (synapse-color (axon-target-synapse thing)))
		    (synapse-color (axon-target-synapse thing))
		    default-color))
	  (synapse (if (and (g-value win :synapse-cxn-color-from-synapse) (synapse-color thing))
		       (synapse-color thing)
		       default-color)))))
  

(defun get-things-w-connections-item-list (things line-styles win)
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (let (item-list)
    (loop for thing in things
	  for line-style in line-styles
	  do (let* ((start-element (typecase thing
				     (synapse (synapse-pre-synaptic-element thing))
				     (axon thing)))
		    (start (element-absolute-location start-element))
		    (end-element (typecase thing
				   (synapse thing)
				   (axon (axon-target-synapse thing))))
		    (end (element-absolute-location end-element))
		    (mid-points (and (g-value win :use-connection-midpoints)
				     (element-parameter thing 'mid-points))))
	       (when (and start end)
		 (if mid-points
		     (do ((point-list (concatenate 'list (list start) mid-points (list end)) (cdr point-list)))
			 ((null (cdr point-list)))
		       (let* ((start (car point-list))
			      (end (cadr point-list))
			      (x-y-1 (x-y-histology-win-from-view start win))
			      (x-y-2 (x-y-histology-win-from-view end win)))
			 (when (or (x-y-in-win x-y-1 win) (x-y-in-win x-y-2 win))
			   (push (virtual-segment-item-spec
				  (the fn (car x-y-1)) (the fn (cadr x-y-1))
				  (the fn (car x-y-2)) (the fn (cadr x-y-2))
				  line-style 1 nil)
				 item-list))))
		     (let ((x-y-1 (x-y-histology-win-from-view start win))
			   (x-y-2 (x-y-histology-win-from-view end win)))
		       (when (or (x-y-in-win x-y-1 win) (x-y-in-win x-y-2 win))
			 (push (virtual-segment-item-spec
				(the fn (car x-y-1)) (the fn (cadr x-y-1))
				(the fn (car x-y-2)) (the fn (cadr x-y-2))
				line-style 1 nil)
			       item-list)))))))
    item-list))


(defun collect-drawable-things-w-connections (win type)
  (loop for thing in (list-of-all-things type)
	when (typecase thing
	       (axon (and (axon-target-synapse thing)
			  (or (not (g-value win :restrict-axons-to-cells))
			      (member (element-cell thing) (element (g-value win :cells) 'cell)))
			  (or (not (g-value win :restrict-axons-to-synapse-types))
			      (member (synapse-type (axon-target-synapse thing))
				      (g-value win :restrict-axons-to-synapse-types)))))
	       (synapse (and (eq 'voltage (synapse-type-control (synapse-type thing)))
			     (not (axon-p (synapse-pre-synaptic-element thing)))
			     (or (not (g-value win :restrict-synapse-cxns-to-cells))
				 (member (element-cell thing) (element (g-value win :cells) 'cell)))
			     (or (not (g-value win :restrict-synapse-cxns-to-synapse-types))
				 (member (synapse-type (axon-target-synapse thing))
					 (g-value win :restrict-synapse-cxns-to-synapse-types))))))
	collect thing))

(defun collect-locations-of-drawable-things (things)
  (loop for thing in things
	nconcing (list (element-absolute-location thing)) into locations
	when (and (axon-p thing) (axon-target-synapse thing))
	nconcing (list (element-absolute-location (axon-target-synapse thing))) into locations
	when (element-parameter thing 'mid-points)
	nconcing (element-parameter thing 'mid-points) into locations
	finally (return locations)))
	  

(defun draw-axons (win &optional (draw-axons t) (type 'axons))
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (unless (g-value win :where-axons-go) (s-value win :where-axons-go :back))
  (let ((axon-agg (clear-and-add-plot-agg win type :add draw-axons))
	(axon-color (or (g-value win :axon-color) (s-value win :axon-color 'red)))
	(thickness (round (/ *axon-graphics-diameter* (g-value win :scale)))))
    (when axon-agg
      (let* ((axons (case type
		      (axons (collect-drawable-things-w-connections win 'axon))
		      (marked-axons (get-win-branch-elements-of-type win 'axon))))
	     (line-styles (loop for color in 
				(case type
				  (marked-axons (get-win-branch-element-colors-of-type win 'axon))
				  (t (collect-things-colors win axons axon-color)))			 
				collect (access-*line-styles-array* thickness color (g-value win :axon-color-shading))))
	     item-list)
	(when axons
	  (let* ((item-list (get-things-w-connections-item-list axons line-styles win))
		 (v-agg (create-instance nil opal:virtual-aggregate
					 (:item-prototype virtual-segment) (:item-array (list-to-array-generic item-list))
					 (:point-in-item nil) (:contains 'v-axons))))
	    (virtual-agg-finishing v-agg axon-agg (g-value win :where-axons-go))))))))


(defun draw-synapse-connections (win &optional (draw-synapse-cxns t) (type 'synapse-cxns))
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (unless (g-value win :where-synapse-cxns-go) (s-value win :where-synapse-cxns-go :back))
  (let ((synapse-cxn-agg (clear-and-add-plot-agg win type :add draw-synapse-cxns))
	(synapse-cxn-color (or (g-value win :synapse-cxn-color) (s-value win :synapse-cxn-color 'blue)))
	(thickness (round (/ *synapse-cxn-graphics-diameter* (g-value win :scale)))))
    (when synapse-cxn-agg
      (let* ((synapse-cxns (case type
			     (synapse-cxns (collect-drawable-things-w-connections win 'synapse))
			     (marked-synapse-cxns (get-win-branch-elements-of-type win 'synapse-cxn))))
	     (line-styles (loop for color in 
				(case type
				  (marked-synapse-cxns (get-win-branch-element-colors-of-type win 'synapse-cxn))
				  (t (collect-things-colors win synapse-cxns synapse-cxn-color)))
				collect (access-*line-styles-array* thickness color (g-value win :synapse-cxn-color-shading)))))
	  
	(when synapse-cxns
	  (let* ((item-list (get-things-w-connections-item-list synapse-cxns line-styles win))
		 (v-agg (create-instance nil opal:virtual-aggregate
					 (:item-prototype virtual-segment) (:item-array (list-to-array-generic item-list))
					 (:point-in-item nil) (:contains 'v-synapse-cxns))))
	    (virtual-agg-finishing v-agg synapse-cxn-agg (g-value win :where-synapse-cxns-go))))))))


(defun axon-coordinates (win)
  (loop for axon in (collect-drawable-things-w-connections win 'axon)
	nconc (loop for location in
		    (concatenate 'list
				 (list (element-absolute-location axon)
				       (element-absolute-location (axon-target-synapse axon)))
				 (element-parameter axon 'mid-points))
		    collect (list (get-win-view-plane-x location win) (get-win-view-plane-y location win)))))



#|
(defun resize-histology-window (win &optional x-shift y-shift width height)
  (let ((dummy3 x-shift)
	(dummy4 y-shift)	
	(dummy6 width)
	(dummy7 height)
	(dummy8 (g-value win :scale))
	vpx vpy)
    (case (g-value win :adjust-histology-window)
      (:automatic
       (loop for node being the hash-value of (NODE-HASH-TABLE) ; Find x-y extent of cells.
	     when (and (node-IS-PHYSICAL-CELL-NODE node)
		       (not (equal node *ground-node*))
		       (member (node-cell node) (element (g-value win :cells) 'cell)))
	     do
	     (setq vpx (get-win-view-plane-x (node-absolute-location node) win)
		   vpy (get-win-view-plane-y (node-absolute-location node) win))
	     and maximizing vpx into max-x-all
	     and maximizing vpy into max-y-all
	     and minimizing vpx into min-x-all
	     and minimizing vpy into min-y-all
	     finally
	     (when max-x-all
	       (setf dummy3 (* 0.5 (+ max-x-all min-x-all))
		     dummy4 (* 0.5 (+ max-y-all min-y-all))
		     ;; The factor of 1.2 to make the window a bit bigger than the cell(s).
		     dummy6 (* 1.3 (max *minimum-cell-histo-x-span (- max-x-all min-x-all)))
		     dummy7 (+ (* *HISTOLOGY-WINDOW-MIN-HEIGHT-EXTRA dummy8) ; a little extra height
			       (* 1.3 (max *minimum-cell-histo-y-span (- max-y-all min-y-all))))))))
      (:menu
       (choose-variable-values
	'((dummy3 "Center of window along X direction [um]:" :number)
	  (dummy4 "Center of window along Y direction [um]:" :number)
	  (dummy6 "Histology window width [um]" :float)
	  (dummy7 "Histology window height [um]" :float))
	':label "Histology XY (center moves when cell is redrawn)")))
    (reset-histology-xfrm win dummy3 dummy4  dummy6 dummy7 dummy8)))

|#

